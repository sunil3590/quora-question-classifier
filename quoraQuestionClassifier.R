rm(list=ls())

# import libraries
library(jsonlite)
library(XML)
library(RCurl)
library(RMOA)

# include other source file
source("streamPreProcessorAndClassifier.R")

# global question counter
qCount = 0

# list of topics related to computer science
tech_topic_list = c("Computer Science",
                    "Programming Languages",
                    "Computer Programming",
                    "Software Engineering",
                    "Computer Networking",
                    "Machine Learning",
                    "Web Development",
                    "Algorithms",
                    "Data Structure",
                    "Operating Systems",
                    "Software and Applications",
                    "Artificial Intelligence",
                    "Computer Engineering",
                    "Learning to Program",
                    "Data Science",
                    "Information Technology",
                    "Programming",
                    "Web Development",
                    "Ruby",
                    "Java (programming language)",
                    "Science",
                    "Physics",
                    "Astrophysics",
                    "Startups",
                    "Mathematics",
                    "Facebook (product)",
                    "Technology",
                    "Startup Advice and Strategy",
                    "Startup Founders and Entrepreneurs",
                    "Silicon Valley",
                    "Internships",
                    "Google",
                    "Facebook (company)",
                    "Statistics (academic discipline)",
                    "Probability (statistics)"
)

# check if URL exists
# http://memosisland.blogspot.com/2012/03/check-url-existance-with-r.html
urlExists = function(address) {  
  tryCatch ({
    con = url(address)  
    a = capture.output(suppressWarnings(readLines(con)))
    close(con)
    TRUE;
  },
  error = function(err) {
    occur = grep("cannot open the connection", capture.output(err));  
    if(length(occur) > 0) {
      close(con)
      FALSE;
    }
  }
  )
}

# get username from user profile URL
getUserNameFromProfile = function(url) {
  # ex : http://www.quora.com/Angshuman-Kalita
  return(substring(url, 22))
}

# get question name from question URL
getQuesNameFromQues = function(url) {
  # ex : http://www.quora.com/What-is-the-biggest-irony-in-India-1
  return(substring(url, 22))
}

# 4 - preprocessor for every question
# reduce to 2 class problem
multiClassToTwoClass = function(topics) {
  # check if there is a match in the topics
  x = length(intersect(tolower(tech_topic_list), tolower(topics)))
  
  # decide tech or nontech based on intersection
  if(x == 0) {
    return("NonTech")
  }
  else {
    return("Tech") 
  }
}

# 3 - get tags for a question
getQuesTopics = function(ques) {
  # form URL and check if it exists
  url = paste0("http://127.0.0.1:5000/questions", ques)
  if(!urlExists(url)) {
    cat("\n\nNot exist : ", url, "\n\n\n")
    return(NULL)
  }
  
  # read the response and format output
  raw.data = readLines(url, warn = FALSE)
  rd = jsonlite::fromJSON(raw.data)
  return(rd$topics)
}

# 2 - get activity(list of user follows and questions) of a user
getActivity = function(user) {
  # form URL and check if it exists
  url = paste0("http://www.quora.com", user)
  
  # disable check, it is not working
  if(!urlExists(url)) {
    cat("\n\nNot exist : ", url, "\n\n\n")
    return(NULL)
  }
  
  # get the html for user profile and build DOM for the same
  html = readLines(url, warn = FALSE)
  dom = xmlParse(url, isHTML=TRUE)
  
  # we are interested only in user names and questions
  users = xpathSApply(dom, "//div[@class='object_follow_story']/
                      a[@class='user']", xmlGetAttr, "href")
  
  ques = xpathSApply(dom, "//a[@class='question_link']", 
                     xmlGetAttr, "href")
  
  # return only content of interest
  return(list(ques, users))
}

# 1 - main function
main = function() {
  # list of users seen as of now
  userList = c("/Sunil-DS", "/Aditya-Bhise", "/Abhishek-Ravi-1")
  
  # store questions and classes
  QList = c()
  classList = c()
  
  # threshold denotes number of questions for 
  # initial model learning
  threshold = 15
  
  # metrics
  TPCount = 0
  TNCount = 0
  FPCount = 0
  FNCount = 0
  qcount = 0
  
  cat("Crawling to get ", threshold, " questions to train on.\n")
  
  # keep calm and crawl, forever
  while(TRUE) {
    # get random user on list and remove him/her
    curIdx = floor(runif(1, min=1, max=length(userList)))
    curUser = userList[curIdx]
    userList = userList[-curIdx]
    
    # get activity for curUser
    activity = getActivity(curUser)
    
    # if no activity found, move on to next user
    if(is.null(activity)) {
      next
    }
    
    # keep a max of 100 users at a time
    numUser = length(userList)
    if(numUser < 100) {
      needed = 100 - numUser
      has = length(activity[[2]])
      if(has < needed) {
        canTake = has 
      } else {
        canTake = needed
      }
      
      # update user list with newly found users
      if(canTake > 0) {
        userList = c(userList, activity[[2]][1:canTake]) 
      }
    }
    
    # process all question in the activity
    ques = activity[[1]]
    for(q in ques) {   
      # get topics for question
      topics = getQuesTopics(q)
      
      # if question not found, move on to next
      if(is.null(topics)) {
        next
      }
      
      # preprocess topics
      tag = multiClassToTwoClass(topics)
      
      #add question to list if threshold not met
      if(qcount < threshold) {
        QList = c(QList,q) 
        classList = c(classList, tag)
        
        cat("Train : ", q, " : ", tag, "\n")
      }
      else if(qcount == threshold) {
        #build initial model
        cat("\nThreshold reached. Beginning to train.\n\n")
        
        # create Term Document Matrix out of questions
        qTDM = getTDMFromQList(QList) 
        
        #transpose it to have words as features and counts as values
        qDataFrame = as.data.frame(t(as.matrix(qTDM)))
        
        #add the class column
        qDataFrame["category"] = classList
        
        #remove row names
        row.names(qDataFrame) = NULL
        
        #do feature selection using sparsity
        qDataFrame = factorise(qDataFrame)
        
        #convert data frame to data stream
        qDataStream = datastream_dataframe(qDataFrame)
        
        #feed it to RMOA, see how to use kernlab here
        hdTree <- HoeffdingTree(numericEstimator = 
                                  "GaussianNumericAttributeClassObserver")    
        hdModel <- trainMOA(model = hdTree, 
                            formula = category ~ ., 
                            data = qDataStream)
        
        # Predict using the HoeffdingTree on the training data
        predictedClass <- predict(hdModel, newdata=qDataFrame, type="response")
        
        # print confusion matrix for training data
        cat("\nTraining complete\nConfusion matrix : \n")
        print(table((predictedClass), as.character(qDataFrame[,"category"])))
        cat("Starting to predict and dynamically update the model\n\n")
      }
      else {
        # predict and learn
        newQ = c(q)
        newClass = c(tag)
        
        #create TDM out of the question
        newTDM = getTDMFromQList(newQ) 
        
        #transpose it to have words as features and counts as values
        newDataFrame = as.data.frame(t(as.matrix(newTDM)))
        
        #add the class column
        newDataFrame["category"] = newClass
        
        #find intersection with the feature set of previous model
        #initiate by zero and override present values
        testDataFrame = qDataFrame[1,]
        
        #assign corresponding value to common features
        commonfeat = intersect(names(newDataFrame),names(testDataFrame))
        
        # set the freq of intersecting terms and reset other terms to 0
        for(cname in names(testDataFrame)) {
          if(cname %in% commonfeat) {
            testDataFrame[1,cname] = newDataFrame[1,cname]
          }
          else {
            testDataFrame[1,cname] = 0
          }
        }
        
        #predict the class for this question
        predClass <- predict(hdModel, newdata=testDataFrame, type="response")
        
        print("------------------------------------")
        cat("\nQuestion        : ", q, "\n")
        cat("Actual class    : ", tag, "\n")
        cat("Predicted Class : ", predClass, "\n\n") 
        print("------------------------------------")
        
        
        if(predClass == "Tech" && tag == "Tech") {
          TPCount = TPCount + 1
        }
        else if (predClass == "Tech" && tag == "NonTech") {
          FPCount = FPCount + 1 
        }
        else if (predClass == "NonTech" && tag == "NonTech") {
          TNCount = TNCount + 1
        }
        else if (predClass == "NonTech" && tag == "Tech") {
          FNCount = FNCount + 1
        }
        
        #
        if(qcount %% 5 == 0) {
          print("***********************************************")
          print("Metrics")
          print(paste("TP", TPCount, sep = " : "))
          print(paste("FP", FPCount, sep = " : "))
          print(paste("TN", TNCount, sep = " : "))
          print(paste("FN", FNCount, sep = " : "))
          
          accuracy = (TPCount + FNCount)/(TPCount + FNCount + FPCount + FNCount)
          precision = (TPCount)/(TPCount+FPCount)
          recall = TPCount / (TPCount + FNCount)
          sensitivity = recall
          specificity = TNCount/ (TNCount+FPCount)
          f1_measure = 2* precision* recall/ (precision + recall)
          print(paste("Accuracy", accuracy, sep= " : "))
          print(paste("Precision", precision, sep= " : "))
          print(paste("Recall", recall, sep= " : "))
          print(paste("Sensitivity", sensitivity, sep= " : "))
          print(paste("Specificity", specificity, sep= " : "))
          print(paste("F1 - Measure", f1_measure, sep= " : "))
          print("***********************************************")
        }
        
        #do feature selection using sparsity
        qDataFrame = factorise(qDataFrame)
        
        #convert data frame to data stream
        testDataStream = datastream_dataframe(testDataFrame)
        
        #having reset as false, update model
        hdModel <- trainMOA(model = hdModel$model, 
                            formula = category ~ .,
                            data = testDataStream,
                            reset= FALSE, trace=TRUE, 
                            chunksize=1)     
      }
      
      # update question counter
      qcount = qcount +1;   
    }
  }
}

# start
main()