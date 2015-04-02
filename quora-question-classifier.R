rm(list=ls())

# import libraries
library(jsonlite)
library(XML)
library(RCurl)

# global question counter
qCount = 0

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

# get question name from answer URL - TODO
getQuesNameFromAns = function(url) {
  
}

# 4 - preprocessor for every question - TODO
# reduce to 2 class problem
preProcess = function(ques, topics) {
  qCount <<- qCount + 1
  # TODO : dheeraj
  # for now randomly choose the class (1 or 2)
  return(floor(runif(1, min=1, max=3)))
}

# 3 - get tags for a question
getQuesTopics = function(ques) {
  # form URL and check if it exists
  url = paste0("http://127.0.0.1:5000/questions", ques)
  if(!urlExists(url)) {
    cat("\n\nNot exist : ", url, "\n\n\n")
    return(NULL)
  }
  
  # debug print
  # print(url)
  
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

  # debug print
  # print(url)
  
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
main = function(startUser) {
  # list of users seen as of now
  userList = c(startUser)
  
  # keep calm and crawl, forever
  while(TRUE) {
    # get random user on list and remove him/her
    curIdx = floor(runif(1, min=1, max=length(userList)))
    curUser = userList[curIdx]
    userList = userList[-curIdx]
    
    # debug print
    # print(length(userList))
    # print(curUser)
    
    # get activity for curUser
    activity = getActivity(curUser)
    
    # if no activity found, move on to next user
    if(is.null(activity)) {
      next
    }
    
    # debug print
    # print(activity)
  
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
      
      # debug print
      # print(userList)
    }
  
    # process all question in the activity
    ques = activity[[1]]
    for(q in ques) {
      # debug print
      # print(q)
      
      # get topics for question
      topics = getQuesTopics(q)
      
      # if question not found, move on to next
      if(is.null(topics)) {
        next
      }
      
      # debug print
      # print(topics)
      
      # preprocess topics
      class = preProcess(q, topics)
      if(class == 1) {
        cat(q, " : Tech\n")
      } else if(class == 2) {
        cat(q, " : Non-tech\n")
      } else {
        print("Unknown class!")
      }
      
      # train + test dynamically
    }
  }
}

# start crawling from a user
main("/Sunil-DS")