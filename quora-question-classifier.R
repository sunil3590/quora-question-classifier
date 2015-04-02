rm(list=ls())

# import libraries
library(jsonlite)

# get username from user profile URL
getUserNameFromProfile = function(url) {
  return(substring(url, 22))
}

# get question name from question URL
getQuesNameFromQues = function(url) {
  return(substring(url, 22))
}

# get question name form answer URL - TODO
getQuesNameFromAns = function(url) {
  
}

# 4 - preprocessor for every question - TODO
# reduce to 2 class problem
preprocess = function(ques, topics) {
  
}

# 3 - get tags for a question
getQuesTopics = function(ques) {
  url = paste0("http://127.0.0.1:5000/questions", ques)
  raw.data = readLines(url, warn = FALSE)
  rd = jsonlite::fromJSON(raw.data)
  return(rd$topics)
}

# 2 - get activity(list of user follows and questions) of a user
getActivity = function(user) {
  # get the html for user profile and build DOM for the same
  url = paste0("http://quora.com", user)
  html = readLines(url, warn = FALSE)
  dom = xmlParse(url, isHTML=TRUE)
  
  # we are interested only in user names and questions
  users = xpathSApply(dom, "//div[@class='object_follow_story']/
                      a[@class='user']", xmlGetAttr, "href")
  
  ques = xpathSApply(dom, "//a[@class='question_link']", 
                     xmlGetAttr, "href")
  
  return(list(ques, users))
}

# 1 - main function
main = function(startUser) {
  # list of users seen as of now
  userList = c(startUser)
  
  while(TRUE) {
    # get random user on list and remove him/her
    curIdx = floor(runif(1, min=1, max=length(userList)))
    curUser = userList[curIdx]
    userList = userList[-curIdx]
    
    # debug print
    # print(length(userList))
    # print(curUser)
    
    # get activity for last user on list
    activity = getActivity(curUser)
    
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
      print(q)
      
      # get topics for question
      topics = getQuesTopics(q)
      print(topics)
      
      # preprocess topics
      
      # train + test dynamically
    }
  }
}

# start crawling from a user
main("/Sunil-DS")