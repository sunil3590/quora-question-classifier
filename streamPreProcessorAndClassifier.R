library(tm)
library(SnowballC)

# build term document matrix for the list of questions
getTDMFromQList = function(subset) {
  
  # replace hyphen by space
  subset = gsub("-", " ", subset)
  
  # create corpus of questions
  questionCorpus = Corpus(VectorSource(subset), 
                          readerControl = list(language = "en"))

  # Change all letters to lower case
  questionCorpus = tm_map(questionCorpus,content_transformer(tolower))
  
  # Remove punctuation and numbers
  questionCorpus = tm_map(questionCorpus, removePunctuation)
  questionCorpus = tm_map(questionCorpus, removeNumbers)
  
  # Remove common stop words
  questionCorpus = tm_map(questionCorpus, removeWords, stopwords('english')) 
  
  # Stemming
  questionCorpus = tm_map(questionCorpus, stemDocument, language = "english")

  # get a corpus
  questionCorpus = tm_map(questionCorpus, PlainTextDocument)
  
  # construct TDM, leave out small words
  questionTDM = TermDocumentMatrix(questionCorpus, 
                                   control = list(minWordLength = 3))
  
  # remove terms that are sparse
  questionTDM = removeSparseTerms(questionTDM, 0.98)
  
  return(questionTDM)
}