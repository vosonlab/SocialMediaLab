CreateSemanticNetwork <-
function(x,writeToFile,termFreq,hashtagFreq,removeTermsOrHashtags,stopwordsEnglish)
  {
    if (missing(writeToFile)) {
      writeToFile <- FALSE # default = not write to file
    }
    if (missing(termFreq)) {
      termFreq <- 5 # default to the top 5% most frequent terms. reduces size of graph.
    }
    if (missing(hashtagFreq)) {
      hashtagFreq <- 50 # default to the top 50% hashtags. reduces size of graph. hashtags are 50% because they are much less frequent than terms.
    }
    if (missing(removeTermsOrHashtags)) {
      removeTermsOrHashtags <- NA
    }
    if (missing(stopwordsEnglish)) {
      stopwordsEnglish <- TRUE # default to true, because most English users will probably want this
    }
      UseMethod("CreateSemanticNetwork",x)
   }
