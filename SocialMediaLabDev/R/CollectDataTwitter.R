CollectDataTwitter <-
function(searchTerm, numTweets, verbose, writeToFile, language) {

# cat(paste("DEBUG - numTweets is set to:", numTweets)) # DEBUG

  # handle the arguments

  if (missing(verbose)) {
    verbose <- TRUE # default to verbose
  }

  if (missing(language)) {
    language <- NULL # default to NULL (as per 'twitteR' package default)
  }

  if (missing(writeToFile)) {
    writeToFile <- FALSE # default = not write to file
  }

  if (verbose=="TRUE" | verbose=="true" | verbose=="T" | verbose==TRUE) {
    verbose <- TRUE
  }
  else {verbose <- FALSE}

  if (missing(numTweets)) {
    numTweets <- 1500 # default to 1500 max tweets
  }

  # Ensure that argument `pageName` has been specified by user.

  if (missing(searchTerm)) {
    cat("Error. Argument `searchTerm` is missing.\nPlease specify a search term or hashtag to collect data from.\n")
    break
  }

  # Start data collection
if (verbose) {
  cat(paste("Now retrieving data based on search term: ",searchTerm,"\n",sep=""))
  flush.console()
}
  # Collecting tweets based on hashtag / keyword

  tweetsData <- searchTwitter(searchTerm, n=numTweets, lang=language) #1500 is max

  # Convert this data into a dataframe object, for ease of use
  if (verbose) {
    cat("Done\n")  ### DEBUG
    flush.console()
    cat("Cleaning and sorting the data...\n")
  }
  df <- twListToDF(tweetsData) # a better way

  # rename metadata
    names.twitteR <- c("screenName", "created") # change from
    names.api <- c("screen_name", "created_at") # change to
    for(name in names.twitteR) {
      names(df)[which(names(df)==name)] <- names.api[which(names.twitteR==name)]
    }
    df$from_user <- df$screen_name

  # removing odd characters
  df <- RemoveOddChars(df)

  # extract user info and add to df
  df <- ExtractUserInfo(df)

  # extract HASHTAG info and add to df
  df <- ExtractHashtagInfo(df)
  if (verbose) {
    cat("Done\n")  ### DEBUG
    flush.console()
  }
  ################################################

  if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
    tweetsDataDF <- twListToDF(tweetsData) # we just want the original tweets data
    currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
    currTime <- gsub(":","_",currTime)
    write.csv(tweetsDataDF,paste0(currTime,"_",searchTerm,"_TwitterData.csv"))
    cat("Twitter data was written to current working directory, with filename:\n")
    cat(paste0(currTime,"_",searchTerm,"_TwitterData.csv"))
  }

  class(df) <- append(class(df),c("dataSource","twitter"))

  cat("\n")

  return(df)

  ################################################

}
