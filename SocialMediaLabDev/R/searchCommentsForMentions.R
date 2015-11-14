searchCommentsForMentions <-
function (commentsTextCleaned,usernamesCleaned) {

  ptm <- proc.time() # Start the timer # DEBUG

  matchTemp <- lapply(commentsTextCleaned, function(x) {

      tempResult <- lapply(usernamesCleaned, function(y) {

        foo <- grep(paste("(\\+|\\@)", y, sep=""),x)

          if(length(foo)>0){
            return(y)
          }
          else {
            return("FALSE")
          }

        }
      )
    }
  )

  matchTemp <- unlist(matchTemp)

  # have to split `matchTemp` into as many groups as there are rows (i.e. comment texts)
  matchTemp2 <- split(matchTemp, ceiling(seq_along(matchTemp)/length(commentsTextCleaned)))

  # Now we want to retrieve the username with MAX CHARACTERS that was mentioned,
  # or if all values were "FALSE" then just return a single "FALSE" value.
  # THE REASON IS:
  # If we have the following comment text: "+Timothy some text",
  # and there are two users in the data, namely "Tim" and "Timothy",
  # the `grep` will have matched both of these in the comment text.
  # So, we want to ensure it takes the username with more characters (i.e. "Timothy"),
  # rather than the subset match (i.e. "Tim").

  matchTemp3 <- lapply(matchTemp2, function(x) {

    # if all elements == "FALSE" then just return "FALSE"
    if (length(x[which(x=="FALSE")])==length(x)) {
      return("FALSE")
    }

    # if all elements except one == "FALSE" then return the 'non false' element
    # e.g. c("FALSE", "FALSE", "Timothy", "FALSE") ---> returns "Timothy"
    if (length(x[which(x!="FALSE")])==1){
      return(x[which(x!="FALSE")])
    }

    #
    else {
      tempResult <- x[which(x!="FALSE")]
      tempResult <- x[which(nchar(x)==max(nchar(x)))][1] # if two duplicate results (e.g. "Timothy" and "Timothy"), then just return the 1st
      return(tempResult)
      # return(max(nchar(x))) #DEBUG
    }
    })

  matchTemp3 # what is this?

  # debugResultDF <- data.frame(commentsTextCleaned,usernamesCleaned,unlist(matchTemp3)) #DEBUG
  finalMatchesTemp <- as.vector(unlist(matchTemp3))

  # convert back (or 'de-regex') the username characters
  finalMatches <- gsub("\\\\","",finalMatchesTemp)

  #functionRunTime <- proc.time() - ptm                # DEBUG
  #print("Runtime of FindMentions function was:")      # DEBUG
  #flush.console()                                     # DEBUG
  #print(functionRunTime)                              # DEBUG
  #flush.console()                                     # DEBUG

  return (finalMatches)

}
