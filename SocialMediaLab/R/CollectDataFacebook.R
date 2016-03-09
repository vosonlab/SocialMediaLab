#' Collect data from Facebook pages for generating different types of networks
#' 
#' This function collects data from Facebook pages (i.e. post data and
#' comments/likes data within posts), and structures the data into a data frame
#' of class \code{dataSource.facebook}, ready for creating networks for further
#' analysis.
#' 
#' \code{CollectDataFacebook} collects public 'post' data from a given Facebook
#' page, including comments and 'likes' from within each post.
#' 
#' The function then finds and maps the relationships between users and posts,
#' and structures these relationships into a format suitable for creating
#' bimodal networks using \code{CreateBimodalNetwork}.
#' 
#' A date range must be specified for collecting post data using
#' \code{rangeFrom} and \code{rangeTo} (i.e. data will be collected from posts
#' posted within the date range). If no date range is supplied, then the
#' default is the current system date minus one week (i.e. 7 days leading up to
#' current system date).
#' 
#' @param pageName character string, specifying the name of the Facebook page.
#' For example, if page is: https://www.facebook.com/StarWars, then
#' \code{pageName="StarWars".}
#' @param rangeFrom character string, specifying a 'start date' for data
#' collection, in the format YYYY-MM-DD. For example, to collect data starting
#' from July 4th 2015, \code{rangeFrom} would be "2015-07-04". Default value is
#' current system date minus one week (i.e. the date 7 days ago).
#' @param rangeTo character string, specifying an 'end date' for data
#' collection, in the format YYYY-MM-DD. For example, to collect data until
#' December 25th 2015, \code{rangeFrom} would be "2015-12-25". Default value is
#' the current system date.
#' @param verbose logical. If \code{TRUE} then this function will output
#' runtime information to the console as it computes. Useful diagnostic tool
#' for long computations. Default is \code{FALSE}.
#' @param n numeric, maximum number of comments and likes to return (see
#' \code{getPost} in Rfacebook package). Default value is 1000.
#' @param writeToFile logical. If \code{TRUE} then the data is saved to file in
#' current working directory (CSV format), with filename denoting
#' \code{rangeFrom}, \code{rangeTo}, and \code{pageName}.
#' @param dynamic logical. If \code{TRUE} then temporal data will be collected,
#' which can be used to create dynamic networks (using function
#' \code{CreateDynamicNetwork}). Note: Facebook API does not currently provide
#' timestamp data for 'likes' on posts. Therefore, if dynamic is set to
#' \code{TRUE}, then only 'comments' data are collected. In this way, an edge
#' from user i to post j represents whether (and how many times) user i has
#' commented on post j.
#' @param credential Optional, a \code{credential} object created by \code{Authenticate}.
#' @return A data frame object of class \code{dataSource.facebook} that can be
#' used with \code{CreateBimodalNetwork}.
#' @note Currently supported network types:
#' 
#' - bimodal networks; \code{CreateBimodalNetwork} - dynamic bimodal networks;
#' \code{CreateDynamicNetwork}
#' 
#' Note: dynamic networks created using Facebook data are bimodal. This means
#' that there are two types of vertices present in the network (i.e. Facebook
#' users and Facebook posts), with edges representing the time(s) when user i
#' commented on post j. Currently, timestamp data is not available through the
#' Facebook API for 'likes' data (i.e. when user i 'likes' post j), so
#' relationships based on 'likes' are excluded from dynamic Facebook data (and
#' therefore networks generated using \code{CreateDynamicNetwork}).
#' @author Timothy Graham <timothy.graham3@@uq.net.au> & Robert Ackland
#' <robert.ackland@@anu.edu.au>
#' @seealso \code{AuthenticateWithFacebookAPI} must be run first or no data
#' will be collected, \code{collectTemporalDataFacebook} is used to collect
#' temporal data suitable for dynamic networks (i.e. Facebook network that
#' changes as a function of time).
#' @keywords facebook data mining SNA
#' @examples
#' 
#' \dontrun{
#'   ## Use your own values for myAppID and myAppSecret
#'   myAppID <- "123456789098765"
#'   myAppSecret <- "abc123abc123abc123abc123abc123ab"
#' 
#'   # Authenticate with the Facebook API using `AuthenticateWithFacebookAPI`
#'   fb_oauth <- AuthenticateWithFacebookAPI(appID=myAppID, appSecret=myAppSecret,
#'     extended_permissions=FALSE, useCachedToken=TRUE)
#' 
#'   # Run the `CollectDataFacebook` function and store the results in variable `myFacebookData`
#'   myFacebookData <- CollectDataFacebook(pageName="StarWars", rangeFrom="2015-05-01",
#'   rangeTo="2015-06-03",writeToFile=FALSE,verbose=TRUE)
#' 
#'   # Create a 'bimodal' network using \code{CreateBimodalNetwork}
#'   g_bimodal_facebook <- CreateBimodalNetwork(myFacebookData)
#' 
#'   # View descriptive information about the bimodal network
#'   g_bimodal_facebook
#' }
#' @export
CollectDataFacebook <-
function(pageName,rangeFrom,rangeTo,verbose,n,writeToFile,dynamic, credential = NULL) {
#with(list(pageName,rangeFrom,rangeTo,verbose,n,writeToFile), {

    postID=from=relationship=edgeWeight=to=NULL # to please the gods of R CMD CHECK

  # handle the arguments
  # if the user has specified dynamic==TRUE
  # then we will have to run an entirely different set of code (this may be fixed in future versions)

    ## use side effect
    if (!(exists("fb_oauth")) & is.null(credential)) {
        fb_oauth <- NULL # to get rid of the note when doing R CMD CHECK
    }
    ## not use side effect
    if (!is.null(credential)) {
        fb_oauth <- credential$auth
    }

  if (missing(dynamic)) {
    dynamic <- FALSE # default to non-dynamic data
  }

## <<<< In this block of code we collect a non-dynamic data set (because the user specified dynamic=FALSE)
if (dynamic==FALSE | dynamic =="FALSE" | dynamic=="false" | dynamic=="F") {

      if (missing(verbose)) {
        verbose <- TRUE # default to verbose
      }

      if (missing(writeToFile)) {
        writeToFile <- FALSE # default = not write to file
      }

      if (verbose=="TRUE" | verbose=="true" | verbose=="T" | verbose==TRUE) {
        verbose <- TRUE
      }
      else {verbose <- FALSE}

      if (missing(n)) {
        n <- 1000 # default to 1000 max comments/like for each post (within the page)
      }

      numCommentsLikes <- n

      rm(n) # debug - could cause problems with other functions, maybe

      # Ensure that argument `pageName` has been specified by user.

      if (missing(pageName)) {
        cat("Error. Argument `pageName` is missing.\nPlease specify a Facebook page to collect data from.\n")
        break
      }

      # If date range has not been specified, or one of the range values is missing,
      # then default to a range of one week (from current system date).

      if (missing(rangeFrom) | missing(rangeTo)) {

        rangeFrom <- as.Date(Sys.Date()) - 7
        rangeTo <- as.Date(Sys.Date())
        cat(paste("No date range was specified for data collection.\nDefaulting to one week, from: ",rangeFrom," to ",rangeTo,"\n",sep=""))
      }

      # Start data collection

      cat(paste("Now retrieving data from page: ",pageName,"\n",sep=""))
      flush.console()

      dates <- seq(from=as.Date(rangeFrom), to=as.Date(rangeTo), by="day") # search by day
      n <- length(dates)-1

      df <- list()

      for (i in 1:n){

        if (!verbose) {
          quiet(
            try(df[[i]] <- getPage(pageName, token=fb_oauth, since=dates[i], until=dates[i+1]),silent=TRUE)
          )
        }

        if (verbose) {
          cat(as.character(dates[i]), " ") # if user wants feedback (verbose TRUE)
        }

        if (verbose) {
          try(df[[i]] <- getPage(pageName, token=fb_oauth, since=dates[i], until=dates[i+1]),silent=TRUE)
        }

      }

      pageData <- do.call(rbind, df)

      # Generate error message and return from function if there is no post data found for the data range specified.
      # cat(paste("\nDEBUG length of pageData: ",length(pageData)))
      if (length(pageData)==0) {
        cat("Error. No posts found for this date range! Please try a larger range.")
        return()
      }

      cat("\n")
      cat("Now collecting data from POSTS (within the page).\n")  ### DEBUG
      flush.console()

      if (verbose) {
        cat(paste(" Collecting posts data from ",nrow(pageData)," posts.\n",sep=""))
        cat(paste("Collecting maximum of",numCommentsLikes,"comments and likes for each post.\n"))
      }

      tempDataList <- lapply(pageData[,7], function(x) { # apply function over IDs of posts in `pageData`

        if (verbose) {
            cat(".")
        }

        # OLD WAY (sometimes Facebook API causes errors, see: http://stackoverflow.com/questions/8353119/facebook-api-oauthexception-an-unexpected-error-has-occurred-please-retry-you)
        # postData <- getPost(post=x, n=numCommentsLikes, token=fb_oauth)

        # NEW WAY, error handling

            tryCatch({
                  postData <- getPost(post=x, n=numCommentsLikes, token=fb_oauth)
              },
              error=function(cond) {
                cat(paste("\nFacebook API caused an unknown error while retrieving post ID, ", x,"\n"))
                cat(paste("Condition: ",cond,"\n"))
                # Just return a unique string for debugging in case of error
                return("dodgydata123")
              }
            )


      })

        # clean ODD CHARACTERS from tempDataList
        for (i in 1:length(tempDataList)){

    # DEBUG
    # cat(paste0("\nI am cleaning odd characters on tempDataList[",i,"] out of ",length(tempDataList)))

          tempDataList[i][[1]]$post$from_name <- iconv(tempDataList[i][[1]]$post$from_name, to = 'UTF-8')
          tempDataList[i][[1]]$post$message <- iconv(tempDataList[i][[1]]$post$message, to = 'UTF-8')
          tempDataList[i][[1]]$like$from_name <- iconv(tempDataList[i][[1]]$like$from_name, to = 'UTF-8')
          tempDataList[i][[1]]$comments$from_name <- iconv(tempDataList[i][[1]]$comments$from_name, to = 'UTF-8')
          tempDataList[i][[1]]$comments$message <- iconv(tempDataList[i][[1]]$comments$message, to = 'UTF-8')
        }

          # anatomy of the tempDataList:
          # tempDataList[i][[1]]$post         # 10 variables
          # tempDataList[i][[1]]$likes        # 2 variables (from_name, from_id)
          # tempDataList[i][[1]]$comments     # 6 variables

          # now we have a list with all the post data (in the set of pageData$id)
          # pull out the data we want, and put into DATA TABLE (not dataframe... too slow)

          # 'dummy' first row of dataframe, for DEBUG purposes (fix later.....)

          tempDataTable <- data.table(
            postID = "foo",
            postName = "bar",
            postMessage = "something",
            postType = "something",
            postLink = "something",
            postTimestamp = "something",

            likeFromID = "fizz",
            likeFromName = "buzz",

            commentFromID = "foo",
            commentFromName = "bar",
            commentText = "blah")

            # keyCols = c("postID","postName")
            # setkey(tempDataTable,postID) # set the key value of the data table

          # pre-declare vars for speed

          likeFromID_TEMP       <- "foo"
          likeFromName_TEMP     <- "foo"
          commentFromID_TEMP    <- "foo"
          commentFromName_TEMP  <- "foo"
          commentText_TEMP      <- "foo"

          for (i in 1:length(tempDataList)) { # for each post (top-level list element) in the pageData

    # DEBUG
    # cat(paste0("\nI am sorting through tempDataList[",i,"] out of ",length(tempDataList)))

            # for each of the likes
            for (j in 1:length(tempDataList[i][[1]]$likes$from_id)) {

    # DEBUG
    # cat(paste0("\nI am sorting through 'likes' data on tempDataList[",j,"] out of ",length(tempDataList[i][[1]]$likes$from_id)))

                likeFromID_TEMP    <- tempDataList[i][[1]]$likes$from_id[j]
                likeFromName_TEMP  <- tempDataList[i][[1]]$likes$from_name[j]

                if(length(likeFromID_TEMP) < 1) {
                  likeFromID_TEMP <- "NA"
                }

                if(length(likeFromName_TEMP) < 1) {
                  likeFromName_TEMP <- "NA"
                }

                tempDataTableLOOP <- data.table(
                  postID          = tempDataList[i][[1]]$post$id,
                  postName        = tempDataList[i][[1]]$post$from_name,
                  postMessage     = tempDataList[i][[1]]$post$message,
                  postType        = tempDataList[i][[1]]$post$type,
                  postLink        = tempDataList[i][[1]]$post$link,
                  postTimestamp   = tempDataList[i][[1]]$post$created_time,
                  likeFromID      = likeFromID_TEMP,
                  likeFromName    = likeFromName_TEMP,
                  commentFromID   = "NA",
                  commentFromName = "NA",
                  commentText     = "NA")

                tempDataTable <- rbind(tempDataTable,tempDataTableLOOP)

            }

            # for each of the comments
            for (j in 1:length(tempDataList[i][[1]]$comments$from_id)) {

    # DEBUG
    # cat(paste0("\nI am sorting through 'comments' data on tempDataList[",j,"] out of ",length(tempDataList[i][[1]]$comments$from_id)))

                commentFromID_TEMP    <- tempDataList[i][[1]]$comments$from_id[j]
                commentFromName_TEMP  <- tempDataList[i][[1]]$comments$from_name[j]
                commentText_TEMP      <- tempDataList[i][[1]]$comments$message[j]

                if(length(commentFromID_TEMP) < 1) {
                  commentFromID_TEMP <- "NA"
                }
                if(length(commentFromName_TEMP) < 1) {
                  commentFromName_TEMP <- "NA"
                }
                if(length(commentText_TEMP) < 1) {
                  commentText_TEMP <- "NA"
                }

                tempDataTableLOOP <- data.table(
                  postID          = tempDataList[i][[1]]$post$id,
                  postName        = tempDataList[i][[1]]$post$from_name,
                  postMessage     = tempDataList[i][[1]]$post$message,
                  postType        = tempDataList[i][[1]]$post$type,
                  postLink        = tempDataList[i][[1]]$post$link,
                  postTimestamp   = tempDataList[i][[1]]$post$created_time,
                  likeFromID      = "NA",
                  likeFromName    = "NA",
                  commentFromID   = commentFromID_TEMP,
                  commentFromName = commentFromName_TEMP,
                  commentText     = commentText_TEMP)

                tempDataTable <- rbind(tempDataTable,tempDataTableLOOP)

                # keyCols = c("postID","postName")
                setkey(tempDataTable,postID) # set the key value of the data table

            }

            # remove dummy first row
            # tempDataTable <- tempDataTable[-1,] # OLD WAY
            tempDataTable[postID != "foo"] # NEW DATA TABLE WAY
          }

          ##
          ##
          ## TO DO - delete NA values in tempDataTable, before proceeding with data munging?
          ##
          ##

    # cat("\n ########## Creating subsets of data for processing...") # DEBUG

          # create subsets of data, for processing

          usersLikedPosts <- data.table(
            from          = tempDataTable$likeFromName,
            to            = tempDataTable$postID,
            relationship  = "User-Post Based on Likes",
            edgeWeight    = 1,
            postType      = tempDataTable$postType,
            postLink      = tempDataTable$postLink,
            postTimestamp = tempDataTable$postTimestamp,
            commentText   = tempDataTable$commentText
            )

          usersCommentedPosts <- data.table(
            from          = tempDataTable$commentFromName,
            to            = tempDataTable$postID,
            relationship  = "User-Post Based on Comments",
            edgeWeight    = 1,
            postType      = tempDataTable$postType,
            postLink      = tempDataTable$postLink,
            postTimestamp = tempDataTable$postTimestamp,
            commentText   = tempDataTable$commentText
            )

          dataCombined <- rbind(usersLikedPosts,usersCommentedPosts)

          setkey(dataCombined,from) # set the key value of the data table

    # cat("\n ########## Removing 'fake' NA values...") # DEBUG

          # remove the "fake" NA values (these will occur due to the way the original data.table is structured, see above)
            # toRemove <- which(dataCombined$from=="NA")
            # dataCombined <- dataCombined[-toRemove,]
          # do it the data.table way...
          dataCombined <- dataCombined[!"NA"]

    # cat("\n ########## removing actual NA values...") # DEBUG

          # and then remove the "actual" NA values
              #toRemove <- which(is.na(dataCombined$from))
              #if (length(toRemove) != 0) {
              #  dataCombined <- dataCombined[-toRemove,]
              #}
          # do it the data.table way...
          dataCombined <- na.omit(dataCombined)

    # cat("\n ########## Get unique pairs...") # DEBUG

          # get unique pairs only
          dataCombinedUNIQUE <- data.table(
            from          = dataCombined$from,
            to            = dataCombined$to,
            relationship  = "xx",
            edgeWeight    = 1
            )

          dataCombinedUNIQUE <- unique(dataCombinedUNIQUE)

          setkey(dataCombinedUNIQUE,from,to) # set the key value of the data table

    # cat("\n ########## calculating matches and weights...") # DEBUG

          for (i in 1:nrow(dataCombinedUNIQUE)) {

            matchesTemp <- which(dataCombined$from == dataCombinedUNIQUE$from[i] & dataCombined$to == dataCombinedUNIQUE$to[i])
            ## data table way:
            # dataCombined[  , .I[dataCombinedUNIQUE$from[i],dataCombinedUNIQUE$to[i]]

            # !! Not sure about this.
            # Current approach means that if user i has LIKED *and* COMMENTED on post j,
            # then there will be multiple rows matched, and we will take the LAST match,
            # which results in the `relationship` defaulting to `User-Post Based on Comments`
              # OLD WAY:
              # dataCombinedUNIQUE$relationship[i] <- dataCombined$relationship[last(matchesTemp)]
            # the data.table way...
            dataCombinedUNIQUE[i, relationship := dataCombined$relationship[last(matchesTemp)]]

            # DO THE WEIGHTS THIS WAY (vectorised):
              # OLD WAY:
              # dataCombinedUNIQUE$edgeWeight[i] <- sum(dataCombined$from == dataCombinedUNIQUE$from[i] & dataCombined$to == dataCombinedUNIQUE$to[i])
            # NEW data table way:
            dataCombinedUNIQUE[i, edgeWeight := dataCombinedUNIQUE[, sum(dataCombined$from == dataCombinedUNIQUE$from[i] & dataCombined$to == dataCombinedUNIQUE$to[i]) ]]

          }

          class(dataCombinedUNIQUE) <- append(class(dataCombinedUNIQUE),c("dataSource","facebook"))

          if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
            #currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
            write.csv(dataCombined,paste0(rangeFrom,"_to_",rangeTo,"_",pageName,"_FacebookData.csv"))
            cat("Facebook data was written to current working directory, with filename:\n")
            cat(paste0(rangeFrom,"_to_",rangeTo,"_",pageName,"_FacebookData.csv"))
          }

          cat("\n")

          return(dataCombinedUNIQUE)
} ## <<<<< end of non-dynamic data collection
##############################################################################################################
## <<<<< in this block of code we begin DYNAMIC DATA collection, if dynamic is TRUE
if (dynamic=="TRUE" | verbose=="true" | verbose=="T" | verbose==TRUE) {

        if (missing(verbose)) {
          verbose <- TRUE # default to verbose
        }

        if (missing(writeToFile)) {
          writeToFile <- FALSE # default = not write to file
        }

        if (verbose=="TRUE" | verbose=="true" | verbose=="T" | verbose==TRUE) {
          verbose <- TRUE
        }

        else {verbose <- FALSE}

        if (missing(n)) {
          n <- 1000 # default to 1000 max comments/like for each post (within the page)
        }

        numCommentsLikes <- n

        rm(n) # debug - is this causing problems?

        # Ensure that argument `pageName` has been specified by user.

        if (missing(pageName)) {
          cat("Error. Argument `pageName` is missing.\nPlease specify a Facebook page to collect data from.\n")
          return()
        }

        # If date range has not been specified, or one of the range values is missing,
        # then default to a range of one week (from current system date).

        if (missing(rangeFrom) | missing(rangeTo)) {
          rangeFrom <- as.Date(Sys.Date()) - 7
          rangeTo <- as.Date(Sys.Date())
          cat(paste("No date range was specified for data collection.\nDefaulting to one week, from: ",rangeFrom," to ",rangeTo,sep=""))
        }

        # Start data collection

        cat(paste("Now retrieving data from page: ",pageName,"...\n",sep=""))
        flush.console()

        dates <- seq(from=as.Date(rangeFrom), to=as.Date(rangeTo), by="day") # search by day
        n <- length(dates)-1

        df <- list()

        for (i in 1:n){

          if (!verbose) {
            quiet(
              try(df[[i]] <- getPage(pageName, token=fb_oauth, since=dates[i], until=dates[i+1]),silent=TRUE)
            )
          }

          if (verbose) {
            cat(as.character(dates[i]), " ") # if user wants feedback (verbose TRUE)
          }

          if (verbose) {
            try(df[[i]] <- getPage(pageName, token=fb_oauth, since=dates[i], until=dates[i+1]),silent=TRUE)
          }

        }

        pageData <- do.call(rbind, df)

        # Generate error message and return from function if there is no post data found for the data range specified.
        # cat(paste("\nDEBUG length of pageData: ",length(pageData)))
        if (length(pageData)==0) {
          cat("Error. No posts found for this date range! Please try a larger range.")
          return()
        }

        cat("Now retrieving data from POSTS (within the page)...")  ### DEBUG
        flush.console()

        if (verbose) {
          cat(paste(" Collecting posts data from",nrow(pageData),"posts.\n"))
          cat(paste("Collecting maximum of",numCommentsLikes,"comments and likes for each post.\n"))
        }

        tempDataList <- lapply(pageData[,7], function(x) { # apply function over IDs of posts in `pageData`

          if (verbose) {
              cat(".")
          }

          # OLD WAY (sometimes Facebook API causes errors, see: http://stackoverflow.com/questions/8353119/facebook-api-oauthexception-an-unexpected-error-has-occurred-please-retry-you)
          # postData <- getPost(post=x, n=numCommentsLikes, token=fb_oauth)
          # NEW WAY, error handling
          quiet(
            try(postData <- getPost(post=x, n=numCommentsLikes, token=fb_oauth))
          )

        })

      # return(tempDataList)

              # clean ODD CHARACTERS from tempDataList
              for (i in 1:length(tempDataList)){
                tempDataList[i][[1]]$post$from_name <- iconv(tempDataList[i][[1]]$post$from_name, to = 'UTF-8')
                tempDataList[i][[1]]$post$message <- iconv(tempDataList[i][[1]]$post$message, to = 'UTF-8')
                tempDataList[i][[1]]$like$from_name <- iconv(tempDataList[i][[1]]$like$from_name, to = 'UTF-8')
                tempDataList[i][[1]]$comments$from_name <- iconv(tempDataList[i][[1]]$comments$from_name, to = 'UTF-8')
                tempDataList[i][[1]]$comments$message <- iconv(tempDataList[i][[1]]$comments$message, to = 'UTF-8')
              }

        # anatomy of the tempDataList:
        # tempDataList[i][[1]]$post         # 10 variables
        # tempDataList[i][[1]]$likes        # 2 variables (from_name, from_id)
        # tempDataList[i][[1]]$comments     # 6 variables

        # now we have a list with all the post data (in the set of pageData$id)
        # pull out the data we want, and put into DATA TABLE (not dataframe... too slow)

        # 'dummy' first row of dataframe, for DEBUG purposes (*to fix*)

        tempDataTable <- data.table(
          postID = "foo",
          postName = "bar",
          postMessage = "something",
          postType = "something",
          postLink = "something",
          postTimestamp = "something",
          commentFromID = "foo",
          commentFromName = "bar",
          commentText = "blah",
          commentTimestamp = "bar")

        # pre-declare vars
        commentFromID_TEMP    <- "foo"
        commentFromName_TEMP  <- "foo"
        commentText_TEMP      <- "foo"
        commentTimestamp_TEMP <- "foo"

        for (i in 1:length(tempDataList)) { # for each post (top-level list element) in the pageData

          # for each of the comments
          for (j in 1:length(tempDataList[i][[1]]$comments$from_id)) {

              commentFromID_TEMP    <- tempDataList[i][[1]]$comments$from_id[j]
              commentFromName_TEMP  <- tempDataList[i][[1]]$comments$from_name[j]
              commentText_TEMP      <- tempDataList[i][[1]]$comments$message[j]
              commentTimestamp_TEMP      <- tempDataList[i][[1]]$comments$created_time[j]

              if(length(commentFromID_TEMP) < 1) {
                commentFromID_TEMP <- "NA"
              }
              if(length(commentFromName_TEMP) < 1) {
                commentFromName_TEMP <- "NA"
              }
              if(length(commentText_TEMP) < 1) {
                commentText_TEMP <- "NA"
              }
              if(length(commentTimestamp_TEMP) < 1) {
                commentTimestamp_TEMP <- "NA"
              }

              tempDataTableLOOP <- data.table(
                postID          = tempDataList[i][[1]]$post$id,
                postName        = tempDataList[i][[1]]$post$from_name,
                postMessage     = tempDataList[i][[1]]$post$message,
                postType        = tempDataList[i][[1]]$post$type,
                postLink        = tempDataList[i][[1]]$post$link,
                postTimestamp   = tempDataList[i][[1]]$post$created_time,
                commentFromID   = commentFromID_TEMP,
                commentFromName = commentFromName_TEMP,
                commentText     = commentText_TEMP,
                commentTimestamp     = commentTimestamp_TEMP)

              tempDataTable <- rbind(tempDataTable,tempDataTableLOOP)

              setkey(tempDataTable,postID) # set the key value of the data table

          }

          # remove dummy first row
          # tempDataTable <- tempDataTable[-1,]
          tempDataTable[postID != "foo"] # NEW DATA TABLE WAY
        }

        #####################################################
        ############### CREATE BIMODAL NETWORK ##############

        # create subsets of data, for processing

        usersCommentedPosts <- data.table(
          from          = tempDataTable$commentFromName,
          to            = tempDataTable$postID,
          relationship  = "User-Post Based on Comments",
          edgeWeight    = 1,
          postType      = tempDataTable$postType,
          postLink      = tempDataTable$postLink,
          postTimestamp = tempDataTable$postTimestamp,
          commentText   = tempDataTable$commentText,
          commentTimestamp   = tempDataTable$commentTimestamp
          )

        # dataCombined <- rbind(usersLikedPosts,usersCommentedPosts)
        dataCombined <- usersCommentedPosts # because we aren't including likes

        setkey(dataCombined,from) # set the key value of the data table

        # remove the "fake" NA values (these will occur due to the way the original data.table is structured, see above)
        # the data table way:
        dataCombined <- dataCombined[!"NA"]
        dataCombined <- na.omit(dataCombined)

        dataCombinedUNIQUE <- dataCombined ## keep the var name, so code below works (we aren't using unique anymore)

        # need to convert the timestamps to integer values.
        # we have two 'epochs' to work with.
        # our first 'epoch' will start from the timestamp of OLDEST comment, which will become 0 (zero).
        # our second 'epoch' will utilise the unix epoch, i.e. number of seconds since Jan 1 1970.
        # both of these 'epoch' edge attributes are useful in different ways.

        dataCombinedUNIQUE$commentTimestamp <- as.character(strptime(dataCombinedUNIQUE$commentTimestamp, "%Y-%m-%dT%H:%M:%S+0000"))

        # THIS GIVES US THE NUMBER OF SECONDS SINCE JANUARY 1, 1970:
        dataCombinedUNIQUE$commentTimestampUnixEpoch <- as.numeric(as.POSIXct(dataCombinedUNIQUE$commentTimestamp))

        tempVar <- strptime(dataCombinedUNIQUE$commentTimestamp, "%Y-%m-%dT%H:%M:%S+0000")

        tempVarConverted <- as.numeric(tempVar - min(tempVar))

        dataCombinedUNIQUE$commentTimestampConverted <- tempVarConverted

        class(dataCombinedUNIQUE) <- append(class(dataCombinedUNIQUE),c("dataSource","facebook","temporal"))

        if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
          #currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
          #currTime <- gsub(":","_",currTime)
          write.csv(dataCombined,paste0(rangeFrom,"_to_",rangeTo,"_",pageName,"_FacebookTemporalData.csv"))
          cat("Facebook temporal data was written to current working directory, with filename:\n")
          cat(paste0(rangeFrom,"_to_",rangeTo,"_",pageName,"_FacebookTemporalData.csv"))
        }

        cat("\n")

        return(dataCombinedUNIQUE)

  }

}
