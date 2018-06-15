#' Collect YouTube Video Comments
#' 
#' YouTube Video Comments Collection with R tuber package
#' DO NOT CALL DIRECTLY - USE \code{Collect}
#'
#' Collection performed using the Google API via the tuber package:
#' http://soodoku.github.io/tuber/reference/get_all_comments.html
#'
#' Collects and structures the data into a data frame of class
#' \code{dataSource.youtube2} suitable for creating networks such as
#' a unimodal actor network (\code{CreateActorNetwork}).
#'
#' @param credential optional, character vector containing authentication token.
#' @param videoIDs character vector, specifying one or more YouTube video IDs.
#' @param verbose boolean, if \code{TRUE} then this function will output
#' information to the console during processing. Default is \code{FALSE}.
#' @param writeToFile boolean, if \code{TRUE} then the data is saved to file in
#' current working directory in \code{CSV} format. The file name will be
#' the current system time and data type as 
#' \code{MMM_DD_HH_MM_SS_YYYY_YoutubeData2.csv}. The file is appended as each
#' videos comments data is collected. Default is \code{FALSE}.
#' @param maxComments numeric integer, sets limit for how many total comments
#' to collect. This is a very rough measure that only works if comments from
#' multiple videos are requested. If \code{maxComments} is reached in the
#' current video, no further videos will be processed. By default this
#' function attempts to collect all comments.
#' @return data frame object of class \code{dataSource.youtube2}
#'
#' @seealso \code{AuthenticateWithYoutube2} must be run first or no data will
#' be collected.
#'
#' @export
CollectDataYoutube2 <- function(credential=NA, videoIDs, verbose, writeToFile=FALSE, maxComments) {
  
  if (missing(verbose)) {
    verbose <- FALSE
  }

  if (verbose=="TRUE" | verbose=="true" | verbose=="T" | verbose==TRUE) {
    verbose <- TRUE
  } else {
    verbose <- FALSE
  }
  
  if (missing(maxComments)) {
    maxComments <- 10000000000000
  }
  
  if (missing(videoIDs)) {
    cat("error: argument 'videoIDs' is missing.\n")
    cat("please specify a vector of videoIDs to collect data from.\n")
    cat("hint: to do this you can use the 'GetYoutubeVideoIDs' function in this package.")
    return(NA)
  }
  
  combined_video_comments <- data.frame(x=numeric(0), y=numeric(0))
  video_count <- 0
  comment_count <- 0
  
  currTime <- format(Sys.time(), "%b_%d_%H_%M_%S_%Y")
  
  for (i in 1:length(videoIDs)) {
    video_id <- videoIDs[i]
    
    if (verbose) { 
      cat(paste0("\ngetting comments for video: ", video_id, " (", i, " of ", length(videoIDs), ")\n"))
    }
    
    # sometimes errors if no comments, catch errors so can continue processing
    video_comments <- data.frame(x=numeric(0), y=numeric(0))
    tryCatch(video_comments <- get_all_comments(video_id), 
             error = function(e) {
               cat(paste0("error in tuber get_all_comments(\"", video_id,"\")\n"))
               cat(paste0(e))
             })
    
    comment_count <- comment_count + nrow(video_comments)
    
    if (verbose) { 
      cat(paste0("video comments collected: ", nrow(video_comments), " (", comment_count, " total | ", maxComments, " max)\n"))
    }
    
    video_count <- video_count + 1
    
    # write data to a csv file
    if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
      output_file_name <- paste0(currTime, "_YoutubeData2.csv")
      
      if (nrow(video_comments) > 0) {
        if (i == 1) {
          write.table(video_comments, file=output_file_name, row.names=FALSE, 
                      col.names=TRUE, sep=",", quote=TRUE)
        } else {
          write.table(video_comments, file=output_file_name, row.names=FALSE, 
                      col.names=FALSE, sep=",", quote=TRUE, append=TRUE)
        }
        cat(paste0("youtube comments data for video ", video_id, " was written to filename:\n"))
        cat(output_file_name)        
      }
    }    
    
    # append video comments to combined comment data frame
    if (i == 1) {
      combined_video_comments <- video_comments
    } else {
      combined_video_comments <- rbind(combined_video_comments, video_comments)
    }
    
    # clear loop data frame
    video_comments = video_comments[0,]
    
    # very coarse comment limiting, delimited by video 
    if (comment_count>=maxComments && i<length(videoIDs)) {
      cat(paste0("\nmax comments reached: ", comment_count, " (max_comments = ", maxComments, ")\n"))
      cat(paste0("video count: ", i, " of ", length(videoIDs), "\n"))
      break
    }
  }

  if (comment_count == 0) {
    cat("no comments were found or able to collected.")
  }    
  
  if (verbose) {
    cat(paste0("\ntotal comments collected: ", comment_count, " (", video_count, " videos)\n"))
  }
  
  class(combined_video_comments) <- append(class(combined_video_comments), c("dataSource", "youtube2"))
  
  return(combined_video_comments)
}

#' Gets a the author channel id and display name for a given youtube video id
#' 
#' tuber function get_video_details can accept a vector of video ids, function
#' needs to be adjusted to do this in the future to save api requests.
#' 
#' @param videID id string of youtube video
#' @return data.frame videoId, authorChannelId.value and authorDisplayName
#' @noRd
GetYoutubeVideoAuthor <- function(videoID) {
  video_channel_id <- ""
  video_channel_title <- ""
  
  # catches errors so they don't halt upstream processing
  tryCatch({video_details <- get_video_details(videoID)
  
            video_channel_id <- video_details[["items"]][[1]][["snippet"]][["channelId"]]
            video_channel_title <- video_details[["items"]][[1]][["snippet"]][["channelTitle"]]},
            error = function(e) {
              cat(paste0("error in tuber get_video_details(\"", videoID,"\")\n"))
              cat(paste0(e))
            })
  
  return(data.frame(videoId=videoID, authorChannelId.value=video_channel_id, 
                    authorDisplayName=video_channel_title, stringsAsFactors=FALSE))
}
