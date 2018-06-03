#' @export
CollectDataYoutube2 <- function(credential, videoIDs, verbose, writeToFile=FALSE, maxComments) {
  
  if (missing(verbose)) {
    verbose <- FALSE
  }
  
  if (missing(maxComments)) {
    maxComments <- 10000000000000
  }
  
  if (missing(writeToFile)) {
    writeToFile <- FALSE
  }
  
  if (missing(videoIDs)) {
    cat("error: argument 'videoIDs' is missing.\n")
    cat("please specify a vector of videoIDs to collect data from.\n")
    cat("hint: to do this you can use the 'GetYoutubeVideoIDs' function in this package.")
    break
  }
  
  if (verbose=="TRUE" | verbose=="true" | verbose=="T" | verbose==TRUE ) {
    verbose <- TRUE
  } else {
    verbose <- FALSE
  }
  
  combined_video_comments <- data.frame(x=numeric(0), y=numeric(0))
  video_count <- 0
  comment_count <- 0
  
  currTime <- format(Sys.time(), "%b_%d_%H_%M_%S_%Y_%Z")
  
  for (i in 1:length(videoIDs)) {
    video_id <- videoIDs[i]
    
    if (verbose) { 
      cat(paste0("\ngetting comments for video: ", video_id, " (", i, " of ", length(videoIDs), ")\n"))
    }
    
    # sometimes errors if no comments, catch so we can continue processing
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
    
    # write to file
    if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
      output_file_name <- paste0(currTime, "_YoutubeData2.csv")
      
      if (nrow(video_comments) > 0) {
        if (i == 1) {
          write.table(video_comments, file=output_file_name, row.names=FALSE, col.names=TRUE, sep=",", quote=TRUE)
        } else {
          write.table(video_comments, file=output_file_name, row.names=FALSE, col.names=FALSE, sep=",", quote=TRUE, append=TRUE)
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
    if (comment_count>=maxComments && i<length(video_ids)) {
      cat(paste0("\nmax comments reached: ", comment_count, " (max_comments = ", maxComments, ")\n"))
      cat(paste0("video count: ", i, " of ", length(video_ids), "\n"))
      break
    }
  }

  if (comment_count == 0) {
    cat("no comments were found or able to collected.")
  }    
  
  if (verbose) {
    cat(paste0("\ntotal comments collected: ", comment_count, " (", video_count, " videos)\n"))
  }
  
  #return(combined_video_comments)
  class(combined_video_comments) <- append(class(combined_video_comments), c("dataSource", "youtube2"))
  
  return(combined_video_comments)
}

# returns data frame of video id, author id and display name
GetYoutubeVideoAuthor <- function(videoID) {
  video_channel_id <- ""
  video_channel_title <- ""
  
  tryCatch({video_details <- get_video_details(videoID)
  
            video_channel_id <- video_details[["items"]][[1]][["snippet"]][["channelId"]]
            video_channel_title <- video_details[["items"]][[1]][["snippet"]][["channelTitle"]]},
            error = function(e) {
              cat(paste0("error in tuber get_video_details(\"", video_id,"\")\n"))
              cat(paste0(e))
            })
  
  return(data.frame(videoId=videoID, authorChannelId.value=video_channel_id, authorDisplayName=video_channel_title, stringsAsFactors=FALSE))
}
