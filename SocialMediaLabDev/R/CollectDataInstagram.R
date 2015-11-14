CollectDataInstagram <-
function(tag, n, lat, lng, distance, folder, mindate, maxdate, verbose, sleep, writeToFile, waitForRateLimit) {

  from_userID = from_username = from_full_name = from_profile_picture = edge_type = to_post_id = post_created_time = post_type = post_longitude = post_latitude = post_location_name = post_location_id = post_link = post_image_URL = post_caption = post_username = post_user_ID = post_user_fullname = comment_created_time = comment_text = commentsData = likesData = NULL

  if(!(exists("instagram_oauth_token"))) {
    instagram_oauth_token <- NULL
  }

  # handle the arguments

  if (missing(verbose)) {
    verbose <- FALSE # default = not verbose
  }

  if (missing(writeToFile)) {
    writeToFile <- FALSE # default = not write to file
  }

  if (verbose=="TRUE" | verbose=="true" | verbose=="T" | verbose==TRUE) {
    verbose <- TRUE
  }
  else {verbose <- FALSE}

  if (missing(n)) {
    n <- 100 # default to 100 max posts
  }

  if (missing(tag)) {
    tag <- NULL
  }

  if (missing(lat)) {
    lat <- NULL
  }

  if (missing(lng)) {
    lng <- NULL
  }

  if (missing(distance)) {
    distance=1000
  }

  if (distance < 1 | distance > 5000) {
    cat("\nNote: maximum distance for searching posts is 5000 (metres). Value changed to 5000.")
    distance <- 5000
  }

  if (missing(folder)) {
    folder <- NULL
  }

  if (missing(mindate)) {
    mindate <- NULL
  }

  if (missing(maxdate)) {
    maxdate <- NULL
  }

  if (missing(sleep)) {
    sleep <- 0 # time to sleep in seconds between API calls
  }

  if (missing(waitForRateLimit)) {
    waitForRateLimit <- FALSE # the default is NOT to wait for the rate limit. This will probably result in an error if the rate limit is maxed out.
  }

  # Start data collection

  rateLimitHourTimer <- proc.time() # start the 60 minute timer (for minding the rate limit if waitForRateLimit==TRUE)
  totalCallsToAPI <- 0

  testData <- searchInstagram(tag=tag, n=n, lat=lat, lng=lng, distance=distance, folder=folder, mindate=mindate, maxdate=maxdate, verbose=verbose, sleep=sleep, token=instagram_oauth_token)
  if (n <= 50) {totalCallsToAPI <- totalCallsToAPI + 1} # increment by 1
  if (n > 50) {totalCallsToAPI <- totalCallsToAPI + round(n / 100)} # increment 1 call for every 100 posts requested (calls are made in batches of 100)

  # create a new column (list) to store the comments data
  testData$commentsData <- vector("list",nrow(testData))

  # create a new column (list) to store the likes data
  testData$likesData <- vector("list",nrow(testData))

  # (2) get all the comments for all posts in the dataset
  # and store the results back into the dataframe
  # NOTE: implicitly convert the data to data.table objects
  for (i in 1:nrow(testData)) {
    if (testData$comments_count[i] != 0) { # check to make sure there are comments to collect

      if (verbose & (totalCallsToAPI %% 100 == 0)) { # inform the user every 100 calls
        cat(paste0("\nNumber of API calls used so far is: ",totalCallsToAPI,"\n"))
      }

      if (waitForRateLimit & totalCallsToAPI==4999 & (proc.time() - rateLimitHourTimer)[3] < 3600) { # if we have hit the limit within one hour
        cat("\nThe rate limit has reached maximum capacity! Sleeping now for 60 minutes...\n")
        Sys.sleep(3600)
#cat("\nThe rate limit has reached 60! Sleeping now for 2 minutes...\n") # DEBUG
# Sys.sleep(120) # DEBUG
        cat("Waking up now! Back to work...\n")
        totalCallsToAPI <- 0 # reset number of calls
        rateLimitHourTimer <- proc.time() # reset hourly timer
      }

      possibleError <- tryCatch({ # we also need to catch errors, e.g. if there is one comment but it gets deleted in between calls
        testData$commentsData[i][[1]] <- getComments(testData$id[i], instagram_oauth_token)
        totalCallsToAPI <- totalCallsToAPI + 1
        testData$commentsData[i][[1]] <- data.table(testData$commentsData[i][[1]]) # convert to data.table
        },
        error=function(e) e
      )
      if(inherits(possibleError, "error")) {
        cat(paste0("\n I caught an error collecting comments data... (row ",i,")\n"))
        next
        }
    }
  }

  # (3) get all the likes for all posts in the dataset
  # and store the results back into the dataframe
  # NOTE: implicitly convert the data to data.table objects
  for (i in 1:nrow(testData)) {
    if (testData$likes_count[i] != 0) { # check to make sure there are likes to collect

      if (verbose & (totalCallsToAPI %% 100 == 0)) { # notify every 100 calls
        cat(paste0("\nNumber of API calls is: ",totalCallsToAPI,"\n"))
      }

      if (waitForRateLimit & totalCallsToAPI==4999 & (proc.time() - rateLimitHourTimer)[3] < 3600) { # if we have hit the limit within one hour
        cat("\nThe rate limit has reached maximum capacity! Sleeping now for 60 minutes...\n")
        Sys.sleep(3600)
# cat("\nThe rate limit has reached 60! Sleeping now for 2 minutes...\n") # DEBUG
# Sys.sleep(120) # DEBUG
        cat("Waking up now! Back to work...\n")
        totalCallsToAPI <- 0 # reset number of calls
        rateLimitHourTimer <- proc.time() # reset hourly timer
      }

      possibleError <- tryCatch({ # we also need to catch errors, e.g. if there is one like but it gets deleted in between calls
        testData$likesData[i][[1]] <- getLikes(testData$id[i], instagram_oauth_token)
        totalCallsToAPI <- totalCallsToAPI + 1
          testData$likesData[i][[1]] <- data.table(testData$likesData[i][[1]]) # convert to data.table
          },
        error=function(e) e
      )
      if(inherits(possibleError, "error")) {
        cat(paste0("\n I caught an error collecting likes data... (row ",i,")\n"))
        next
      }
    }
  }

  # now we convert the dataframe to a data.table to make processing FASTER
  # (this doesn't convert the dataframes in the list vectors, but we have already done that)
  testDataTable <- data.table(testData)

  ## STEP 2 - sort through the data and make a data.table of relations

  # for each row in testDataTable we extract data from `testDataTable$commentsData` and `testDataTable$likesData`
  # and put this into a data.table `dataCombined` that we can then construct a bimodal graph out of.

  # for speed we will pre-allocate `dataCombined` to a very large size (more rows than needed)
  # and after everything is finished we will delete the unused rows

  dataCombined <- data.table(
    from_userID = rep("NA_f00",20000000),
    from_username = rep("NA_f00",20000000),
    from_full_name = rep("NA_f00",20000000),
    from_profile_picture = rep("NA_f00",20000000),
    edge_type = rep("NA_f00",20000000),
    to_post_id = rep("NA_f00",20000000),

    post_created_time = rep(Inf,20000000),
    post_type = rep("NA_f00",20000000),
    post_longitude = rep(Inf,20000000),
    post_latitude = rep(Inf,20000000),
    post_location_name = rep("NA_f00",20000000),
    post_location_id = rep(Inf,20000000),
    post_link = rep("NA_f00",20000000),
    post_image_URL = rep("NA_f00",20000000),
    post_caption = rep("NA_f00",20000000),
    post_username = rep("NA_f00",20000000),
    post_user_ID = rep("NA_f00",20000000),
    post_user_fullname = rep("NA_f00",20000000),

    comment_created_time = rep(Inf,20000000),
    comment_text = rep("NA_f00",20000000)
  )

  setkey(dataCombined,from_userID) # set the key value of the data table

  nextEmptyRow <- 1 # so we can update rows in `dataCombined` in a relatively efficient way

  # We firstly do the comments data
  for (i in 1:nrow(testDataTable)) {

    if (is.null(testDataTable[i,commentsData][[1]])) {next} # we check if there are comments, if not skip to next row

    for (j in 1:nrow(testDataTable$commentsData[i][[1]])){ # for each row of the comments data for post i

      # nextEmptyRow <- dataCombined[  , .I[from_userID=="NA_f00"] ][1] # we get index of the next 'empty' row to put data into # NOT NEEDED NOW, BUT USEFUL FOR LATER

      dataCombined[nextEmptyRow, from_userID := testDataTable$commentsData[i][[1]]$from_id[j]]
      dataCombined[nextEmptyRow, from_username := testDataTable$commentsData[i][[1]]$from_username[j]]
      dataCombined[nextEmptyRow, from_full_name := testDataTable$commentsData[i][[1]]$from_full_name[j]]
      dataCombined[nextEmptyRow, from_profile_picture := testDataTable$commentsData[i][[1]]$from_profile_picture[j]]
      dataCombined[nextEmptyRow, edge_type := "comment"]
      dataCombined[nextEmptyRow, to_post_id := testDataTable$id[i]] # here we want the post id that the person commented on

      dataCombined[nextEmptyRow, post_created_time := testDataTable$created_time[i]]
      dataCombined[nextEmptyRow, post_type := testDataTable$type[i]]
      dataCombined[nextEmptyRow, post_longitude := testDataTable$longitude[i]]
      dataCombined[nextEmptyRow, post_latitude := testDataTable$latitude[i]]
      dataCombined[nextEmptyRow, post_location_name := testDataTable$location_name[i]]
      dataCombined[nextEmptyRow, post_location_id := testDataTable$location_id[i]]
      dataCombined[nextEmptyRow, post_link := testDataTable$link[i]]
      dataCombined[nextEmptyRow, post_image_URL := testDataTable$image_url[i]]
      dataCombined[nextEmptyRow, post_caption := testDataTable$caption[i]]
      dataCombined[nextEmptyRow, post_username := testDataTable$username[i]]
      dataCombined[nextEmptyRow, post_user_ID := testDataTable$user_id[i]]
      dataCombined[nextEmptyRow, post_user_fullname := testDataTable$user_fullname[i]]

      dataCombined[nextEmptyRow, comment_created_time := testDataTable$commentsData[i][[1]]$created_time[j]]
      dataCombined[nextEmptyRow, comment_text := testDataTable$commentsData[i][[1]]$text[j]]

      nextEmptyRow <- nextEmptyRow + 1 # increment the row to update in `dataCombined`

    }

  }

  # Next we do the likes data
  for (i in 1:nrow(testDataTable)) {

    if (is.null(testDataTable[i,likesData][[1]])) {next} # we check if there are likes, if not skip to next row

    for (j in 1:nrow(testDataTable$likesData[i][[1]])){ # for each row of the likes data for post i

      # nextEmptyRow <- dataCombined[  , .I[from_userID=="NA_f00"] ][1] # we get index of the next 'empty' row to put data into # NOT NEEDED NOW, BUT USEFUL FOR LATER

      dataCombined[nextEmptyRow, from_userID := testDataTable$likesData[i][[1]]$id[j]]
      dataCombined[nextEmptyRow, from_username := testDataTable$likesData[i][[1]]$username[j]]
      dataCombined[nextEmptyRow, from_full_name := testDataTable$likesData[i][[1]]$full_name[j]]
      dataCombined[nextEmptyRow, from_profile_picture := testDataTable$likesData[i][[1]]$profile_picture[j]]
      dataCombined[nextEmptyRow, edge_type := "like"]
      dataCombined[nextEmptyRow, to_post_id := testDataTable$id[i]] # here we want the post id that the person commented on

      dataCombined[nextEmptyRow, post_created_time := testDataTable$created_time[i]]
      dataCombined[nextEmptyRow, post_type := testDataTable$type[i]]
      dataCombined[nextEmptyRow, post_longitude := testDataTable$longitude[i]]
      dataCombined[nextEmptyRow, post_latitude := testDataTable$latitude[i]]
      dataCombined[nextEmptyRow, post_location_name := testDataTable$location_name[i]]
      dataCombined[nextEmptyRow, post_location_id := testDataTable$location_id[i]]
      dataCombined[nextEmptyRow, post_link := testDataTable$link[i]]
      dataCombined[nextEmptyRow, post_image_URL := testDataTable$image_url[i]]
      dataCombined[nextEmptyRow, post_caption := testDataTable$caption[i]]
      dataCombined[nextEmptyRow, post_username := testDataTable$username[i]]
      dataCombined[nextEmptyRow, post_user_ID := testDataTable$user_id[i]]
      dataCombined[nextEmptyRow, post_user_fullname := testDataTable$user_fullname[i]]

      dataCombined[nextEmptyRow, comment_created_time := NA] # not applicable because this is 'likes' data
      dataCombined[nextEmptyRow, comment_text := NA] # not applicable because this is 'likes' data

      nextEmptyRow <- nextEmptyRow + 1 # increment the row to update in `dataCombined`

    }

  }

  # we now delete all the rows at the end of `dataCombined` that are unused
  dataCombined <- dataCombined[from_profile_picture != "NA_f00"] # we just keep the rows that are unchanged from the original dummy data values

  # finish up and return...

    class(dataCombined) <- append(class(dataCombined),c("dataSource","instagram"))

    if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
      currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
      currTime <- gsub(":","_",currTime)
      write.csv(dataCombined,paste0("Instagram_Data_",currTime,".csv"))
      cat("Instagram data was written to current working directory, with filename:\n")
      cat(paste0("Instagram_Data_",currTime,".csv"))
    }

    cat("\n")

    return(dataCombined)

}
