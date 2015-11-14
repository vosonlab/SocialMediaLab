CreateBimodalNetwork.twitter <-
function(x,writeToFile,removeTermsOrHashtags)
{
  if (missing(writeToFile)) {
    writeToFile <- FALSE # default = not write to file
  }

  if (!missing(removeTermsOrHashtags)) {
    removeTermsOrHashtags <- as.vector(removeTermsOrHashtags) #coerce to vector... to be sure
  }

  if (missing(removeTermsOrHashtags)) {
    removeTermsOrHashtags <- "foobar"
  }

  df <- x # match the variable names (this must be used to avoid warnings in package compilation)

  # EnsurePackage("igraph")

      # Now create the dfBimodalNetwork2, a dataframe of relations between users and hashtags (i.e. user i "tweeted" hashtag j)

      print("Generating Twitter bimodal network...")  ### DEBUG
      flush.console()

      usersTemp <- c() # temp var to store output
      hashtagsUsedTemp <- c() # temp var to store output

      # The 'hashtags_used' column in the 'df' dataframe is slightly problematic (i.e. not straightforward)
      # because each cell in this column contains a LIST, itself containing 1 or more char vectors (which are unique hashtags found in the tweet text; empty if no hashtags used).
      # So, need to extract each list item out, and put it into its own row in a new dataframe:

      for (i in 1:nrow(df)) {
        if (length(df$hashtags_used[[i]]) > 0) { # skip any rows where NO HASHTAGS were used
          for (j in 1:length(df$hashtags_used[[i]])) {
            usersTemp <- c(usersTemp, df$from_user[i])
            hashtagsUsedTemp <- c(hashtagsUsedTemp,df$hashtags_used[[i]][j])
          }
        }
      }   # NOTE: try and vectorise this in future work to improve speed.

      # create the "bimodal actor-hashtag network" dataframe (i.e. pairs of users/hashtags; user i --(tweets using)--> hashtag j)
      dfBimodalNetwork2 <- data.frame(usersTemp, hashtagsUsedTemp)

      # OK, now extract only the UNIQUE pairs (i.e. rows)
      # But, also create a WEIGHT value for usages of the same hashtag.
          # NOTE: This edge weights approach might be problematic for TEMPORAL networks, because each edge (with weight > 1) may represent usage of hashtags at DIFFERENT TIMES.
          # NOTE: A possible workaround could be to include an edge attribute that is a set of timestamp elements, showing the date/time of each instance of usage of a hashtag.
          # NOTE: For example, in a temporal visualisation, the first timestamp might 'pop in' the edge to the graph, which then might start to 'fade out' over time (or just 'pop out' of graph after N seconds) if there are no more timestamps indicating activity (i.e. a user using a hashtag).
          # NOTE: So, a 'timestamps' edge attribute could factor into a kind of 'entropy' based approach to evolving the network visually over time.

      # unique pairs:
      unique_dfBimodalNetwork2 <- unique(dfBimodalNetwork2) # hmm, need this still?

      # number of times hashtag was used per user/hashtag pair (i.e. edge weight):
      for (i in 1:nrow(unique_dfBimodalNetwork2)) {
        unique_dfBimodalNetwork2$numHashtagUsages[i] <- sum(usersTemp==unique_dfBimodalNetwork2[i,1] & hashtagsUsedTemp==unique_dfBimodalNetwork2[i,2])
      }

      # make a vector of all the unique actors in the network1
      actorsNames <- unique(factor(c(as.character(unique(unique_dfBimodalNetwork2$usersTemp)),as.character(unique(unique_dfBimodalNetwork2$hashtagsUsedTemp)))))

      # Retrieve all the USER details (e.g. follower count, number of tweets, etc) and include as node attributes.
        # NOTE: Given API rate limits, the below implementation supports up to 7500 users overall in dataset (150 requests * 50 users per request).
        # NOTE: Future work needs to address the Twitter API rate limit for looking up user information (150 requests per 15 minutes).
        # NOTE: In my experience requesting 50 users at a time seems to avoid rate limit errors (it's a safe bet!).

          # This function is supposed to perform the lookups in batches
          # and mind the rate limit:
          getUserObjects <- function(users) {
            groups <- split(users, ceiling(seq_along(users)/50))
            userObjects <- ldply(groups, function(group) { # ldply is a very cool function, found in plyr package.
              objects <- lookupUsers(group)
              out <- twListToDF(objects) # twListToDF is also a handy function, found in twitteR package. Converts weird class object to data frame.
                # print("Waiting for 15 minutes (to 'refresh' the rate limit)...") # Don't need to use this yet. Implement later for number of users > 7500 (have to do chunked batches... chunks of chunks... urrghh)
                # Sys.sleep(900)
              return(out)
            })
            return(userObjects)
          }

      # Putting it into action:
      usersInformationAttributes <- getUserObjects(as.character(unique(unique_dfBimodalNetwork2$usersTemp)))
      actorsInfoDF <- usersInformationAttributes

      # Need to clean the user text collected (get rid of odd characters):
      actorsInfoDF <- RemoveOddCharsUserInfo(actorsInfoDF) # uses the new function in v2_munge_tweets.R

      # We sometimes have a PROBLEM of missing actors (no info could be retrieved for them - might be misspellings/errors/pun or joke, etc)
      # So, identify which users are missing from original set to retrieved set, then ensure these users/connections are removed before proceeding onwards:

      missingActors <- setdiff(as.character(unique(unique_dfBimodalNetwork2$usersTemp)),usersInformationAttributes$screenName) # not "actorsNames" object any more (as per Network1 script)!
        # NOTE: This is a horrible approach, need to optimise.
      missingTemp <- NULL # store the indexes of "offending" edge connections (i.e. bad/missing actors)
        # NOTE: Obviously the 'offending' users can only be found in the 2nd column
        # NOTE: Ipso facto, if they are hashtags then they can't be the source of a directed edge... (do hashtags dream of sending tweets?...).

      for (i in 1:length(missingActors)) {
        missingTemp <- c(missingTemp, which(missingActors[i] == unique_dfBimodalNetwork2$usersTemp))
      }

      if (length(missingTemp > 0)) { # ** only do this if there are missing users to deal with **
        # REMOVE the offenders:
        unique_dfBimodalNetwork2 <- unique_dfBimodalNetwork2[-missingTemp,]

        # REMOVE any duplicated usernames in the retrieved user information (NOT SURE HOW/WHY THIS WOULD OCCUR **NEED TO CHECK**):
        duplicatedUsers <- which(duplicated(actorsInfoDF$screenName))
        actorsInfoDF <- actorsInfoDF[-duplicatedUsers,]
      }

      # Construct a data frame of USER nodes and attributes (The "mode" of actor here is USER)
      actors <- data.frame(
        name=actorsInfoDF$screenName,
        modeOfNode="USER",
        userDescription=actorsInfoDF$description,
        statusesCount=actorsInfoDF$statusesCount,
        followersCount=actorsInfoDF$followersCount,
        favoritesCount=actorsInfoDF$favoritesCount,
        friendsCount=actorsInfoDF$friendsCount,
        url=actorsInfoDF$url,
        realName=actorsInfoDF$name,
        dateAccountCreated=actorsInfoDF$created,
        userLocation=actorsInfoDF$location,
        userLanguage=actorsInfoDF$lang,
        numberOfListsUserIsFeaturedOn=actorsInfoDF$listedCount,
        profileImageUrl=actorsInfoDF$profileImageUrl
        )

      # Construct a data frame of HASHTAG nodes and attributes (The "mode" of actor here is HASHTAG)
      # So, need to reinsert the hashtags as "actors" in the final dataframe (even the hashtag nodes will not have any 'user information' values in those attributes... obviously)
          # NOTE: This is a quick and dirty method! Future work should improve/optimise this.

      actorsHashtags <- data.frame(
          name=unique(hashtagsUsedTemp),
          modeOfNode="HASHTAG",
          userDescription=NA,
          statusesCount=NA,
          followersCount=NA,
          favoritesCount=NA,
          friendsCount=NA,
          url=NA,
          realName=NA,
          dateAccountCreated=NA,
          userLocation=NA,
          userLanguage=NA,
          numberOfListsUserIsFeaturedOn=NA,
          profileImageUrl=NA
          )

      actorsFixed <- rbind(actors,actorsHashtags)

      # make a dataframe of the relations between actors
      relations <- data.frame(from=unique_dfBimodalNetwork2$usersTemp,to=unique_dfBimodalNetwork2$hashtagsUsedTemp,weight=unique_dfBimodalNetwork2$numHashtagUsages)

      ##### STEP FOUR #####

      # convert into a graph
      g <- graph.data.frame(relations, directed=TRUE, vertices=actorsFixed)
      # shouldn't need to simplify the graph, but it can't hurt anyway
      # g <- simplify(g)

      # Make the node labels play nice with Gephi
      V(g)$label <- V(g)$name

      # remove the search term / hashtags, if user specified it:
      if (removeTermsOrHashtags[1]!="foobar") {
          toDel <- match(tolower(removeTermsOrHashtags),V(g)$name) # we force to lowercase because all terms/hashtags are already converted to lowercase
          toDel <- toDel[!is.na(toDel)] # in case of user error (i.e. trying to delete terms/hashtags that don't exist in the data)
          g <- delete.vertices(g, toDel)
      }

      if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
        # Output the final network to a graphml file, to import directly into Gephi
        currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
        currTime <- gsub(":","_",currTime)
        write.graph(g,paste0(currTime,"_TwitterBimodalNetwork.graphml"),format="graphml")
        cat("Twitter bimodal network was written to current working directory, with filename:\n")
        cat(paste0(currTime,"_TwitterBimodalNetwork.graphml"))
      }

      cat("\nDone\n") ### DEBUG
      flush.console()

    return(g)

}
