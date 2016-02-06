#' @export
CreateActorNetwork.list <-
function(x,writeToFile)
{

  if (missing(writeToFile)) {
    writeToFile <- FALSE # default = not write to file
  }

  # the user has passed a LIST of DATAFRAMES to the function,
  # so we rbind these into one data frame, before proceeding

  df <- as.data.frame(data.table::rbindlist(x))

  # EnsurePackage("igraph")

  # The `hashtags_used` column in `df` causes problems for creating actor network, so delete it:

  df <- df[,-21]

    # Now create the dfActorNetwork1, a dataframe of relations between users (i.e. node i "mentions" node j)
        # Note: we define a "mention" more broadly as any explicit reference, i.e. retweeting, replying, or tweeting 'to' another user.

    cat("Generating the network...\n")  ### DEBUG
    flush.console()

    usersTemp <- c() # temp var to store output
    mentionedUsersTemp <- c() # temp var to store output

    # The 'users_mentioned' column in the 'df' dataframe is slightly problematic (i.e. not straightforward)
    # because each row in this column contains a LIST, itself containing 1 or more char vectors (which are 'mentions' of usernames, or if no mentions then empty).
    # (Note: this improves on previous approaches that only take the FIRST pattern match of a 'mention' in any given tweet text).
    # So, need to extract each list item out, and put it into its own row in a new dataframe:

    for (i in 1:nrow(df)) {
      if (length(df$users_mentioned[[i]]) > 0) { # skip any rows where NO USERS were mentioned
        for (j in 1:length(df$users_mentioned[[i]])) {
          usersTemp <- c(usersTemp, df$from_user[i])
          mentionedUsersTemp <- c(mentionedUsersTemp,df$users_mentioned[[i]][j])
        }
      }
    }   # NOTE: try and vectorise this loop in future work to improve speed.

    # create the "mentions network" dataframe (i.e. pairs of users; node i --(mentions)--> node j)
    dfActorNetwork1 <- data.frame(usersTemp, mentionedUsersTemp)

    # OK, now extract only the UNIQUE pairs (i.e. rows)
    # But, also create a WEIGHT value for multiple mentions between users
        # NOTE: This edge weights approach might be problematic for TEMPORAL actor networks, because each edge (with weight > 1) represents mentions in tweets at DIFFERENT TIMES.
        # NOTE: A possible workaround could be to include an edge attribute that is a set of timestamp elements, showing the date/time of each unique 'mention'.
        # NOTE: For example, in a temporal visualisation, the first timestamp might 'pop in' the edge to the graph, which then might start to 'fade out' over time (or just 'pop out' of graph after N seconds) if there are no more timestamps indicating activity (i.e. mentions) between the two users.
        # NOTE: So, a 'timestamps' edge attribute could factor into a kind of 'entropy' based approach to evolving the network visually over time.

    # unique pairs:
    unique_dfActorNetwork1 <- unique(dfActorNetwork1)

    # number of mentions per pair (i.e. edge weight):
    for (i in 1:nrow(unique_dfActorNetwork1)) {
      unique_dfActorNetwork1$numMentions[i] <- sum(usersTemp==unique_dfActorNetwork1[i,1] & mentionedUsersTemp==unique_dfActorNetwork1[i,2])
    }

    # make a vector of all the unique actors in the network1
    actorsNames <- unique(factor(c(as.character(unique(unique_dfActorNetwork1$usersTemp)),as.character(unique(unique_dfActorNetwork1$mentionedUsersTemp)))))

    # Retrieve all the user details (e.g. follower count, number of tweets, etc) and include as node attributes.
      # NOTE: Given API rate limits, the below implementation supports up to 7500 users overall in dataset (150 requests * 50 users per request).
      # NOTE: Future work needs to address the Twitter API rate limit for looking up user information (150 requests per 15 minutes).
      # NOTE: In my experience requesting 50 users at a time seems to avoid rate limit errors (it's a safe bet...).

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
    usersInformationAttributes <- getUserObjects(actorsNames)
    actorsInfoDF <- usersInformationAttributes

    # Need to clean the user text collected here (get rid of odd characters):
    actorsInfoDF <- RemoveOddCharsUserInfo(actorsInfoDF) # uses the new function in v2_munge_tweets.R

    # We sometimes have a PROBLEM of missing actors (no info could be retrieved for them - might be misspellings/errors/pun or joke, etc)
    # So, identify which users are missing from original set to retrieved set, then ensure these users/connections are removed before proceeding onwards:

    missingActors <- setdiff(actorsNames,usersInformationAttributes$screenName)
      # NOTE: This is a horrible approach, need to optimise.
    missingTemp <- NULL # store the indexes of "offending" edge connections (i.e. bad/missing actors)
      # NOTE: Obviously the 'offending' users can only be found in the 2nd column
      # NOTE: Ipso facto, if they are not real/actual users, then they can't be the source of a directed edge... (do ghosts send tweets?)...

    for (i in 1:length(missingActors)) {
      missingTemp <- c(missingTemp, which(missingActors[i] == unique_dfActorNetwork1$mentionedUsersTemp))
    }

    ## remove NA values
    toDel <- which(is.na(unique_dfActorNetwork1$mentionedUsersTemp) | is.na(unique_dfActorNetwork1$usersTemp))
    # REMOVE the offendors:
    if(length(toDel) > 0) {
      unique_dfActorNetwork1 <- unique_dfActorNetwork1[-toDel,]
    }

    # REMOVE the offendors:
    if(length(missingTemp) > 0) {
    unique_dfActorNetwork1 <- unique_dfActorNetwork1[-missingTemp,]
    }

    # REMOVE any duplicated usernames in the retrieved user information (NOT SURE HOW/WHY THIS WOULD OCCUR **NEED TO CHECK**):
    duplicatedUsers <- which(duplicated(actorsInfoDF$screenName))

    if(length(duplicatedUsers) > 0) {
      actorsInfoDF <- actorsInfoDF[-duplicatedUsers,]
    }

    actors <- data.frame(
      name=actorsInfoDF$screenName,
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

    # make a dataframe of the relations between actors
      # NOTE - FUTURE WORK: include edge attributes to specify the specific type of "mentions" (see previous comments on temporal network problem (see: approx. LINES 113-116)).
      # NOTE - For example, "RETWEET" versus "TWEET TO" (@username specified beginning of tweet) versus "MENTION" (@username specified somewhere else in tweet text)
    relations <- data.frame(from=unique_dfActorNetwork1$usersTemp,to=unique_dfActorNetwork1$mentionedUsersTemp,weight=unique_dfActorNetwork1$numMentions)

    ##### STEP FOUR #####

    # convert into a graph
    g <- graph.data.frame(relations, directed=TRUE, vertices=actors)
    # shouldn't need to simplify the graph, but it can't hurt anyway
    # edit: there could be very specific cases where simplifying is NOT WANTED, e.g. users who mention themselves...?
    # g <- simplify(g)

    # Make the node labels play nice with Gephi
    V(g)$label <- V(g)$name

    if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
      # Output the final network to a graphml file, to import directly into Gephi
      currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
      currTime <- gsub(":","_",currTime)
      write.graph(g,paste0(currTime,"_TwitterActorNetwork.graphml"),format="graphml")
      cat("Twitter actor network was written to current working directory, with filename:\n")
      cat(paste0(currTime,"_TwitterActorNetwork.graphml"))
    }

    cat("\nDone\n") ### DEBUG
    flush.console()

    return(g)

}
