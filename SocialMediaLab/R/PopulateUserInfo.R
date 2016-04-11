#' @export
PopulateUserInfo <- function(network_object) {
  # require(data.table)
  # require(plyr)

  # get the list of users
  listOfUsers <- unique(V(network_object)$name)

  # predefine a data table to store the results (later delete unneeded rows)
  actors <- data.table(
    name=as.character(c(rep("NA_f00",length(listOfUsers)))),
    userDescription=as.character(c(rep("NA_f00",length(listOfUsers)))),
    statusesCount=as.character(c(rep("NA_f00",length(listOfUsers)))),
    followersCount=as.character(c(rep("NA_f00",length(listOfUsers)))),
    favoritesCount=as.character(c(rep("NA_f00",length(listOfUsers)))),
    friendsCount=as.character(c(rep("NA_f00",length(listOfUsers)))),
    url=as.character(c(rep("NA_f00",length(listOfUsers)))),
    realName=as.character(c(rep("NA_f00",length(listOfUsers)))),
    dateAccountCreated=as.character(c(rep("NA_f00",length(listOfUsers)))),
    userLocation=as.character(c(rep("NA_f00",length(listOfUsers)))),
    userLanguage=as.character(c(rep("NA_f00",length(listOfUsers)))),
    numberOfListsUserIsFeaturedOn=as.character(c(rep("NA_f00",length(listOfUsers)))),
    profileImageUrl=as.character(c(rep("NA_f00",length(listOfUsers))))
    )

    setkey(actors,name) # set the key value of the data table

    nextEmptyRow <- 1 # so we can update rows in `dataCombined` in a relatively efficient way

      # This function is supposed to perform the lookups in batches
      # and mind the rate limit:
      getUserObjects <- function(users) {
        groups <- split(users, ceiling(seq_along(users)/50))
        userObjects <- ldply(groups, function(group) { # ldply is a very cool function, found in plyr package.
          objects <- lookupUsers(group, includeNA=TRUE)
          out <- twListToDF(objects) # twListToDF is also a handy function, found in twitteR package. Converts weird class object to data frame.
            # print("Waiting for 15 minutes (to 'refresh' the rate limit)...") # Don't need to use this yet. Implement later for number of users > 7500 (have to do chunked batches... chunks of chunks... urrghh)
            # Sys.sleep(900)
          return(out)
        })
        return(userObjects)
      }

    # Collect user data (will return NA for users who don't exist)

      # query the user data
      cat("\n Fetching the user data...\n") # DEBUG
      usersInformationAttributes <- getUserObjects(listOfUsers)
      actorsInfoDF <- usersInformationAttributes

      actors$name <- actorsInfoDF$screenName
      actors$userDescription <- actorsInfoDF$description
      actors$statusesCount <- actorsInfoDF$statusesCount
      actors$followersCount <- actorsInfoDF$followersCount
      actors$favoritesCount <- actorsInfoDF$favoritesCount
      actors$friendsCount <- actorsInfoDF$friendsCount
      actors$url <- actorsInfoDF$url
      actors$realName <- actorsInfoDF$name
      actors$dateAccountCreated <- actorsInfoDF$created
      actors$userLocation <- actorsInfoDF$location
      actors$userLanguage <- actorsInfoDF$lang
      actors$numberOfListsUserIsFeaturedOn <- actorsInfoDF$listedCount
      actors$profileImageUrl <- actorsInfoDF$profileImageUrl

  # the final thing to do is apply the values in `actors` to the network_object

  V(network_object)$screenName <- actors$name
  V(network_object)$userDescription <- actors$userDescription
  V(network_object)$statusesCount <- actors$statusesCount
  V(network_object)$followersCount <- actors$followersCount
  V(network_object)$favoritesCount <- actors$favoritesCount
  V(network_object)$friendsCount <- actors$friendsCount
  V(network_object)$url <- actors$url
  V(network_object)$realName <- actors$realName
  V(network_object)$dateAccountCreated <- actors$dateAccountCreated
  V(network_object)$userLocation <- actors$userLocation
  V(network_object)$userLanguage <- actors$userLanguage
  V(network_object)$numberOfListsUserIsFeaturedOn <- actors$numberOfListsUserIsFeaturedOn
  V(network_object)$profileImageUrl <- actors$profileImageUrl

  return(network_object)
}
