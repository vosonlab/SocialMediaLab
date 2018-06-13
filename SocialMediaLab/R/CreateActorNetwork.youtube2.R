#' Creates an actor network with comments as edges (not weighted)
#' 
#' @param video_comments data frame of comment data collected by 
#' \code{CollectDataYoutube2}
#' @param video_creators data frame of id and names of creators of the 
#' videoID's. Used to direct top level comments to the video creator actor.
#' @param actors data frame of unique actors ids and names from comments data.
#' Used as reference to create the network with ids and names.
#' @param directIsolates 
#' @return actor_network data frame of the comments edge network
#' @noRd
CreateActorEdgeNetwork <- function(video_comments, video_creators, actors, directIsolates) {
  actor_network <- subset(video_comments, select=c("videoId", "id", "parentId", "authorDisplayName", "authorChannelId.value"), stringsAsFactors=FALSE)
  
  # fields for author id and display name that a comment is directed to
  actor_network$commentToAuthorChannelId <- NA
  actor_network$commentToAuthorDisplayName <- NA
  
  # authorChannelId.value.y and authorDisplayName.y will be the video creator after merge
  actor_network <- merge(x=actor_network, y=video_creators, by="videoId", all=TRUE)
  
  actor_network <- plyr::rename(actor_network, c("authorChannelId.value.x" = "authorChannelId.value", 
                                                 "authorDisplayName.x" = "authorDisplayName"))
  
  for (row in 1:nrow(actor_network)) {
  
    # if parentId is NA it should be a top level comment
    if (is.na(actor_network[row, "parentId"])) {
      
      # if a comment has no parent comment and we want to direct isolates then direct comment to the the video author
      if (directIsolates) {
        if (!is.na(actor_network[row, "authorChannelId.value.y"])) {
          actor_network[row, "commentToAuthorChannelId"] <- actor_network[row, "authorChannelId.value.y"]
        }
      }
    } else {
      # set commentToAuthorChannelId to channel id of the actor comment is directed to
      actor_network[row, "commentToAuthorChannelId"] <- actor_network[which(actor_network$id == actor_network[row, "parentId"]), 
                                                                      "authorChannelId.value"] # authorChannelId.value.x
    }
    
    # set commentToAuthorDisplayName to display name of the actor comment is directed to
    if (!is.na(actor_network[row, "commentToAuthorChannelId"])) {
      author_name <- ''
      author_name <- actors[which(actors$authorChannelId.value == actor_network[row, "commentToAuthorChannelId"]), "authorDisplayName"]
      if (!is.null(author_name) && length(author_name) > 0) {
        actor_network[row, "commentToAuthorDisplayName"] <- author_name 
      }      
    }
  }
  
  return(actor_network)
}

#' Create YouTube Actor Network
#' 
#' Creates a Unimodal Actor Network from YouTube Video Comments
#' DO NOT CALL DIRECTLY - USE \code{Create}
#'
#' Creates a unimodal actor network based on comments and replies to one or
#' more youtube videos.
#'
#' @param commentsData data frame containing comments data collected and
#' structured by \code{CollectDataYoutube2}.
#' @param directIsolates boolean, if \code{TRUE} top level comments will be directed
#' to the creator of the video that the actors are commenting on. If \code{FALSE}
#' top level comments are not directed and actors will be isolates if they have
#' not replied to any comments by other actors.Default is \code{TRUE}.
#' @param writeToFile boolean, if \code{TRUE} then igraph data is saved to a 
#' file in the current working directory in \code{graphml} format. The file
#' name will be the current system time and network type as
#' \code{MMM_DD_HH_MM_SS_YYYY_YoutubeActorNetwork2.graphml}. Default
#' is \code{FALSE}.
#' @return igraph object containing the actor network
#' 
#' @note \code{directIsolates=TRUE} option will use additional
#' Google API resource points as it makes a video details request for 
#' each youtube video that comments were collected from. As such this
#' option will also require \code{AuthenticateWithYoutube2} to be
#' called first. Otherwise set this to \code{FALSE}.
#' @seealso \code{CollectDataYoutube2} must be called first to collect an
#' appropriately structured data frame.
#'
#' @export
CreateActorNetwork.youtube2 <- function(commentsData, directIsolates=TRUE, writeToFile=FALSE) {
#CreateActorNetwork.youtube2 <- function(x, writeToFile) {
    
  comment_actors <- data.frame(x=numeric(0), y=numeric(0))
  video_creators <- data.frame(videoId=character(0), authorChannelId.value=character(0), authorDisplayName=character(0))
  
  if (directIsolates) {
    # create a reference data frame for video id, video creator channel id and display name 
    video_creators <- subset(commentsData, select=c("videoId"), stringsAsFactors=FALSE) %>% dplyr::distinct(rlang::.data$videoId) # unique()
    video_creators$authorChannelId.value <- NA
    video_creators$authorDisplayName <- NA
    
    for (row in 1:nrow(video_creators)) {
      # makes an api request for video details and returns the video creator id and name
      video_author <- GetYoutubeVideoAuthor(video_creators[row, "videoId"])
      
      video_creators[row, "authorChannelId.value"] <- video_author[which(video_author$videoId == video_creators[row, "videoId"]), 
                                                                   "authorChannelId.value"]
      video_creators[row, "authorDisplayName"] <- video_author[which(video_author$videoId == video_creators[row, "videoId"]), 
                                                               "authorDisplayName"]
    }
  }
  
  # dataframe of actor ids and display names, ensures videos creator included if directIsolates
  # includes number of comments for use as a vertex attribute
  video_comment_actors <- rbind(subset(commentsData, select=c("authorChannelId.value", "authorDisplayName"), stringsAsFactors=FALSE), 
                               subset(video_creators, select=c("authorChannelId.value", "authorDisplayName"), stringsAsFactors=FALSE)) %>%
                               dplyr::group_by(rlang::.data$authorChannelId.value, rlang::.data$authorDisplayName) %>% 
                               dplyr::summarise(comments = dplyr::n()) %>% dplyr::ungroup()
                               #distinct(authorChannelId.value, .keep_all=TRUE)
  
  # dataframe of weighted actor edges
  video_comment_actor_network_weighted <- CreateActorEdgeNetwork(commentsData, video_creators, video_comment_actors, directIsolates) %>%
    dplyr::group_by(rlang::.data$authorChannelId.value, rlang::.data$commentToAuthorChannelId) %>% dplyr::summarise(weight = dplyr::n()) %>% dplyr::ungroup()  # authorChannelId.value.x
  
  # igraph
  
  # vertices that appear in the vertices= argument will be added to the graph even if they have no edges
  # if directIsolates=FALSE need to remove them from network to avoid igraph errors
  video_comment_actor_network_weighted <- na.omit(video_comment_actor_network_weighted)
  
  # create igraph object
  g <- graph_from_data_frame(video_comment_actor_network_weighted, directed=TRUE, vertices=video_comment_actors)
  
  V(g)$label <- video_comment_actors$authorDisplayName
  E(g)$weight <- video_comment_actor_network_weighted$weight
  
  if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
    # output the final network to a graphml file, to import directly into gephi
    output_file_name <- paste0(format(Sys.time(), "%b_%d_%H_%M_%S_%Y"), "_YoutubeActorNetwork2.graphml")
    write_graph(g, output_file_name, format=c("graphml"))
    
    cat("YouTube actor network was written to current working directory, with filename:\n")
    cat(output_file_name)
  } 
  
  return(g)
}
