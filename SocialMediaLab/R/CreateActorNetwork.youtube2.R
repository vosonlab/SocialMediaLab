# accepts video author dataframe, tuber comments dataframe, actors dataframe
# returns dataframe of actor comment network
create_actor_network <- function(video_comments, video_creators, actors, directIsolates) {
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

#' @export
CreateActorNetwork.youtube2 <- function(commentsData, directIsolates=TRUE, writeToFile=FALSE) {
  comment_actors <- data.frame(x=numeric(0), y=numeric(0))
  video_creators <- data.frame(videoId=character(0), authorChannelId.value=character(0), authorDisplayName=character(0))
  
  if (directIsolates) {
    # create a reference data frame for video id, video creator channel id and display name 
    video_creators <- subset(commentsData, select=c("videoId"), stringsAsFactors=FALSE) %>% distinct(videoId) 
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
                               group_by(authorChannelId.value, authorDisplayName) %>% summarise(comments = n()) %>% ungroup()
                               #distinct(authorChannelId.value, .keep_all=TRUE)
  
  # dataframe of weighted actor edges
  video_comment_actor_network_weighted <- create_actor_network(commentsData, video_creators, video_comment_actors, directIsolates) %>%
    group_by(authorChannelId.value, commentToAuthorChannelId) %>% summarise(weight = n()) %>% ungroup()  # authorChannelId.value.x
  
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
    output_file_name <- paste0(format(Sys.time(), "%a_%b_%d_%H_%M_%S_%Y_%Z"), "_YoutubeActorNetwork2.graphml")
    write_graph(g, output_file_name, format=c("graphml"))
    
    cat("YouTube actor network was written to current working directory, with filename:\n")
    cat(output_file_name)
  } 
  
  return(g)
}
