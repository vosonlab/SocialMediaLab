CreateDynamicNetwork.facebook <-
function(x,writeToFile)
{

  if (missing(writeToFile)) {
    writeToFile <- FALSE # default = not write to file
  }

  dataCombinedUNIQUE <- x # match the variable names (this must be used to avoid warnings in package compilation)

  # make sure user is not trying to create a dynamic network,
  # i.e. using data that is not temporal

  if (!inherits(dataCombinedUNIQUE,"temporal")) {
    cat("\nERROR. Dynamic networks require dynamic data. Use the 'dynamic=TRUE' argument when collecting data, e.g. when calling the CollectDataFacebook function.\n")
    return()
  }

  # EnsurePackage("igraph")

  cat("\nCreating Facebook dynamic network...\n")

  # make a vector of all the unique actors in the network1
  usersVec <- rep(c("User"),length(unique(dataCombinedUNIQUE$from)))
  postsVec <- rep(c("Post"),length(unique(dataCombinedUNIQUE$to)))
  usersAndPostsVec <- c(usersVec,postsVec)
  actors <- data.frame(name=unique(factor(c(as.character(unique(dataCombinedUNIQUE$from)),as.character(unique(dataCombinedUNIQUE$to))))),type=usersAndPostsVec)

  # actorsNames <- unique(factor(c(as.character(unique(dataCombinedUNIQUE$from)),as.character(unique(dataCombinedUNIQUE$to)))))

  # make a dataframe of the relations between actors
  # we need a dataframe here because igraph needs it AFAIK

  relations <- data.frame(from=dataCombinedUNIQUE$from,to=dataCombinedUNIQUE$to,relationship=dataCombinedUNIQUE$relationship,weight=dataCombinedUNIQUE$edgeWeight,timestamp=dataCombinedUNIQUE$commentTimestamp,timestampNumeric=dataCombinedUNIQUE$commentTimestampConverted,timestampUnixEpoch=dataCombinedUNIQUE$commentTimestampUnixEpoch)

  ##### STEP FOUR #####

  # convert into a graph
  g <- graph.data.frame(relations, directed=TRUE, vertices=actors)

  # Make the node labels play nice with Gephi
  V(g)$label <- V(g)$name

  if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
    # Output the final network to a graphml file, to import directly into Gephi
    currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
    currTime <- gsub(":","_",currTime)
    write.graph(g,paste0(currTime,"_FacebookDynamicBimodalNetwork.graphml"),format="graphml")
    cat("Facebook dynamic bimodal network was written to current working directory, with filename:\n")
    cat(paste0(currTime,"_FacebookDynamicBimodalNetwork.graphml"))
  }

  cat("\nDone!\n") ### DEBUG
  flush.console()

  return(g)

}
