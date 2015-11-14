CreateBimodalNetwork.facebook <-
function(x,writeToFile,removeTermsOrHashtags, ...)
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

  dataCombinedUNIQUE <- x # match the variable names (this must be used to avoid warnings in package compilation)

  # Warn the user if they are trying to create a bimodal network
  # using TEMPORAL data (i.e. it might work, but could be compatibility issues)

  if (inherits(dataCombinedUNIQUE,"temporal")) {
    cat("\nERROR. Attempting to use dynamic data to create bimodal network. Please use the 'dynamic=FALSE' argument when collecting data.\n")
    return()
  }

  #EnsurePackage("igraph")

  cat("\nCreating Facebook bimodal network...\n")

  # make a vector of all the unique actors in the network1
  usersVec <- rep(c("User"),length(unique(dataCombinedUNIQUE$from)))
  postsVec <- rep(c("Post"),length(unique(dataCombinedUNIQUE$to)))
  usersAndPostsVec <- c(usersVec,postsVec)
  actors <- data.frame(name=unique(factor(c(as.character(unique(dataCombinedUNIQUE$from)),as.character(unique(dataCombinedUNIQUE$to))))),type=usersAndPostsVec)

  # make a dataframe of the relations between actors
  # we need a dataframe here because igraph needs it AFAIK

  relations <- data.frame(from=dataCombinedUNIQUE$from,to=dataCombinedUNIQUE$to,relationship=dataCombinedUNIQUE$relationship,weight=dataCombinedUNIQUE$edgeWeight)

  # construct a graph
  g <- graph.data.frame(relations, directed=TRUE, vertices=actors)

  # Make the node labels play nice with Gephi
  V(g)$label <- V(g)$name

  if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
    # Output the final network to a graphml file, to import directly into Gephi
    currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
    currTime <- gsub(":","_",currTime)
    write.graph(g,paste0(currTime,"_FacebookBimodalNetwork.graphml"),format="graphml")
    cat("Facebook bimodal network was written to current working directory, with filename:\n")
    cat(paste0(currTime,"_FacebookBimodalNetwork.graphml"))
  }

  cat("\nDone!\n") ### DEBUG
  flush.console()

  return(g)

}
