GetYoutubeVideoIDs <-
function(file){

  videoIDsTemp <- read.table(file,
                 sep="\n",
                 strip.white=TRUE) # in case of user input error

  videoIDsTemp <- as.vector(videoIDsTemp$V1)

  videoIDsOut <- substr(videoIDsTemp,33,43)

}
