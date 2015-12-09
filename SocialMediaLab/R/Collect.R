Collect <- function(credential, ...) {
    collector <- switch(credential$datasource,
                        facebook = AuthenticateWithFacebookAPI,
                        youtube = youtubeCollector,
                        twitter = AuthenticateWithTwitterAPI,
                        instagram = AuthenticateWithInstagram,
                        stop("Unknown datasource")
                        )
    return(collector(credential, ...))
}

### *collector functions should not be exported. It is just a bunch of helper functions to bridge the CollectDataFrom* functions with Collect(), but with credential obj as the first argument

youtubeCollector <-
    function(credential, videoIDs, verbose, writeToFile, maxComments) {
        return(CollectDataYoutube(videoIDs, apiKeyYoutube = credential$auth, verbose, writeToFile, maxComments))
}
