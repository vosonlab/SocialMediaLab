Collect <- function(credential, ...) {
    collector <- switch(credential$datasource,
                        facebook = facebookCollector,
                        youtube = youtubeCollector,
                        twitter = twitterCollector,
                        instagram = instagramCollector,
                        stop("Unknown datasource")
                        )
    return(collector(credential, ...))
}

### *collector functions should not be exported. It is just a bunch of helper functions to bridge the CollectDataFrom* functions with Collect(), but with credential obj as the first argument

youtubeCollector <-
    function(credential, videoIDs, verbose, writeToFile, maxComments) {
        return(CollectDataYoutube(videoIDs, apiKeyYoutube = credential$auth, verbose, writeToFile, maxComments))
}

facebookCollector <-
    function(credential,pageName,rangeFrom,rangeTo,verbose,n,writeToFile,dynamic) {
        return(CollectDataFacebook(pageName,rangeFrom,rangeTo,verbose,n,writeToFile,dynamic, credential))
}

twitterCollector <- function(credential, searchTerm, numTweets, verbose, writeToFile, language) {
    return(CollectDataTwitter(searchTerm, numTweets, verbose, writeToFile, language)) # credential means nothing to twitteR
}

instagramCollector <- function(credential, tag, n, lat, lng, distance, folder, mindate, maxdate, verbose, sleep, writeToFile, waitForRateLimit) {
    return(CollectDataInstagram(tag, n, lat, lng, distance, folder, mindate, maxdate, verbose, sleep, writeToFile, waitForRateLimit, credential))
}
