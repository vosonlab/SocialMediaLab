AuthenticateWithInstagramAPI <-
function(appID, appSecret, useCachedToken) {

  # EnsurePackage("instaR")

  #if (missing(appId) | missing(appSecret)) {
  #  cat("Error. One or more API credentials arguments are missing.\nPlease specify these.")
  #  return()
  #}

  if (missing(useCachedToken)) {
# cat("\nuseCachedToken is missing, defaulting to none...") # DEBUG
    useCachedToken <- FALSE # default to not using cached token
  }

  if (useCachedToken=="TRUE" | useCachedToken=="true" | useCachedToken=="T" | useCachedToken==TRUE) {
# cat("\nuseCachedToken is TRUE...") # DEBUG
    useCachedToken <- TRUE # handling user input
  }

  if (useCachedToken) {
    if (file.exists("instagram_oauth_token")) {
cat("\nCached token was found (authentication will use the cached token).\n") # DEBUG
      load("instagram_oauth_token")
      return(instagram_oauth_token)
    }
    else {
      cat("\nOAuth token `instagram_oauth_token` not found. A token will be created and saved to working directory.\n")
    }
  }

  instagram_oauth_token <- instaOAuth(appID, appSecret)

  if (useCachedToken) {
    save(instagram_oauth_token, file="instagram_oauth_token")
  }

  return(instagram_oauth_token)

}
