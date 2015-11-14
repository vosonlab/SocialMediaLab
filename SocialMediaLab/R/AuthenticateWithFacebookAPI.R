AuthenticateWithFacebookAPI <-
function(appID, appSecret, extended_permissions, useCachedToken) {

  # EnsurePackage("Rfacebook")

  if (missing(extended_permissions)) {
    extended_permissions <- FALSE # default to not using extended permissions
  }

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
    if (file.exists("fb_oauth")) {
cat("\nCached fb_oauth token was found (using cached token).\n") # DEBUG
      load("fb_oauth")
      return(fb_oauth)
    }
    else {
      cat("\nOAuth token `fb_oauth` not found. A token will be created and saved to working directory.\n")
    }
  }

  fb_oauth <- fbOAuth(appID, appSecret, extended_permissions)

  if (useCachedToken) {
    save(fb_oauth, file="fb_oauth")
  }

  return(fb_oauth)

}
