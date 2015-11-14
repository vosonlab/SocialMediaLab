AuthenticateWithTwitterAPI <-
function(api_key, api_secret, access_token, access_token_secret, createToken) {

  # EnsurePackage("tm") # we only load packages as required (i.e. if user authenticate with twitter, then we load packages for twitter data collection/analysis)
  # EnsurePackage("stringr")
  # EnsurePackage("twitteR")
  # EnsurePackage("RCurl")
  # EnsurePackage("bitops")
  # EnsurePackage("rjson")
  # EnsurePackage("plyr")
  # EnsurePackage("igraph")

  if (missing(api_key) | missing(api_secret) | missing(access_token) | missing(access_token_secret)) {
    cat("Error. One or more API credentials arguments are missing.\nPlease specify these. \n")
    return()
  }

  # We avoid the popup prompt about cached authentication,
  # and instead include a `createToken` argument in the function,
  # and directly set the options parameter for the "httr" package.
  # (And default to no token if the argument is missing)

  origOptions <- options("httr_oauth_cache") # original options setting

  if (missing(createToken)) {
    createToken <- FALSE # default to no token
  }

  if (createToken=="TRUE" | createToken=="true" | createToken=="T" | createToken==TRUE) {
    createToken <- TRUE # handling user input
  }

  if (createToken) {
    options(httr_oauth_cache=T)
  }
  else {
    options(httr_oauth_cache=F)
  }

  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

  options(httr_oauth_cache=origOptions) # reset options back to the original setting

  return()

}
