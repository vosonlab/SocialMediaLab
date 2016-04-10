#' Note: this function is DEPRECATED and will be removed in a future release.
#' Please use the \code{Authenticate} function
#'
#' Facebook API Authentication
#'
#' OAuth token based authentication with the Facebook API, with caching options
#' for automatic authentication (i.e. avoid using the browser).
#'
#' In order to collect data from Facebook, the user must first authenticate
#' with Facebook's Application Programming Interface (API). Furthermore, the
#' user must create a Facebook 'app' and get an 'app secret'.
#'
#' To get a Facebook 'app ID' and 'API secret', the excellent tutorial at
#' http://thinktostart.com/analyzing-facebook-with-r/ provides more
#' information.
#'
#' One problem with Facebook authentication through R is that it normally
#' requires the user to authenticate using their browser each time they wish to
#' collect data. The \code{useCachedToken} argument provides a way to
#' circumvent this, by saving and loading an authenticated 'token' file stored
#' in the working directory. If the \code{useCachedToken} argument is set to
#' \code{TRUE}, then the browser is not necessary for future sessions.
#'
#' @param appID character string specifying the 'App ID' of the Facebook app
#' used for authentication.
#' @param appSecret character string specifying the 'API Secret' associated
#' with the Facebook App used for authentication.
#' @param extended_permissions logical. If \code{TRUE} then behaves as
#' described in package 'Rfacebook': the token will give access to some of the
#' authenticated user's private information (birthday, hometown, location,
#' relationships) and that of his/her friends, and permissions to post status
#' updates as well as to access checkins, likes, and the user's newsfeed. If
#' FALSE, token will give access only to public information. Note that
#' updateStatus will only work for tokens with extended permissions.
#' @param useCachedToken logical. If \code{TRUE} then this function will look
#' for a saved token in the current working directory (name of token file must
#' be \code{fb_oauth}). If \code{fb_oauth} token is not found, then it will
#' create a token and save it to current working directory (i.e. for future
#' use).
#' @return An OAuth access token that enables R to make authenticated calls to
#' the Facebook API.
#' @author Timothy Graham <timothy.graham3@@uq.net.au> & Robert Ackland
#' <robert.ackland@@anu.edu.au>
#' @seealso \code{AuthenticateWithTwitterAPI} and
#' \code{AuthenticateWithYouTubeAPI} for other ways to collect social media
#' data.
#' @keywords facebook social media SNA
#' @examples
#'
#' \dontrun{
#'   ## Use your own values for myAppID and myAppSecret
#'   myAppID <- "123456789098765"
#'   myAppSecret <- "abc123abc123abc123abc123abc123ab"
#'
#'   # Authenticate with the Facebook API using `AuthenticateWithFacebookAPI`
#'   fb_oauth <- AuthenticateWithFacebookAPI(appID=myAppID, appSecret=myAppSecret,
#'     extended_permissions=FALSE, useCachedToken=TRUE)
#'   }
#' @export
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
