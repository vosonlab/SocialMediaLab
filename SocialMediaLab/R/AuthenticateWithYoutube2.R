#' YouTube2 Authentication
#'
#' OAuth based authentication with the Google API using the R tuber package:
#' http://soodoku.github.io/tuber/
#'
#' In order to collect data from YouTube, the user must first authenticate with
#' Google's Application Programming Interface (API). Users can obtain a Google
#' Developer OAuth Client ID and Client Secret at:
#' https://console.developers.google.com
#'
#' @param oauth_client_ID character string specifying your Google Developer 
#' OAuth Client ID
#' @param oauth_client_secret character string specifying your Google Developer
#' OAuth Client Secret
#' @return character string containing option value for \code{google_token}
#' set by the tuber \code{yt_oauth} function
#' @note As per the tuber package documentation if using ubuntu the following 
#' R console command may need to be run first:
#' \code{httr::set_config(config(ssl_verifypeer = 0L))}
#' \code{token=.httr-oauth} optional argument to set the tuber oauth token cache file
#' default file is \code{.httr-oauth} saved in working directory
#' \code{remove_old_oauth=FALSE} optional boolean argument to remove tuber oauth
#' cache file, the default value is \code{FALSE}
#' @seealso \code{AuthenticateWithFacebookAPI} and
#' \code{AuthenticateWithTwitterAPI} for other ways to collect social media
#' data.
#' @keywords youtube tuber social media SNA
#' @examples
#'
#' \dontrun{
#'   # Replace with your Google Developer OAuth Client ID and Secret:
#'   myOAuthClientID <- "1456-123abd78ef.apps.googleusercontent.com"
#'   myOAuthClientSecret <- "Abc1D2ef345-G678hI"
#'
#'   oauthTokenYoutube <- AuthenticateWithYoutube2(
#'                          oauth_client_ID = myOAuthClientID,
#'                          oauth_client_secret = myOAuthClientSecret)
#' }
#' @export
AuthenticateWithYoutube2 <-
  function(oauth_client_ID, oauth_client_secret, ...) {
    # if using ubuntu the following may need to be run first:
    # httr::set_config( config( ssl_verifypeer = 0L ) )
    
    # yt_oauth checks for default .httr-oauth in the working directory or 
    # requires oauth client id and client secret
    # remove the existing .httr-oauth by setting remove_old_oauth to TRUE 
    # yt_oauth launches a browser to authorize the new client
    yt_oauth(oauth_client_ID, oauth_client_secret, ...)
    
    # only returns the google authentication token for consistency,
    # it is managed for us by tuber
    return(getOption("google_token"))
  }