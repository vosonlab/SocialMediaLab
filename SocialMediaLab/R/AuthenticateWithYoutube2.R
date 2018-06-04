#' Authenticate with YouTube API
#' 
#' YouTube API Authentication with R tuber package
#' DO NOT CALL DIRECTLY - USE \code{Authenticate}
#'
#' OAuth based authentication with the Google API via the tuber package:
#' http://soodoku.github.io/tuber/reference/yt_oauth.html
#'
#' @param oauth_client_ID character string specifying your Google Developer
#' OAuth Client ID
#' @param oauth_client_secret character string specifying your Google Developer
#' OAuth Client Secret
#' @param \dots additional arguments passed to \code{yt_oauth}
#' @return character string containing value for \code{google_token}
#' set by the tuber \code{yt_oauth} function
#' 
#' @note As per the tuber package documentation if using ubuntu the following
#' R console command may need to be run first:
#' \code{httr::set_config(config(ssl_verifypeer = 0L))}
#' Additional arguments may be passed to \code{yt_oauth}, such as:
#' \code{token=.httr-oauth} optional argument to set the tuber oauth token
#' cache file default file is \code{.httr-oauth} saved in working directory
#' \code{remove_old_oauth=FALSE} optional boolean argument to remove tuber
#' oauth cache file, the default value is \code{FALSE}
#'
#' @export
AuthenticateWithYoutube2 <- function(oauth_client_ID, oauth_client_secret, ...) {
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
