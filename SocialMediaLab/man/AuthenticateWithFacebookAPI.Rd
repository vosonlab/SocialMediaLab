\name{AuthenticateWithFacebookAPI}
\alias{AuthenticateWithFacebookAPI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Facebook API Authentication
}
\description{
OAuth token based authentication with the Facebook API, with caching options for automatic authentication (i.e. avoid using the browser).
}
\usage{
AuthenticateWithFacebookAPI(appID, appSecret, extended_permissions, useCachedToken)
}
\arguments{
  \item{appID}{
character string specifying the 'App ID' of the Facebook app used for authentication.
}
  \item{appSecret}{
character string specifying the 'API Secret' associated with the Facebook App used for authentication.
}
  \item{extended_permissions}{
logical. If \code{TRUE} then behaves as described in package 'Rfacebook': the token will give access to some of the authenticated user's private information (birthday, hometown, location, relationships) and that of his/her friends, and permissions to post status updates as well as to access checkins, likes, and the user's newsfeed. If FALSE, token will give access only to public information. Note that updateStatus will only work for tokens with extended permissions.
}
  \item{useCachedToken}{
logical. If \code{TRUE} then this function will look for a saved token in the current working directory (name of token file must be \code{fb_oauth}). If \code{fb_oauth} token is not found, then it will create a token and save it to current working directory (i.e. for future use).
}
}
\details{
In order to collect data from Facebook, the user must first authenticate with Facebook's Application Programming Interface (API). Furthermore, the user must create a Facebook 'app' and get an 'app secret'.

To get a Facebook 'app ID' and 'API secret', the excellent tutorial at http://thinktostart.com/analyzing-facebook-with-r/ provides more information.

One problem with Facebook authentication through R is that it normally requires the user to authenticate using their browser each time they wish to collect data. The \code{useCachedToken} argument provides a way to circumvent this, by saving and loading an authenticated 'token' file stored in the working directory. If the \code{useCachedToken} argument is set to \code{TRUE}, then the browser is not necessary for future sessions.
}
\value{
An OAuth access token that enables R to make authenticated calls to the Facebook API.
}
\author{
Timothy Graham <timothy.graham3@uq.net.au> & Robert Ackland <robert.ackland@anu.edu.au>
}
\seealso{
\code{AuthenticateWithTwitterAPI} and \code{AuthenticateWithYouTubeAPI} for other ways to collect social media data.
}
\examples{
\dontrun{
  ## Use your own values for myAppID and myAppSecret
  myAppID <- "123456789098765"
  myAppSecret <- "abc123abc123abc123abc123abc123ab"

  # Authenticate with the Facebook API using `AuthenticateWithFacebookAPI`
  fb_oauth <- AuthenticateWithFacebookAPI(appID=myAppID, appSecret=myAppSecret,extended_permissions=FALSE, useCachedToken=TRUE)
  }
}
\keyword{ facebook }
\keyword{ social }
\keyword{ media }
\keyword{ SNA }
