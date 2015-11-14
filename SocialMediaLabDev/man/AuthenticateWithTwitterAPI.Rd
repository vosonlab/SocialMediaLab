\name{AuthenticateWithTwitterAPI}
\alias{AuthenticateWithTwitterAPI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Twitter API Authentication
}
\description{
Oauth based authentication with the Twitter API
}
\usage{
AuthenticateWithTwitterAPI(api_key, api_secret, access_token, access_token_secret, createToken)
}
\arguments{
  \item{api_key}{
character string specifying the 'API key' used for authentication.
}
  \item{api_secret}{
character string specifying the 'API secret' used for authentication.
}
  \item{access_token}{
character string specifying the 'access token' used for authentication.
}
  \item{access_token_secret}{
character string specifying the 'access token secret' used for authentication.
}
  \item{createToken}{
logical. !! NOT PROPERLY IMPLEMENTED YET.
}
}
\details{
In order to collect data from Twitter, the user must first authenticate with Twitter's Application Programming Interface (API).

This requires setting up an App on Twitter. An excellent guide to achieving this can be found at: http://thinktostart.com/twitter-authentification-with-r/
}
\value{
This is called for its side effect.
}
\author{
Timothy Graham <timothy.graham3@uq.net.au> & Robert Ackland <robert.ackland@anu.edu.au>
}
\seealso{
\code{AuthenticateWithFacebookAPI} and \code{AuthenticateWithYouTubeAPI} for other ways to collect social media data.
}
\examples{
\dontrun{
  # Firstly specify your API credentials
  my_api_key <- "1234567890qwerty"
  my_api_secret <- "1234567890qwerty"
  my_access_token <- "1234567890qwerty"
  my_access_token_secret <- "1234567890qwerty"

  AuthenticateWithTwitterAPI(api_key=my_api_key, api_secret=my_api_secret, access_token=my_access_token, access_token_secret=my_access_token_secret)
}
}
\keyword{ twitter }
\keyword{ social }
\keyword{ media }
\keyword{ SNA }
