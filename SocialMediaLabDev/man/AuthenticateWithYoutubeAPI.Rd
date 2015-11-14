\name{AuthenticateWithYoutubeAPI}
\alias{AuthenticateWithYoutubeAPI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
YouTube API Authentication
}
\description{
OAuth based authentication with the Google API
}
\usage{
AuthenticateWithYoutubeAPI(apiKeyYoutube)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{apiKeyYoutube}{
character string specifying your Google Developer API key.
}
}
\details{
In order to collect data from YouTube, the user must first authenticate with Google's Application Programming Interface (API). Users can obtain a Google Developer API key at: https://console.developers.google.com
}
\value{
This is called for its side effect.
}
\author{
Timothy Graham <timothy.graham3@uq.net.au> & Robert Ackland <robert.ackland@anu.edu.au>
}
\note{
In the future this function will enable users to save the API key in working directory, and the function will automatically look for a locally stored key whenever it is called without \code{apiKeyYoutube} argument.
}
\seealso{
\code{AuthenticateWithFacebookAPI} and \code{AuthenticateWithTwitterAPI} for other ways to collect social media data.
}
\examples{
\dontrun{
  # Replace with your Google Developer API Key:
  my_apiKeyYoutube <- "314159265358979qwerty"

  apiKeyYoutube <- AuthenticateWithYoutubeAPI(my_apiKeyYoutube)
}
}
\keyword{ youtube }
\keyword{ social }
\keyword{ media }
\keyword{ SNA }
