\name{CollectDataFacebook}
\alias{CollectDataFacebook}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Collect data from Facebook pages for generating different types of networks
}
\description{
This function collects data from Facebook pages (i.e. post data and comments/likes data within posts), and structures the data into a data frame of class \code{dataSource.facebook}, ready for creating networks for further analysis.
}
\usage{
CollectDataFacebook(pageName, rangeFrom, rangeTo, verbose, n, writeToFile, dynamic)
}
\arguments{
  \item{pageName}{
character string, specifying the name of the Facebook page. For example, if page is: https://www.facebook.com/StarWars, then \code{pageName="StarWars".}
}
  \item{rangeFrom}{
character string, specifying a 'start date' for data collection, in the format YYYY-MM-DD. For example, to collect data starting from July 4th 2015, \code{rangeFrom} would be "2015-07-04". Default value is current system date minus one week (i.e. the date 7 days ago).
}
  \item{rangeTo}{
character string, specifying an 'end date' for data collection, in the format YYYY-MM-DD. For example, to collect data until December 25th 2015, \code{rangeFrom} would be "2015-12-25". Default value is the current system date.
}
  \item{verbose}{
logical. If \code{TRUE} then this function will output runtime information to the console as it computes. Useful diagnostic tool for long computations. Default is \code{FALSE}.
}
  \item{n}{
numeric, maximum number of comments and likes to return (see \code{getPost} in Rfacebook package). Default value is 1000.
}
  \item{writeToFile}{
logical. If \code{TRUE} then the data is saved to file in current working directory (CSV format), with filename denoting \code{rangeFrom}, \code{rangeTo}, and \code{pageName}.
}
  \item{dynamic}{
logical. If \code{TRUE} then temporal data will be collected, which can be used to create dynamic networks (using function \code{CreateDynamicNetwork}). Note: Facebook API does not currently provide timestamp data for 'likes' on posts. Therefore, if dynamic is set to \code{TRUE}, then only 'comments' data are collected. In this way, an edge from user i to post j represents whether (and how many times) user i has commented on post j.
  }
}
\details{
\code{CollectDataFacebook} collects public 'post' data from a given Facebook page, including comments and 'likes' from within each post.

The function then finds and maps the relationships between users and posts, and structures these relationships into a format suitable for creating bimodal networks using \code{CreateBimodalNetwork}.

A date range must be specified for collecting post data using \code{rangeFrom} and \code{rangeTo} (i.e. data will be collected from posts posted within the date range). If no date range is supplied, then the default is the current system date minus one week (i.e. 7 days leading up to current system date).
}
\value{
A data frame object of class \code{dataSource.facebook} that can be used with \code{CreateBimodalNetwork}.
}
\author{
Timothy Graham <timothy.graham3@uq.net.au> & Robert Ackland <robert.ackland@anu.edu.au>
}
\note{
Currently supported network types:

- bimodal networks; \code{CreateBimodalNetwork}
- dynamic bimodal networks; \code{CreateDynamicNetwork}

Note: dynamic networks created using Facebook data are bimodal. This means that there are two types of vertices present in the network (i.e. Facebook users and Facebook posts), with edges representing the time(s) when user i commented on post j. Currently, timestamp data is not available through the Facebook API for 'likes' data (i.e. when user i 'likes' post j), so relationships based on 'likes' are excluded from dynamic Facebook data (and therefore networks generated using \code{CreateDynamicNetwork}).
}
\seealso{
\code{AuthenticateWithFacebookAPI} must be run first or no data will be collected, \code{collectTemporalDataFacebook} is used to collect temporal data suitable for dynamic networks (i.e. Facebook network that changes as a function of time).
}
\examples{
\dontrun{
  ## Use your own values for myAppID and myAppSecret
  myAppID <- "123456789098765"
  myAppSecret <- "abc123abc123abc123abc123abc123ab"

  # Authenticate with the Facebook API using `AuthenticateWithFacebookAPI`
  fb_oauth <- AuthenticateWithFacebookAPI(appID=myAppID, appSecret=myAppSecret,extended_permissions=FALSE, useCachedToken=TRUE)

  # Run the `CollectDataFacebook` function and store the results in variable `myFacebookData`
  myFacebookData <- CollectDataFacebook(pageName="StarWars", rangeFrom="2015-05-01",rangeTo="2015-06-03",writeToFile=FALSE,verbose=TRUE)

  # Create a 'bimodal' network using \code{CreateBimodalNetwork}
  g_bimodal_facebook <- CreateBimodalNetwork(myFacebookData)

  # View descriptive information about the bimodal network
  g_bimodal_facebook
}
}
\keyword{ facebook }
\keyword{ data }
\keyword{ mining }
\keyword{ SNA }
