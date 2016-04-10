#' Create networks from social media data
#'
#' This function creates network from social media data (i.e. from data frames
#' of class \code{dataSource}. \code{Create} is the final step of the
#' \code{Authenticate}, \code{Collect}, \code{Create} workflow.
#'
#' Note: when creating Facebook *bimodal* networks, the user information
#' is also collected and stored as vertex attributes (involving additional
#' calls to the Facebook API). However, this is not currently implemented
#' for Facebook *actor* networks.
#'
#' @param dataSource a data frame of class \code{dataSource}
#' @param type character, type of network to be created, currently supports
#' "actor", "bimodal", "dynamic", "semantic" and "ego"
#' @param ... additional parameters for Create*Network functions
#' @return An igraph graph object
#' @author Chung-hong Chan <chainsawtiney@@gmail.com>
#' @seealso \code{\link{CreateActorNetwork}},
#' \code{\link{CreateBimodalNetwork}}, \code{\link{CreateDynamicNetwork}},
#' \code{\link{CreateSemanticNetwork}}, \code{\link{CreateEgoNetworkFromData}}
#' @examples
#'
#' \dontrun{
#' require(magrittr)
#' ## Instagram ego network example
#' myAppID <- "123456789098765"
#' myAppSecret <- "abc123abc123abc123abc123abc123ab"
#' myUsernames <- c("senjohnmccain","obama")
#'
#' Authenticate("instagram",
#' appID = myAappId,
#' appSecret = myAppSecret) %>% Collect(ego = TRUE,
#' username = myUsernames) %>% Create
#'
#' ## YouTube actor network example
#' my_apiKeyYoutube <- "314159265358979qwerty"
#' videoIDs <- c("W2GZFeYGU3s","mL27TAJGlWc")
#'
#' Authenticate("youtube",
#' apiKey = my_apiKeyYoutube) %>% Collect(videoIDs = videoIDs) %>% Create('actor')
#' }
#' @export
Create <- function(dataSource, type = "Actor", ...) {
    if (inherits(dataSource, "ego")) {
        return(CreateEgoNetworkFromData(dataSource)) ## you cannot create actor out of ego data
    }
    creator <- switch(tolower(type),
                      actor = CreateActorNetwork,
                      bimodal = CreateBimodalNetwork,
                      dynamic = CreateDynamicNetwork,
                      semantic = CreateSemanticNetwork,
                      ego = CreateEgoNetworkFromData,
                      stop("Unknown Type")
                      )
    return(creator(dataSource, ...))
}
