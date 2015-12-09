Create <- function(dataSource, type = "Actor", ...) {
    creator <- switch(tolower(type),
                      actor = CreateActorNetwork,
                      bimodal = CreateBimodalNetwork,
                      dynamic = CreateDynamicNetwork,
                      semantic = CreateSemanticNetwork,
                      stop("Unknown Type")
                      )
    return(creator(dataSource, ...))
}
