Create <- function(dataSource, type = "Actor", ...) {
    creator <- switch(type,
                      Actor = CreateActorNetwork,
                      Bimodal = CreateBimodalNetwork,
                      Dynamic = CreateDynamicNetwork,
                      Semantic = CreateSemanticNetwork,
                      stop("Unknown Type")
                      )
    return(creator(dataSource, ...))
}
