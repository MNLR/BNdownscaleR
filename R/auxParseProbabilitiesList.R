#' @export

auxParseProbabilitiesList <- function(PT, predictands, prediction.type, threshold.vector, marginals, event){
  tPT <- lapply( PT, function(ELPT) {
    downscaled <- aperm(simplify2array( sapply(ELPT, simplify2array, simplify = FALSE),
                                        higher = TRUE ) , c(3,1,2)
    )
    ELPT <- downscaled[,,match(predictands, colnames(downscaled[1,,]))]
    if ( prediction.type == "event" ){
      return( convertEvent(ELPT, event = event, threshold.vector =  threshold.vector, marginals = marginals) )
    }
    else {
      return(ELPT)
    }
  }
  )
  return( tPT)
}
