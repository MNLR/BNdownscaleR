#' @export

convertEvent <- function(Probability.Table, event = "1", threshold.vector = NULL, marginals) {

  if (is.list(Probability.Table)){ stop("Probability.Table is a list. Does it have members?") }

  if (is.character(threshold.vector) && threshold.vector == "climatologic" && !is.null(marginals)) {
    threshold.vector <- 1 - marginals[event, ]
  } else if ( is.character(threshold.vector) && threshold.vector == "marginal" && !is.null(marginals)){
    threshold.vector <- marginals[event, ]
  }
  else { threshold.vector <- rep(0.5, dim(Probability.Table)[length(dim(Probability.Table))]) }

  prediction.event <- t(apply(Probability.Table, is.mostLikelyEvent,  MARGIN = 1,  event = event, threshold.vector = threshold.vector) )
  colnames(prediction.event) <- colnames(Probability.Table[1,,])
  return( prediction.event )
}
