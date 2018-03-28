#' @export

convertEvent <- function(Probability.Table, event = "1", threshold.vector = NULL) {
  if (is.list(Probability.Table)){ stop("Probability.Table is a list. Does it have members?") }
  if (is.null(threshold.vector)){ threshold.vector <- rep(0.5, dim(Probability.Table)[3]) }
  prediction.event <- t(apply(Probability.Table, is.mostLikelyEvent,  MARGIN = 1,  event = event, threshold.vector = threshold.vector) )
  colnames(prediction.event) <- colnames(Probability.Table[1,,])
  return( prediction.event )
}
