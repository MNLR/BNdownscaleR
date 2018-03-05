#' @export

queryJunction <- function( ev, predictors, junction, predictands) {
  evid <- setEvidence(junction, predictors, as.character(ev)) # Evidence must be provided as character
  return( querygrain(evid, nodes = predictands, type = "marginal") )
}
