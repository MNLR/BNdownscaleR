#' @export

purgeBinAbsence <- function(probabilities){
  dp <- dim(probabilities)
  if (length(dp) != 3) {stop("Invalid format.")}
  if (length(dp == 3) && dp[2]!=2){stop("Data is not binary!") }
  purged <- t(apply(probabilities, MARGIN = 1, FUN = function(observ) {observ["1", ]}))
  return(purged)
}
