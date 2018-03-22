#' @export

measureVector <- function(data, station, measure = "phiCoef", ... ){  #remove.na = TRUE, evidence.nodes = NULL, evidence = NULL) {
  dS <- c()
  casesS <- c()
  for (j in 1:42){
    d <- do.call(measure, list(data = data, st1 = station, st2 = j, ...) )
    dS[j] <- d
    casesS[j] <- attributes(d)$cases
  }
  attr(dS, "cases") <- casesS[1]
  attr(dS, "measure") <- measure
  return(dS)
}
