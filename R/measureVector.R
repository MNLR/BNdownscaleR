#' @export

measureVector <- function(data, station, measure = "phiCoef", ... ){  #remove.na = TRUE, evidence.nodes = NULL, evidence = NULL) {

  if (is.matrix(data) | is.data.frame(data)){
    nstations <- ncol(data)
  } else {
    nstations <- ncol(data$Data)
  }

  dS <- c()
  casesS <- c()

  for (j in 1:nstations){
    d <- do.call(measure, list(data = data, st1 = station, st2 = j, ...) )
    dS[j] <- d
    casesS[j] <- attributes(d)$cases
  }
  attr(dS, "cases") <- casesS[1]
  attr(dS, "measure") <- measure
  return(dS)
}
