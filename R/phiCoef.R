#' @export

phiCoef <- function( data, st1, st2, remove.na =  TRUE, evidence.nodes = NULL, evidence = NULL ){

  if (length(evidence.nodes) != length(evidence)) {stop("Provide a single evidence for every node.")}
  Data <- filterData(data, st1, st2, remove.na, evidence.nodes, evidence)

  ## Phi coefficient equals Pearson for binary variables.
  if (nrow(Data) <= 1) {stop("Less than 1 case matched the evidence.")}
  n11 <- as.numeric(table(Data[ ,1] == 1 & Data[ ,2] == 1)["TRUE"])
  if (is.na(n11)) {n11 <- 0}
  n00 <- as.numeric(table(Data[ ,1] == 0 & Data[ ,2] == 0)["TRUE"])
  if (is.na(n00)) {n00 <- 0}
  n10 <- as.numeric(table(Data[ ,1] == 1 & Data[ ,2] == 0)["TRUE"])
  if (is.na(n10)) {n10 <- 0}
  n01 <- as.numeric(table(Data[ ,1] == 0 & Data[ ,2] == 1)["TRUE"])
  if (is.na(n01)) {n01 <- 0}

  n1_ <- as.numeric(table(Data[ ,1] == 1)["TRUE"])
  n0_ <- as.numeric(table(Data[ ,1] == 0)["TRUE"])
  n_1 <- as.numeric(table(Data[ ,2] == 1)["TRUE"])
  n_0 <- as.numeric(table(Data[ ,2] == 0)["TRUE"])

  phi <- (n11*n00 - n10*n01)/(sqrt(n1_)*sqrt(n0_)*sqrt(n_1)*sqrt(n_0))
  attr(phi, 'cases') <- nrow(Data)

  return( phi )
}
