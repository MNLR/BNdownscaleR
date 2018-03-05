#' @export

logLoss <- function( data, st1, st2, remove.na =  TRUE, evidence.nodes = NULL, evidence = NULL, no.inf = TRUE ){

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

  n <- nrow(Data)

  if (!no.inf && n10*n01 == 0) {logl <- Inf}
  else if (!no.inf && n00*n11 == 0) {logl <- -Inf}
  else{ logl <- log(n00*n11/n10*n01) }
  attr(logl, 'cases') <- n

  return( logl )
}
