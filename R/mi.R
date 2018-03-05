#' @importFrom infotheo mutinformation
#' @export

mi <- function( data, st1, st2, method = "emp", remove.na =  TRUE, evidence.nodes = NULL, evidence = NULL ){

  if (length(evidence.nodes) != length(evidence)) {stop("Provide a single evidence for every node.")}
  Data <- filterData(data, st1, st2, remove.na, evidence.nodes, evidence)

  ## Phi coefficient equals Pearson for binary variables.
  if (nrow(Data) <= 1) {stop("Less than 1 case matched the evidence.")}

  MI <- mutinformation(X = Data[ ,1], Y = Data[ ,2], method = method)
  attr(MI, 'cases') <- nrow(Data)

  return( MI )
}
