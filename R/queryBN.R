#' @export

queryBN <- function( evidence, dbn, evidence.nodes, predictands, type = "exact", which_ = "marginal") {

  junction <- dbn$junction
  BN.fit <- dbn$BN.fit

  if (type == "exact"){
    evid <- setEvidence(junction, evidence.nodes, as.character(evidence)) # Evidence must be provided as character
    return( querygrain( object = evid, nodes = predictands, type = which_) )
  }
  else if (type == "simulation"){
    return( as.factor(simulateBN(BN.fit = BN.fit, evidence.nodes = evidence.nodes, evidence = evidence,
                                junction = junction, n = 1)) )
  }
  else if (type == "hybrid"){
    stop("Not yet.")
  }
  else if (type == "inexact"){
    stop("Not yet.")
  }
  else { stop("Please use a valid inference type: exact, simulation, hybrid, inexact.") }
}
