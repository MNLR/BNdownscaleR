#' @export

queryBN <- function( evidence, dbn, evidence.nodes, predictands, type = "exact",
                     which_ = "marginal", resample.size = 10000,  cl = NULL
                     )  {
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
  else if (type == "approximate"){
    lwsample <- cpdist( fitted = BN.fit, nodes = predictands,
             evidence = auxEvidenceTocpdistImput(evidence.nodes, evidence),
             method = 'lw', cluster = cl)
    simsample <- lwsample[sample(1:nrow(lwsample), prob = attributes(lwsample)$weights, size = resample.size , replace = TRUE), ]
    return( lapply(simsample, FUN = function(x) { return( table(x)/sum(table(x)) ) } ) )
  }
  else { stop("Please use a valid inference type: exact, approximate, simulation.") }
}
