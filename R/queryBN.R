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
    sim_ <- simulate1(BN.fit = BN.fit, junction = junction,
                      evidence.nodes = evidence.nodes,
                      evidence = evidence)
    order.index <- match(predictands, names(sim_))
    return( sim_[order.index] )
  }
  else if (type == "approximate"){
    lwsample <- cpdist( fitted = BN.fit, nodes = predictands,
             evidence = auxEvidenceTocpdistImput(evidence.nodes, evidence),
             method = 'lw', cluster = cl)
    simsample <- lwsample[sample(1:nrow(lwsample), prob = attributes(lwsample)$weights,
                                 size = resample.size , replace = TRUE), ]
    if (length(predictands) == 1){
      tsimsample <- table(simsample)
      return( tsimsample/sum(tsimsample) )
    }
    else{
      return( lapply(simsample, FUN = function(x) {
                                        tx <- table(x)
                                        return(tx/sum(tx))
                                      }
                     )
             )
    }
  }
  else { stop("Please use a valid inference type: exact, approximate, simulation.") }
}
