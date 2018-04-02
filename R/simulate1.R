simulate1 <- function(mbn, evidence.nodes, evidence, junction, evidence0, predictandS ) {

  dummydbn <- list(junction = junction)
  for (i in 1:length(mbn)){
    predictand <- mbn[[i]]$node
    PT <- queryBN(evidence = evidence, dbn = dummydbn, evidence.nodes = evidence.nodes,
                  predictands = predictand, type = "exact", which_ = "marginal")[[predictand]]

    simulated <- sample(x = names(PT), size = 1, prob = PT)
    evidence.nodes <- c(evidence.nodes, predictand)
    evidence <- c(evidence, simulated)
  }
  unsorted.evidence <- evidence[ (evidence0+1):length(evidence) ]
  names(unsorted.evidence) <- evidence.nodes[(evidence0+1):length(evidence.nodes)]

  return(unsorted.evidence)
}
