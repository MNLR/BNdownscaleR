#' @export

simulateBN <- function(BN.fit, evidence.nodes, evidence, junction, cl = NULL, n = 1){

  if (length(evidence.nodes) != length(evidence)) {stop("Provide a single evidence for every evidence node.")}
  if (!identical(intersect(names(dbn$BN.fit), evidence.nodes), evidence.nodes)){
    stop("Some of the evidence.nodes are not present in the DAG.")
  }
  predictandS <- setdiff(names(BN.fit), evidence.nodes)

  mbn <- mutilated(BN.fit, evidence = dataToBNEvidenceList(evidence.nodes, evidence))
  mbn <- mbn[predictandS]
  nodes.parents <- sapply(mbn, FUN = function(x) {return(length(x$parents))})
  mbn <- mbn[names(nodes.parents)[order(nodes.parents)]]
  evidence0 <- length(evidence.nodes)

  if (is.null(cl)){
    simulationS <- sapply(1:n,
           FUN = function(n_, mbn, evidence.nodes, evidence, junction, evidence0, predictandS) {
                  simulation <- simulate1(mbn, evidence.nodes, evidence, junction, evidence0, predictandS)
                  return(simulation)
                 },
           mbn = mbn, evidence.nodes = evidence.nodes, evidence = evidence, junction = junction,
           evidence0 = evidence0, predictandS
    )
  } else {
    simulationS <- parSapply(cl, 1:n,
              FUN = function(n_, mbn, evidence.nodes, evidence, junction, evidence0, predictandS) {
                simulation <- simulate1(mbn, evidence.nodes, evidence, junction, evidence0, predictandS)
                return(simulation)
              },
              mbn = mbn, evidence.nodes = evidence.nodes, evidence = evidence, junction = junction,
              evidence0 = evidence0, predictandS
    )
  }
  simulationS <- t(simulationS)
  # node reordering
  order.index <- match(predictandS, colnames(simulationS))
  simulationS <- simulationS[ , order.index]
  return(simulationS)
}
