#' @title Downscale with Bayesian Networks
#' @param DBN Downscaling Bayesian Network, as returned by \code{build.downscalingBN()}.
#' @param x
#' @param prediction.type Options are: \code{"probabilities"}, \code{"event"}, \code{"simulation"} or
#'  \code{"probabilities.list"}
#' @author M.N. Legasa
#' @importFrom parallel parApply stopCluster
#' @importFrom pbapply pbapply
#' @export
#'

downscaleBN <- function(DBN, x,
                        prediction.type = "probabilities", event = "1", threshold.vector = NULL,
                        output.attr.evidence = FALSE,
                        cl = NULL, stop.cluster = TRUE, parallelize = FALSE, n.cores = NULL,
                        cluster.type = "FORK"){

  x <- x$x.global
  BN <- DBN$BN
  BN.fit <- DBN$BN.fit
  Nglobal <- DBN$NX
  junction <- DBN$junction
  predictors <- names(BN$nodes)[1:Nglobal]
  predictands <- names(BN$nodes)[ -(1:Nglobal) ]
  if (prediction.type == "probabilities" | prediction.type == "event" |
      prediction.type == "probabilities.list") {
    type <- "exact"
  } else {type = prediction.type}

  if (is.null(junction)){
    print("Junction was not compiled at training stage. Compiling junction...")
    junction <- compile( as.grain(BN.fit) )
    print("Done.")
  }

  print("Propagating evidence and computing Probability Tables...")
  if ( parallelize == TRUE) {
    PSOCK.varExports.list <- list( "DBN", "predictors", "type", "predictands", "x")
    PSOCK.funcExportsNames.list <- list("setEvidence", "querygrain", "queryBN")
    cl <- parallelHandler(cluster.type, n.cores, PSOCK.varExports.list, PSOCK.funcExportsNames.list, cl)
    PT <- lapply(x, FUN = function (x) { pbapply(cl = cl, X = x, MARGIN = 1, FUN = queryBN,
                                                  dbn = DBN, evidence.nodes = predictors,
                                                  predictands = predictands, type = type
                                                  )
                                        }
                )
    if (stop.cluster) {
      stopCluster(cl)
      print("Cluster off.")
    }
  }
  else { # Do not parallelize
    PT <- lapply( x, FUN = function (x) {pbapply(X = x, MARGIN = 1, FUN =  queryBN,
                                               dbn = DBN, evidence.nodes = predictors,
                                               predictands = predictands, type = type
                                               )
                                        }
                )
  }
  print("Done.")

  if ( prediction.type == "probabilities.list" ) { return(PT) }
  else if ( prediction.type == "simulation") {
    return(lapply(PT, FUN = function(PT_) {
                                            return(characterToOperableMatrix(t(PT_)))}))
                                          }
  else {
    return( auxParseProbabilitiesList(PT, predictands, prediction.type, threshold.vector,
                                      marginals, event))
  }
}
