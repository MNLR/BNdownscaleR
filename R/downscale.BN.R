#' @title Downscale with Bayesian Networks
#' @param DBN Downscaling Bayesian Network, as returned by \code{build.downscalingBN()}.
#' @author M.N. Legasa
#' @importFrom parallel parApply stopCluster
#' @export
#'

downscale.BN <- function(DBN, x,
                         prediction.type = "probabilities", event = "1", threshold.vector = NULL,
                         output.attr.evidence = FALSE,
                         cl = NULL, stop.cluster = TRUE, parallelize = FALSE, n.cores = NULL , cluster.type = "FORK"){

  x <- x$x.global
  BN <- DBN$BN
  BN.fit <- DBN$BN.fit
  Nglobal <- DBN$NX
  junction <- DBN$junction
  predictors <- names(BN$nodes)[1:Nglobal]
  predictands <- names(BN$nodes)[- (1:Nglobal) ]

  if (is.null(junction)){
    print("Junction was not compiled at training stage. Compiling junction...")
    junction <- compile( as.grain(BN.fit) )
    print("Done.")
  }

  print("Propagating evidence and computing Probability Tables...")
  if ( parallelize == TRUE) {

    PSOCK.varExports.list <- list( "junction", "predictors" , "predictands", "x")
    PSOCK.funcExportsNames.list <- list("setEvidence", "querygrain" , "queryBN")
    cl <- parallelHandler(cluster.type, n.cores, PSOCK.varExports.list, PSOCK.funcExportsNames.list, cl)

    PT <- lapply(x, FUN = function (x) { parApply(cl, x, MARGIN = 1, FUN = queryBN,
                                                  predictors = predictors, junction = junction , predictands = predictands
                                                  )
                                        }
                )
    if (stop.cluster) {
      stopCluster(cl)
      print("Cluster off.")
    }
  }
  else { # Do not parallelize
    PT <- lapply( x, FUN = function (x) {apply(x, MARGIN = 1, FUN =  queryBN,
                                               predictors = predictors, junction = junction , predictands = predictands
                                               )
                                        }
                )
  }
  print("Done.")

  if ( prediction.type == "probabilities.list" ) {
    return(PT)
  }
  else {
    return( auxParseProbabilitiesList(PT, predictands, prediction.type, threshold.vector, marginals, event))
  }
}
