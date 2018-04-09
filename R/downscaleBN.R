#' @title Downscale with Bayesian Networks
#' @param DBN Downscaling Bayesian Network, as returned by \code{build.downscalingBN()}.
#' @param x
#' @param prediction.type Options are \code{"exact"}, \code{"approximate"} and \code{"simulation"}.
#' Exact inference requires a compilable junction.
#' @param output Options are: \code{"probabilities"}, \code{"event"} and
#'  \code{"probabilities.list"}. You should probably not use the last one. Note that if
#'  \code{prediction.type = "simulation"}, output is forced to \code{"event"}.
#' @author M.N. Legasa
#' @importFrom parallel parApply stopCluster
#' @importFrom pbapply pbapply
#' @export

downscaleBN <- function(DBN, x, output = "probabilities",
                        prediction.type = "exact", event = "1", threshold.vector = NULL,
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

  if (is.null(junction)) {
    print("Junction was not compiled at training stage.")
    junction <- compileJunction( BN.fit )
    print("Done.")
  }
  else if (prediction.type == "exact" && is.na(DBN$junction)) {
    warning("Junction was set as not compilable at training stage, prediction.type has been set to
          approximate",  immediate. = TRUE )
    prediction.type <- "approximate"
  }

  print("Propagating evidence and computing Probability Tables...")
  if ( parallelize == TRUE || !is.null(cl) ) {
    PSOCK.varExports.list <- list( "DBN", "predictors", "type", "predictands", "x")
    PSOCK.funcExportsNames.list <- list("setEvidence", "querygrain", "queryBN")
    cl <- parallelHandler(cluster.type, n.cores, PSOCK.varExports.list, PSOCK.funcExportsNames.list, cl)
    PT <- lapply(x, FUN = function (x) { pbapply(cl = cl, X = x, MARGIN = 1, FUN = queryBN,
                                                  dbn = DBN, evidence.nodes = predictors,
                                                  predictands = predictands, type = prediction.type
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
                                               predictands = predictands, type = prediction.type
                                               )
                                        }
                )
  }
  print("Done.")

  if (prediction.type == "simulation"){
      return(lapply(PT,
                    FUN = function(PT_) { return(characterToOperableMatrix(t(PT_)))}
      )
      )
  }
  else {
    if ( output == "probabilities.list" ) { return(PT) }
    else {
      return( auxParseProbabilitiesList(PT, predictands, prediction.type, threshold.vector,
                                        marginals, event))
    }
  }


}
