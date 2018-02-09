#' @title Downscale with Bayesian Networks
#' @param DBN Downscaling Bayesian Network, as returned by build.downscalingBN(). This is a wrapper for plot.restrictedgraph.R
#' @author M.N. Legasa
#' @importFrom parallel parApply stopCluster
#' @export
#'

downscale.BN <- function(DBN, x,
                         prediction.type = "probabilities", event = "1", threshold.vector = NULL,
                         output.attr.evidence = FALSE,
                         parallelize = FALSE, n.cores = NULL , cluster.type = "PSOCK"){

  # Parallelize = TRUE should reduce computation times significantly when lots of evidences are provided.
  # cluster.type    Accepts "PSOCK" and "FORK". "FORK" cannot be used in Windows systems.
  # prediction.type Options are "event" "probabilities" "probabilities.list"
  #                   "event" returns a binary prediction based on threshold.vector. By default threshold.vector
  #                     is set to NULL, which will use as threshold 1-MP where MP is the marginal probability,
  #                     for each node. If DBN has no $marginals value or threshold.vector is NULL,
  #                     "probabilities" setting will apply.
  #                   "probabilities" returns the probabilities as a matrix where dimensions are [obs, cat, node]
  #                   "probabilities.list" returns a list of nodes with their probability tables.
  #                     Warning: Beware of the nodes ordering if set to FALSE!
  #

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

    #clusterExport(cl,  , envir = environment())
    #clusterExport(cl,
    PSOCK.varExports.list <- list( "junction", "predictors" , "predictands", "x")
    PSOCK.funcExportsNames.list <- list("setEvidence", "querygrain" , "predict.DBN")
    cl <- parallel.starter(cluster.type, n.cores, PSOCK.varExports.list, PSOCK.funcExportsNames.list)

    PT <- lapply(x, FUN = function (x) { parApply(cl, x, MARGIN = 1, FUN = predict.DBN,
                                                  predictors = predictors, junction = junction , predictands = predictands
                                                  )
                                        }
                )
    stopCluster(cl)
  }
  else { # Do not parallelize
    PT <- lapply( x, FUN = function (x) {apply(x, MARGIN = 1, FUN =  predict.DBN,
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
    # Node re-ordering due to bnlearn disordering
    return( lapply( PT, function(ELPT) { downscaled <- aperm(simplify2array( sapply(ELPT, simplify2array, simplify = FALSE),
                                                                             higher = TRUE ) , c(3,1,2)
                                                            )
                                      ELPT <- downscaled[,,match(predictands, colnames(downscaled[1,,]))]
                                      if ( prediction.type == "event" & ( !(is.null(DBN$marginals)) | !(is.null(threshold.vector)) ) ){
                                        if (is.null(threshold.vector)){ threshold.vector  <- 1 - DBN$marginals[event, ] }
                                        return( is.mostLikely(ELPT, event = event, threshold.vector =  threshold.vector) )
                                      }
                                      else {
                                        return(ELPT)
                                      }
                                  }
                  )
          )
  }
}
