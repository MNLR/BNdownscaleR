
buildDynamicCBNnoG <- function(y,
                               structure.learning.algorithm = "hc",
                               structure.learning.args.list = list(),
                               param.learning.method = "bayes",
                               epochs = 2,
                               structure.learning.steps = 1,
                               fix.intermediate = TRUE,
                               structure.learning.algorithm2 = NULL,
                               structure.learning.args.list2 = list(),
                               keep.dynamic.distance = TRUE,
                               remove.past.G = TRUE,
                               forbid.backwards = FALSE,
                               forbid.past.dynamic.GD = TRUE,
                               forbid.dynamic.GG = FALSE,
                               forbid.past.DD = FALSE,
                               return.intermediate = FALSE,
                               compile.junction = FALSE,
                               parallelize = FALSE, n.cores= NULL,
                               cluster.type = "FORK") {

  if (structure.learning.steps != 1){
    warning("Structure.learning.steps not implemented yet.")
  }
  steps.left <- 0

  py <- prepare_Dataset_forDescriptiveBN(y)

  print( paste0(paste0("Building Bayesian Network using ", structure.learning.algorithm) , "..." ) )

  if ( epochs >= 2 & is.null(py$names.distribution) ) { # is.null(data$data) = TRUE when already processed for Dynamic
    py <- prepareDataDynamicBN(py, epochs)
  }
  POS <- py$positions
  NX <- py$nx
  NY <- py$ny
  DATA <- py$data

  structure.learning.args.list <- addtoBlacklistDynamic(structure.learning.args.list,
                                                        py$names.distribution,
                                                        forbid.backwards,
                                                        forbid.past.DD
  )

# if ( !(is.null(structure.learning.args.list$distance)) ){   # local learning
#   distance <- structure.learning.args.list$distance
#   if (is.null(step.data)) { step.data <- data }
#   structure.learning.args.list <- handleLocalLearning(step.data, structure.learning.args.list,
#                                                       dynamic, keep.dynamic.distance)
#   structure.learning.args.list$distance <- NULL
# } else { distance <- NULL }

  structure.learning.args.list[["x"]] <- DATA
  bn <- learnDAG(structure.learning.algorithm, structure.learning.args.list,
                 parallelize, cluster.type, n.cores
  )

  if (steps.left == 0){
    bn.fit <- bn.fit(bn, data = DATA, method = param.learning.method)
    print("Done building Bayesian Network.")
  }

  if (compile.junction){
    junction <- compileJunction(bn.fit)
  } else { junction <- NULL }

  marginals_ <- marginals( list(BN = bn, NX = NX, junction = junction,
                                training.data = DATA) )

  dynamic.args.list <- list( epochs = epochs,
                             forbid.backwards = forbid.backwards,
                             forbid.past.DD = forbid.past.DD
  )
  names.distribution <- py$names.distribution

#if (!(is.null(distance))) { structure.learning.args.list[["distance"]] <- distance }

  wg <- list(BN = bn, training.data = DATA, positions = POS, BN.fit = bn.fit,
             junction = junction,
             dynamic.args.list = dynamic.args.list,
             NX = NX, NY = NY, names.distribution = names.distribution,
             marginals = marginals_,
             structure.learning.algorithm = structure.learning.algorithm,
             structure.learning.args.list = structure.learning.args.list,
             param.learning.method = param.learning.method
  )
  class(wg) <- "Weather.Generator.BN"
  return(wg)
}
