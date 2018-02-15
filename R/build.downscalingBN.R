##############################################################################################################
#                     BAYESIAN NETWORKS DOWNSCALING                                                                    #
##############################################################################################################

# build.downscalingBN.R

#' @title Downscale DISCRETE climate data using Bayesian Networks.
#' @description Downscale discrete data to local scales by means of Bayesian Networks.
#' @param data Expects output from \code{\link[BNdownscaleR]{prepare_predictors.forBN}}.
#' @param structure.learning.algorithm Algorithm used to perform structure learning, with name as text. Supports all the score-based,
#' constraint-based  and hybrid bayesian network algorithms from \code{\link[bnlearn]{bnlearn}} and their *.local counterparts.
#' Refer to \code{Details} for a list of supported algorithms.
#' @param structure.learning.args.list List of arguments passed to structure.learning.algorithm, in particular distance argument if local learning
#' is used. Refer to \code{\link[bnlearn]{bnlearn}} for the specific options.
#' @param forbid.global.arcs      Arcs between grid nodes will be forbidden.
#' @param forbid.local.arcs       Arcs between local, i.e. station nodes, will be forbidden.
#' Will be used in second step if two.step is set to TRUE. See \code{Details}.
#' @param param.learning.method Either "bayes" or "mle", passed to learn the parameters of the built network structure from \code{data}.
#' @param two.step Learn first a local bayesian network, i.e. just for the stations, then inject global (grid) nodes. See arguments
#' \code{structure.learning.algorithm2} and \code{structure.learning.args.list2}. See \code{Details}.
#' @param structure.learning.algorithm2   Same as structure.learning.algorithm for the global injection process,
#' ignored if two.step is set to FALSE. See \code{Details}.
#' @param structure.learning.args.list2   Same as structure.learning.args.list for the global injection process,
#' ignored if two.step is set to FALSE. See \code{Details}.
#' @param output.marginals Compute and output Marginal Probability distribution Tables. Setting this to \code{FALSE} will force
#' \code{prediction.type = "probabilities"} in  downscale.BN().
#' @param compile.junction Compile the junction from BN.fit to compute probabilities. Can be set to FALSE,
#' in which case it will be computed at the training stage.
#' @param parallelize Set to \code{TRUE} for parallelization. Refer to the \code{\link[parallel]{parallel}} and see \code{Details}.
#' @param n.cores When \code{parallelize = TRUE}, number of threads to be used, will use detectCores()-1 if not set.
#' @param cluster.type Either "PSOCK" or "FORK". Use the former under Windows systems, refer to \code{\link[parallel]{parallel}}
#' package.
#'
#' @details
#'
#' \strong{Structure Learning Algorithms}
#' Use \code{structure.learning.algorithm}
#' Currently it DOES NOT support local discovery algorithms, expect malfuncion if used.
#' List of supported algorithms:
#' "hc", "tabu" (score-based), "gs", "iamb", "fast.iamb", "inter.iamb" (constraint-based),  "mmhc", "rsmax2" (hybrid).
#' Use "*.local" to perform the local learning counterpart, e.g. "hc.local", "fast.iamb.local"...
#' Check their corresponding parameters in \code{\link[bnlearn]{bnlearn}}, arguments may be passed to the algorithm through
#' the parameter structure.learning.args.list. Do not forget to set the distance argument in \code{structure.learning.args.list} for
#'  *.local learning.
#'
#' \strong{Two Step Learning}
#' When \code{two.step = TRUE}, an independent DAG (Directed Acyclic Graph) will be built for the local (i.e. grid or predictand)
#' nodes, then a second graph, containing the first as a subgraph and with the grid (predictor) nodes, will be learnt.
#' Note:
#' \itemize{
#' \item First step uses parameters \code{structure.learning.algorithm} and \code{structure.learning.args.list} are used for the
#' construction of the first DAG. \code{forbid.global.arcs} and \code{forbid.local.arcs} are ignored at this stage.
#' \item Global injection step uses \code{structure.learning.algorithm2} and \code{structure.learning.args.list2} for learning the
#' DAG structure. At this stage, \code{forbid.global.arcs = TRUE} will forbid arcs between grid (predictor) nodes, whereas
#' \code{forbid.local.arcs} will forbid the creation of new arcs between local (predictand) nodes.
#' }
#' If \code{return.first = TRUE}, the output will be a list containing \code{$first} and \code{$last}:
#' #' \itemize{
#' \item \code{$first} contains the first DAG, in a minimal form, which means that it is not a Bayesian Network but still retains a
#' structure that allows the usage of plot.DBN() to examine the graph.
#' \item \code{$last} contains the proper, ready to use, Bayesian Network.
#' }
#'
#' \strong{Aditional details}
#' \code{output.marginals} and \code{compile.junction} are useful to save time if the user only intends to visualize the DAG.
#' \code{whitelist} and \code{blacklist} arguments can be passed to structure.learning.args.list, but beware of the naming convention,
#' it is best to use plotDBN() first with a dummy network.
#' @return An object of type DBN, which contains, in particular, the Bayesian Network.
#' @author MN Legasa
#' @export
#' @examples
#' # Loading predictors


build.downscalingBN <- function(data,
                                structure.learning.algorithm = "hc",
                                structure.learning.args.list = list(),
                                param.learning.method = "bayes",
                                forbid.global.arcs = TRUE, forbid.local.arcs = FALSE,
                                dynamic = FALSE, epochs = 2, only.present.G = TRUE,
                                forbid.backwards = FALSE, forbid.dynamic.GD = TRUE, forbid.dynamic.global.arcs = TRUE, forbid.past.DD = TRUE,
                                structure.learning.steps = NULL,
                                structure.learning.algorithm2 = NULL,
                                structure.learning.args.list2 = list(),
                                structure.learning.algorithm3 = NULL,
                                structure.learning.args.list3 = list(),
                                return.first = FALSE,
                                output.marginals = TRUE,
                                compile.junction = TRUE,
                                parallelize = FALSE, n.cores= NULL, cluster.type = "PSOCK"
                                ) {

  if (!(is.character(structure.learning.algorithm))) { stop("Input algorithm name as character") }

  if (dynamic & epochs >= 2 & is.null(data$names.distribution)) {
    data <- prepareDataDynamicBN(data, epochs)
    if (only.present.G) {
      data <- purgeOldGs(data)
      forbid.dynamic.global.arcs <- FALSE
    }
    structure.learning.args.list <- addtoBlacklistDynamic(structure.learning.args.list, data$names.distribution, forbid.backwards, forbid.dynamic.GD, forbid.dynamic.global.arcs,
                                                           forbid.global.arcs, forbid.local.arcs, forbid.past.DD)
  }

  POS <- data$positions
  NX <- data$nx
  NY <- data$ny

  if (!is.null(structure.learning.steps) && structure.learning.steps != 1){
    aux <- handleLearningSteps(data, structure.learning.steps, dynamic)
    POS <- aux$POS
    DATA <- aux$DATA
    structure.learning.steps <- aux$structure.learning.steps
    steps.left <- length(structure.learning.steps)
  }
  else{
    steps.left <- 0
    if (forbid.global.arcs & dynamic == FALSE){
      globalNodeNames <- data$x.names
      structure.learning.args.list <- add.toBlacklist(globalNodeNames, structure.learning.args.list)
    }
    if (forbid.local.arcs & dynamic == FALSE){
      localNodeNames <- data$y.names
      structure.learning.args.list <- add.toBlacklist(localNodeNames, structure.learning.args.list)
    }
    DATA <- data$data
  }
  # For local learning positions need to be inputed
  if ( substr(structure.learning.algorithm, nchar(structure.learning.algorithm)-5+1, nchar(structure.learning.algorithm)) == "local" ){
    structure.learning.args.list[["positions"]] <- POS
  }

  structure.learning.args.list[["x"]] <- DATA

  print("Building Bayesian Network...")
  alg <- strsplit(structure.learning.algorithm, split = ".", fixed = TRUE)[[1]][1]
  if ( (alg == "gs") | (alg == "iamb") | (alg == "fast")  | (alg == "inter") | (alg == "inter") ) { # Constraint based, parallelizable
    cl <- NULL
    if ( parallelize ) { # constraint-based algorithms allow parallelization
      cl <- parallel.starter(cluster.type, n.cores)
      structure.learning.args.list[["cluster"]] <- cl
    }
    bn <- cextend( do.call(structure.learning.algorithm, structure.learning.args.list) )
    if (!(is.null(cl))) {stopCluster(cl)} # Stops parallel cluster
  }
  else if ( (alg == "mmhc") | (alg == "rsmax2") ) { bn <- cextend( do.call(structure.learning.algorithm, structure.learning.args.list) )} # Non parallelizable, need cextend arc direction
  else { bn <-  do.call(structure.learning.algorithm, structure.learning.args.list) } # Non parallelizable, already DAG (directed)
  if (steps.left == 0){ bn.fit <- bn.fit(bn, data = DATA, method = param.learning.method) }
  print("Done building Bayesian Network.")

  if ( steps.left >= 1){
    print("Injecting next step into Bayesian Network...")
    whitelist <- bn$arcs

    if (is.null(structure.learning.algorithm2) ){ structure.learning.algorithm2 <- structure.learning.algorithm }

    if (steps.left == 2){
      if (is.null(structure.learning.algorithm3)){
        structure.learning.algorithm3 <- structure.learning.algorithm2
      }
      if ( is.null(structure.learning.args.list3$whitelist) ){ structure.learning.args.list3[["whitelist"]] <- whitelist }
      else{ rbind(whitelist, structure.learning.args.list3$whitelist) }
    }
    else {
      if ( is.null(structure.learning.args.list2$whitelist) ){ structure.learning.args.list2[["whitelist"]] <- whitelist }
      else{ rbind(whitelist, structure.learning.args.list2$whitelist) }
    }
    print(whitelist)
    DBN <-  build.downscalingBN(data,
                                forbid.global.arcs = forbid.global.arcs,
                                forbid.local.arcs = forbid.local.arcs,
                                structure.learning.algorithm = structure.learning.algorithm2,
                                structure.learning.args.list = structure.learning.args.list2,
                                structure.learning.algorithm2 = structure.learning.algorithm3,
                                structure.learning.args.list2 = structure.learning.args.list3,
                                dynamic = dynamic,
                                structure.learning.steps = structure.learning.steps,
                                parallelize = parallelize, n.cores= n.cores, cluster.type = cluster.type,
                                output.marginals = output.marginals,
                                compile.junction = compile.junction,
                                param.learning.method = param.learning.method
                                )
    if (return.first){
      print("return.first is deactivated")
      #return( list(first = list(BN = bn, training.data = DATA, positions = POS,  structure.learning.args.list = structure.learning.args.list),
      #             last = DBN) )
      return(DBN)
    }
    else { return(DBN) }
  }
  else {

    if (output.marginals){
      print("Computing Marginal Distributions...")
      marginals_ <- marginals( list(BN = bn, BN.fit = bn.fit, NX = NX) )
      print("Done.")
    }
    else {marginals_ <- NULL}
    if (compile.junction){
      print("Compiling junction...")
      junction <- compile( as.grain(bn.fit) )
      print("Done.")
    }
    else {
      junction <- NULL
    }

    if (dynamic) {dynamic.args.list <- list( epochs = epochs, only.present.G = only.present.G,
                                             forbid.backwards = forbid.backwards,
                                             forbid.dynamic.GD = forbid.dynamic.GD,
                                             forbid.dynamic.global.arcs = forbid.dynamic.global.arcs,
                                             forbid.past.DD = forbid.past.DD)}
    else { dynamic.args.list <- NULL}

    return( list(BN = bn, training.data = DATA, positions = POS, BN.fit = bn.fit, junction = junction,
                 dynamic.args.list = dynamic.args.list,
                 NX = NX, NY = NY,
                 marginals = marginals_,
                 structure.learning.algorithm = structure.learning.algorithm,
                 structure.learning.args.list = structure.learning.args.list,
                 param.learning.method = param.learning.method)
          )
  }
}


