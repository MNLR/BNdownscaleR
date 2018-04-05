##############################################################################################################
#                     BAYESIAN NETWORKS DOWNSCALING                                                                    #
##############################################################################################################

# build.downscalingBN.R

#' @title Downscale DISCRETE climate data using Bayesian Networks.
#' @description Downscale discrete data to local scales by means of Bayesian Networks.
#' @param data Expects output from \code{\link[BNdownscaleR]{prepare_predictors.forBN}}.
#' @param structure.learning.algorithm Algorithm used to perform structure learning, with name
#' as text. Supports all the score-based, constraint-based and hybrid bayesian network structure
#' learning algorithms from \code{\link[bnlearn]{bnlearn}}.
#' Refer to \code{Details} for a list of supported algorithms.
#' @param structure.learning.args.list List of arguments passed to structure.learning.algorithm,
#'  in particular distance argument if local learning is used. Note that other arguments, e.g.
#'  \code{whitelist}, are an option (check the naming convention, see \code{Details}).
#'  Refer to \code{\link[bnlearn]{bnlearn}} for the specific options.
#' @param param.learning.method Either "bayes" or "mle", passed to learn the parameters for the
#' Conditional Probability Tables for the built DAG \code{data}.
#' @param forbid.GG If set to TRUE, arcs between grid or G nodes will be forbidden.
#' @param forbid.DD If set to TRUE, arcs between local, i.e. station or D nodes, will be forbidden.
#' @param forbid.DtoG If set to TRUE, arcs from D nodes to G nodes will be forbidden.
#' @param force.closest.GD Expects a positive integer or \code{NULL}. If not \code{NULL}, each D node will
#'  be linked, see \code{closest.GD.direction}, with the n closest G node(s), where n is
#'  \code{force.closest.GD}.
#' @param closest.GD.direction Either \code{NULL}, which lets the structure learning algorithm
#'  decide the direction, "up", which will place the arc(s) in the form \code{D -> G}, or "down",
#'  which will place the arc(s) in the form \code{G -> D}.
#' @param forbid.GD If set to TRUE, arcs between G and D nodes will be forbidden. See
#'  \code{Details}.
#' @param structure.learning.steps It is used to perform structure learning in up to three steps.
#'  Note that \code{past} refers to the past epochs when \code{dynamic = TRUE}.
#'  Refer to \code{Details}.
#' \itemize{
#'  \item \code{1} or \code{NULL} (Default) 1 step
#'  \item \code{2} or \code{c("local", "global")} If \code{dynamic = FALSE} learn first a DAG
#'  for D nodes, then inject G nodes. If \code{dynamic = TRUE} it equals
#'  c("local-global", "past")
#'  \item \code{3} Equals c("local", "global", "past")
#'  \item \code{c("local-global", "past")} or \code{c("global-local", "past")}.
#'   Learn first DAG for D and G nodes, then inject past nodes.
#'  \item \code{c("local", "global-past")} or \code{c("local", "past-global")}.
#'  Learn first DAG for D nodes, then inject past and G nodes.
#'  \item \code{c("local-past", "global")} or \code{c("past-local", "global")}.
#'  Learn first DAG for D and past nodes, then inject G nodes.
#'  \item \code{c("local", "global", "past")} Learn first DAG for D nodes, then inject G nodes,
#'  then inject past nodes.
#'  \item \code{c("local", "past", "global")} Learn first DAG for D nodes, then inject
#'  past nodes, then inject G nodes.
#' }
#' Note that only first two options are valid when \code{dynamic = FALSE}
#' @param fix.intermediate Set to TRUE to forbid the creation of new arcs in the next steps
#' for already built DAGs. See \code{Details}.
#' \code{structure.learning.algorithm2} and \code{structure.learning.args.list2}. See
#'  \code{Details}.
#' @param structure.learning.algorithm2   Same as structure.learning.algorithm for the second
#' step if \code{structure.learning.steps} is employed. Ignored otherwise.
#' @param structure.learning.args.list2   Same as structure.learning.args.list for the second
#'  step if \code{structure.learning.steps} is employed. Ignored otherwise.
#' @param dynamic Set to TRUE to use Dynamic Bayesian Networks. See \code{Details}.
#' @param epochs Number of epochs to consider for Dynamic Bayesian Networks.
#' @param remove.past.G When \code{dynamic = TRUE} Set to TRUE to remove the past G nodes.
#' @param keep.dynamic.distance When \code{dynamic = TRUE} and local learning is employed,
#'  if set to TRUE it will use its corresponding distance (See \code{Details}) between nodes
#'  from diferent epochs.
#' @param forbid.backwards When \code{dynamic = TRUE}, set to TRUE to forbid arcs going
#' back in time.
#' @param forbid.past.dynamic.GD When \code{dynamic = TRUE}, set to TRUE to forbid arcs in the
#' form G->D or D->G between different epochs.
#' @param forbid.dynamic.GG When \code{dynamic = TRUE} and \code{remove.past.G = FALSE}, set to
#' TRUE to forbid arcs in the form G-G in the past epochs.
#' @param forbid.past.DD When \code{dynamic = TRUE}, set to TRUE to forbid arcs in the form D-D
#' in the past epochs.
#' @param structure.learning.algorithm3   Same as structure.learning.algorithm for the third
#'  step if \code{structure.learning.steps} with 3 steps is employed. Ignored otherwise.
#' @param structure.learning.args.list3   Same as structure.learning.args.list for the third step
#' if \code{structure.learning.steps} with 3 steps is employed. Ignored otherwise.
#' See \code{Details}.
#' @param return.intermediate Add the intermediate DAGs to the output, as $intermediateDBN1 and
#'  $intermediateDBN2 (if any) if \code{structure.learning.steps} is employed.
#' @param output.marginals Compute and output Marginal Probability distribution Tables.
#' @param compile.junction Compile the junction from BN.fit to compute probabilities. Can be set
#'  to FALSE, in which case it can still be computed if needed at the training stage, i.e. through
#'  \code{downscale.BN()}.
#' @param parallelize Set to \code{TRUE} for parallelization. Refer to the
#'  \code{\link[parallel]{parallel}} and see \code{Details}.
#' @param n.cores When \code{parallelize = TRUE}, number of threads to be used, will use
#'  detectCores()-1 if not set.
#' @param cluster.type Either "PSOCK" or "FORK". Use the former under Windows systems,
#'  refer to \code{\link[parallel]{parallel}} package.
#'
#' @details
#' \strong{Structure Learning Algorithms}
#' Use \code{structure.learning.algorithm} to specify the algorithm for the structure (DAG) learning process.
#' Currently it DOES NOT support local discovery algorithms, expect malfuncion if used.
#' List of supported algorithms:
#' \code{"hc"}, \code{"tabu"} (score-based), \code{"gs"}, \code{"iamb"}, \code{"fast.iamb"}, \code{"inter.iamb"} (constraint-based),
#' \code{"mmhc"}, \code{"rsmax2"} (hybrid).
#' Check their corresponding parameters in \code{\link[bnlearn]{bnlearn}}, arguments may be passed to the algorithm through
#' the parameter structure.learning.args.list. Do not forget to set the distance argument in \code{structure.learning.args.list} for
#' local learning.
#'
#' \strong{Two or Three Step Learning}
#' \itemize{
#' \item \code{structure.learning.steps} allows to build separate DAGs for each set of nodes. Note that by employing the three
#' \code{structure.learning.algorithm}, \code{structure.learning.algorithm2}, \code{structure.learning.algorithm3} arguments and their
#' corresponding \code{structure.learning.args.list*} counterparts, many different configurations can be used for the structure learning
#' process, e.g. by using grow-shrink for D nodes with distance set to 1, then injecting the left nodes using hill-climbing without distance
#' restriction.
#' \item \code{fix.intermediate}, if set to \code{TRUE}, will forbid the creation of new arcs between nodes that were present in the previous
#' learning step. E.g. if \code{structure.learning.steps = c("local", "global\-past")}, no new arcs between D nodes will be created in the
#' second step, as the first DAG will be considered finished. If set to \code{FALSE}, the previous step DAG will be kept, but the next
#' learning algorithm could create new arcs between D nodes over the first one.
#' }
#'
#' \strong{Forbidding or Forcing Arcs}
#' For non dynamic Bayesian Networks, i.e. when \code{dynamic = FALSE} (default),
#' \code{forbid.GG}, \code{forbid.DD}, \code{forbid.DtoG}, \code{force.closest.GD},
#' \code{forbid.GD}, \code{fix.intermediate}, \code{structure.learning.steps} allow
#' introducing constraints to the structure learning algorithm. The user might also combine them
#' with \code{structure.learning.args.list$whitelist} and
#' \code{structure.learning.args.list$blacklist}. As \code{whitelist} has priority over
#'  \code{blacklist}, i.e. an arc placed in both will always be present in the DAG, they provide
#'  maximum flexibility. Bearing the priority of the \code{whitelist}, \code{force.closest.GD = TRUE}
#'  and \code{forbid.GD = TRUE} will, for example, forbid the placement of \emph{aditional}
#'  arcs beyond those specified as the closest G-D.
#'
#'  When manually specifying a whitelist or blacklist through \code{structure.learning.args.list},
#'  beware of the naming convention. It overrides the names and marks them as either "D.X" or "G.X",
#'  preditand and predictor nodes, respectivelly. It is best to plot a dummy network using plotDBN()
#'  first.
#'
#'
#'
#' \strong{Aditional details}
#' Parameters \code{output.marginals} and \code{compile.junction} are useful to save time if the
#'  user only intends to visualize the DAG.
#' @return An object of type DBN which contains the learnt Bayesian Network.
#' @author MN Legasa
#' @export
#' @examples
#' # Loading predictors

build.downscalingBN <- function(data,
                                structure.learning.algorithm = "hc",
                                structure.learning.args.list = list(),
                                param.learning.method = "bayes",
                                forbid.GG = FALSE, forbid.DD = FALSE, forbid.DtoG = FALSE,
                                force.closest.GD = NULL, closest.GD.direction = NULL,
                                forbid.GD = FALSE,
                                structure.learning.steps = 1,
                                fix.intermediate = TRUE,
                                structure.learning.algorithm2 = NULL,
                                structure.learning.args.list2 = list(),
                                dynamic = FALSE, epochs = 2,
                                remove.past.G = TRUE, keep.dynamic.distance = TRUE,
                                forbid.backwards = FALSE, forbid.past.dynamic.GD = TRUE,
                                forbid.dynamic.GG = TRUE, forbid.past.DD = TRUE,
                                structure.learning.algorithm3 = NULL,
                                structure.learning.args.list3 = list(),
                                return.intermediate = FALSE,
                                output.marginals = TRUE,
                                compile.junction = TRUE,
                                parallelize = FALSE, n.cores= NULL, cluster.type = "FORK"
                                ) {

  if (!(is.character(structure.learning.algorithm))) { stop("Input algorithm name as character") }
  if (dynamic && epochs == 1){
    dynamic <- FALSE
    print("Dynamic with 1 epoch equals non dynamic.")
  }

  if (dynamic && remove.past.G) {
    forbid.dynamic.GG <- FALSE
  }

  if (dynamic & epochs >= 2 & is.null(data$names.distribution)) { # is.null(data$data) = TRUE when already processed for Dynamic
    data <- prepareDataDynamicBN(data, epochs)
    if (remove.past.G) {
      data <- purgePastGs(data, epochs)
      forbid.dynamic.GG <- FALSE
    }
  }

  POS <- data$positions
  NX <- data$nx
  NY <- data$ny
  steps.left <- 0

  if (!is.null(structure.learning.steps) && structure.learning.steps != 1){

    structure.learning.steps <- parseStructureLearningStepsArg(structure.learning.steps, dynamic,
                                                               remove.past.G)
    if (grepl("past", structure.learning.steps[1])) {
      int.dynamic.args.list <- list(remove.past.G = FALSE, epochs = epochs)
    }
    else {int.dynamic.args.list <- NULL} # marks intermediate DAGs

    step.data <- handleLearningSteps(data, structure.learning.steps, dynamic)
    if (is.null(step.data)) {stop("Please, use a valid structure.learning.steps option.")}

    POS <- step.data$positions
    DATA <- step.data$DATA
    steps.left <- length(step.data$structure.learning.steps)
    print(paste0( c("Building intermediate DAG ", steps.left , " using ",
                    structure.learning.algorithm, " for ", structure.learning.steps[1],
                    " nodes", "..." ),
                  collapse = ""))
    structure.learning.steps <- step.data$structure.learning.steps
    if (dynamic){
      structure.learning.args.list <- addtoBlacklistDynamic(structure.learning.args.list,
                                                            step.data$names.distribution,
                                                            forbid.backwards, forbid.past.dynamic.GD,
                                                            forbid.dynamic.GG,
                                                            forbid.GG, forbid.DD, forbid.past.DD)
    }
  }
  else{ # Single or last step
    step.data <- NULL
    print( paste0(paste0("Building Bayesian Network using ", structure.learning.algorithm) , "..." ) )
    if (dynamic){  # MARKED FOR REVIEW; SHOULD SOMETHING FAIL
      # WARNING: addtoBlacklistDynamic() has yet forbid.DtoG to be implemented
      # WARNING: addtoBlacklistDynamic() has yet force.closest.GD to be implemented
      # WARNING: addtoBlacklistDynamic() has yet forbid.GD to be implemented
      structure.learning.args.list <- addtoBlacklistDynamic(structure.learning.args.list,
                                                            data$names.distribution, forbid.backwards,
                                                            forbid.past.dynamic.GD, forbid.dynamic.GG,
                                                            forbid.GG, forbid.DD, forbid.past.DD
                                                            )
    }
    else{
      structure.learning.args.list <- addtoBlacklist(structure.learning.args.list,
                                                     data, forbid.GG, forbid.DD, forbid.DtoG,
                                                     force.closest.GD, closest.GD.direction,
                                                     forbid.GD
                                                     )
    }
    DATA <- data$data
  }

  if ( !(is.null(structure.learning.args.list$distance)) ){   # local learning
    distance <- structure.learning.args.list$distance
    if (is.null(step.data)) { step.data <- data }
    structure.learning.args.list <- handleLocalLearning(step.data, structure.learning.args.list,
                                                        dynamic, keep.dynamic.distance)
    structure.learning.args.list$distance <- NULL
  } else { distance <- NULL }

  structure.learning.args.list[["x"]] <- DATA

  alg <- strsplit(structure.learning.algorithm, split = ".", fixed = TRUE)[[1]][1]
  if ( (alg == "gs") | (alg == "iamb") | (alg == "fast")  | (alg == "inter") | (alg == "inter")
       | (alg == "pc") ) { # Constraint based, parallelizable
    cl <- NULL
    if ( parallelize ) { # constraint-based algorithms allow parallelization
      cl <- parallelHandler(cluster.type, n.cores)
      structure.learning.args.list[["cluster"]] <- cl
    }
    bn <- cextend( do.call(structure.learning.algorithm, structure.learning.args.list) )
    if (!(is.null(cl))) {stopCluster(cl)} # Stops parallel cluster
  }
  else if ( (alg == "mmhc") | (alg == "rsmax2") ) {
    bn <- cextend( do.call(structure.learning.algorithm, structure.learning.args.list) )
  } # Non parallelizable, needs cextend arc direction
  else { bn <-  do.call(structure.learning.algorithm, structure.learning.args.list) } # Non parallelizable, already DAG (directed)
  if (steps.left == 0){
    bn.fit <- bn.fit(bn, data = DATA, method = param.learning.method)
    print("Done building Bayesian Network.")
  }

  if ( steps.left >= 1){
    print("Injecting next step into the DAG...")
    whitelist <- bn$arcs
    if (fix.intermediate){
      structure.learning.args.list2 <- fixIntermediate(step.data$names.distribution,
                                                       structure.learning.args.list2, whitelist
                                                       )
    }

    if (is.null(structure.learning.algorithm2) ){
      structure.learning.algorithm2 <- structure.learning.algorithm
    }
    if (steps.left == 2){
      if (is.null(structure.learning.algorithm3)){
        structure.learning.algorithm3 <- structure.learning.algorithm2
      }
    }

    structure.learning.args.list2 <- initializeDummyGreylist(structure.learning.args.list2,
                                                             "whitelist")
    rbind(structure.learning.args.list2$whitelist, whitelist)

    DBN <-  build.downscalingBN(data,
                                forbid.GG = forbid.GG,
                                forbid.DD = forbid.DD,
                                forbid.DtoG = forbid.DtoG,
                                force.closest.GD = force.closest.GD,
                                closest.GD.direction = closest.GD.direction,
                                forbid.GD = forbid.GD,
                                structure.learning.algorithm = structure.learning.algorithm2,
                                structure.learning.args.list = structure.learning.args.list2,
                                structure.learning.algorithm2 = structure.learning.algorithm3,
                                structure.learning.args.list2 = structure.learning.args.list3,
                                return.intermediate = return.intermediate,
                                dynamic = dynamic, keep.dynamic.distance, epochs = epochs,
                                remove.past.G = remove.past.G,forbid.backwards = forbid.backwards,
                                forbid.past.dynamic.GD = forbid.past.dynamic.GD,
                                forbid.dynamic.GG = forbid.dynamic.GG,
                                forbid.past.DD = forbid.past.DD,
                                structure.learning.steps = structure.learning.steps,
                                parallelize = parallelize, n.cores= n.cores,
                                cluster.type = cluster.type,
                                output.marginals = output.marginals,
                                compile.junction = compile.junction,
                                param.learning.method = param.learning.method
                                )
    if (return.intermediate){
      if (steps.left == 2){
        DBN[["intermediateDBN2"]] <- list(BN = bn, training.data = DATA, positions = POS,
                                          dynamic.args.list = int.dynamic.args.list,
                                          names.distribution = step.data$names.distribution,
                                          NX=NX, NY=NY,
                                          structure.learning.args.list = structure.learning.args.list)
      }
      if (steps.left == 1){
        DBN[["intermediateDBN1"]] <- list(BN = bn, training.data = DATA, positions = POS, dynamic.args.list = int.dynamic.args.list,
                                          names.distribution = step.data$names.distribution, NX=NX, NY=NY,
                                          structure.learning.args.list = structure.learning.args.list)
      }
      return(DBN)
    }
    else { return(DBN) }
  }
  else {
    if (output.marginals){
      print("Computing Marginal Distributions...")
      marginals_ <- marginals( list(BN = bn, BN.fit = bn.fit, NX = NX) )
      print("Done.")
    } else {marginals_ <- NULL}

    if (compile.junction){
      print("Compiling junction...")
      junction <- compile( as.grain(bn.fit), propagate = TRUE )
      print("Done.")
    } else {junction <- NULL}

    if (dynamic) {dynamic.args.list <- list( epochs = epochs, remove.past.G = remove.past.G,
                                             forbid.backwards = forbid.backwards,
                                             forbid.past.dynamic.GD = forbid.past.dynamic.GD,
                                             forbid.dynamic.GG = forbid.dynamic.GG,
                                             forbid.past.DD = forbid.past.DD
                                           )
      names.distribution <- data$names.distribution
    }
    else {
      dynamic.args.list <- NULL
      names.distribution <- NULL
    }

    if (!(is.null(distance))) { structure.learning.args.list[["distance"]] <- distance }

    return( list(BN = bn, training.data = DATA, positions = POS, BN.fit = bn.fit, junction = junction,
                 dynamic.args.list = dynamic.args.list,
                 NX = NX, NY = NY, names.distribution = names.distribution,
                 marginals = marginals_,
                 structure.learning.algorithm = structure.learning.algorithm,
                 structure.learning.args.list = structure.learning.args.list,
                 param.learning.method = param.learning.method)
          )
  }
}
