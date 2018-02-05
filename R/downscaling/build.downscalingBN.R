source("R/downscaling/aux/add.toBlacklist.R")
source("R/downscaling/marginals.R")
source("R/local.bnlearning/hc.local.R")
source("R/local.bnlearning/tabu.local.R")
source("R/local.bnlearning/gs.local.R")
source("R/local.bnlearning/iamb.local.R")
source("R/local.bnlearning/fast.iamb.local.R")
source("R/local.bnlearning/inter.iamb.local.R")
source("R/local.bnlearning/mmhc.local.R")
source("R/local.bnlearning/rsmax2.local.R")

build.downscalingBN <- function(data,
                                forbid.global.arcs = TRUE, forbid.local.arcs = FALSE,
                                bnlearning.algorithm = "hc",
                                bnlearning.args.list = list(),
                                param.learning.method = "bayes",
                                output.marginals = TRUE,
                                compile.junction = TRUE,
                                parallelize = FALSE, n.cores= NULL, cluster.type = "PSOCK",
                                two.step = FALSE,
                                return.first = FALSE,
                                bnlearning.algorithm2 = NULL,
                                bnlearning.args.list2 = list()
                                ) {
  # global   Expects output from prepare_predictors()
  # local    predictands. Expects categorical data. NaNs will be processed
  # bnlearning.algorithm    Supports all the score-based, constraint-based  and hybrid bayesian network algorithms from bnlearn and their *.local counterparts.
  #                           Check their corresponding parameters, ?bnlearn. DOES NOT support local discovery algorithms, expect malfuncion if used.
  #                           List of supported algorithms:
  #                             "hc", "tabu" (score-based), "gs", "iamb", "fast.iamb", "inter.iamb" (constraint-based),  "mmhc", "rsmax2" (hybrid).
  #                             Use "*.local" to perform local learning, e.g. "hc.local", "fast.iamb.local"...
  #                               Do not forget to set the distance argument in bnlearning.args.list for *.local learning.
  # bnlearning.args.list    List of arguments passed to bnlearning.algorithm, in particular distance argument if local learning is used.
  # forbid.global.arcs      Arcs between global nodes will be forbidden.
  # forbid.local.arcs       Arcs between local nodes will be forbidden. Will be used in second step if two.step is set to TRUE
  # param.learning.method   Either "bayes" or "mle", passed to learn the parameters of the built network structure.
  #
  # output.marginals        Compute and output Marginal Probability distribution Tables. This will force prediction.type = "probabilities"
  #                           in downscale.BN().
  # compile.junction        Compile the junction from BN.fit to compute probabilities. Can be set to FALSE, in which case it will be computed
  #                           at the training stage.
  # two.step                Learn first local bayesian network, then inject global nodes.
  #                           bnlearning.algorithm and bnlearning.args.list will be used for the first step.
  #                           bnlearning.algorithm2 and bnlearning.args.list2 will be used for the second step (global injection)
  # return.first            If set to TRUE, output will be a list with the two bayesian networks, $first and $last if two.step is set to TRUE.
  #                           Otherwise the single last adjusted Bayesian network will be returned.
  # bnlearning.algorithm2   Same as bnlearning.algorithm for the global injection process, ignored if two.step is set to FALSE
  # bnlearning.args.list2   Same as bnlearning.args.list for the global injection process, ignored if two.step is set to FALSE
  # parallelize             Set to TRUE to make use of the parallel package.
  #                           Should improve computation times a lot if
  #                               1) Clustering is performed, and/or
  #                               2) A constraint-based family algorithm is used
  # n.cores                 Number of threads to be used, will use detectCores()-1 if not set.
  # cluster.type            Either "PSOCK" or "FORK". Use the former under Windows systems, refer to parallel package.


  if (!(is.character(bnlearning.algorithm))) { stop("Input algorithm name as character") }

  POS <- data$positions
  NX <- data$nx
  NY <- data$ny

  if (two.step){ # First step has no global
    POS <- POS[ , (NX+1):(NX+NY)]
    DATA <- data$data[ , (NX+1):(NX+NY)]
  }
  else{
    if (forbid.global.arcs){
      globalNodeNames <- colnames(POS[ , 1:NX ])
      bnlearning.args.list <- add.toBlacklist(globalNodeNames, bnlearning.args.list)
    }
    if (forbid.local.arcs){
      localNodeNames <- colnames(POS[ , (NX+1):(NX+NY)])
      bnlearning.args.list <- add.toBlacklist(localNodeNames, bnlearning.args.list)
    }
    DATA <- data$data
  }
  # For local learning positions need to be inputed
  if ( substr(bnlearning.algorithm, nchar(bnlearning.algorithm)-5+1, nchar(bnlearning.algorithm)) == "local" ){
    bnlearning.args.list[["positions"]] <- POS
  }

  bnlearning.args.list[["x"]] <- DATA

  print("Building Bayesian Network...")
  alg <- strsplit(bnlearning.algorithm, split = ".", fixed = TRUE)[[1]][1]
  if ( (alg == "gs") | (alg == "iamb") | (alg == "fast")  | (alg == "inter") | (alg == "inter") ) { # Constraint based, parallelizable
    cl <- NULL
    if ( parallelize ) { # constraint-based algorithms allow parallelization
      if ( is.null(n.cores) ){
        n.cores <- floor(detectCores()-1)
      }
      # Initiate cluster
      cl <- makeCluster(n.cores, type = cluster.type )
      bnlearning.args.list[["cluster"]] <- cl
    }
    bn <- cextend( do.call(bnlearning.algorithm, bnlearning.args.list) )
    if (!(is.null(cl))) {stopCluster(cl)}
  }
  else if ( (alg == "mmhc") | (alg == "rsmax2") ) { bn <- cextend( do.call(bnlearning.algorithm, bnlearning.args.list) )} # Non parallelizable, need cextend arc direction
  else { bn <-  do.call(bnlearning.algorithm, bnlearning.args.list) } # Non parallelizable, already DAG (directed)
  if (!two.step){ bn.fit <- bn.fit(bn, data = DATA, method = param.learning.method) }
  print("Done.")

  if ( two.step ){
    print("Injecting Globals into Bayesian Network...")
    whitelist <- bn$arcs
    print(whitelist)

    if (is.null(bnlearning.algorithm2) ){ bnlearning.algorithm2 <- bnlearning.algorithm }

    if ( is.null(bnlearning.args.list2$whitelist) ){ bnlearning.args.list2[["whitelist"]] <- whitelist }
    else{ rbind(whitelist, bnlearning.args.list2$whitelist) }

    DBN <-  build.downscalingBN(data,
                                forbid.global.arcs = forbid.global.arcs,
                                forbid.local.arcs = forbid.local.arcs,
                                bnlearning.algorithm = bnlearning.algorithm2,
                                bnlearning.args.list = bnlearning.args.list2,
                                parallelize = parallelize, n.cores= n.cores, cluster.type = cluster.type,
                                output.marginals = output.marginals,
                                param.learning.method = param.learning.method,
                                two.step = FALSE)
    if (return.first){
      return( list(first = list(BN = bn, training.data = DATA, positions = POS,  bnlearning.args.list = bnlearning.args.list),
                   last = DBN) )
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
      bn.fit <- NULL
    }
    else {
      junction <- NULL
    }

    return( list(BN = bn, training.data = DATA, positions = POS, BN.fit = bn.fit, junction = junction,
                 NX = NX,
                 marginals = marginals_,
                 bnlearning.algorithm = bnlearning.algorithm,
                 bnlearning.args.list = bnlearning.args.list,
                 param.learning.method = param.learning.method)
          )
  }
}


