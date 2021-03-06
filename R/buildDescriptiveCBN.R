#'
#' @export

buildDescriptiveCBN <- function(y, structure.learning.algorithm =  "tabu",
                                structure.learning.args.list = list(),
                                compile.junction = TRUE,
                                param.learning.method = "bayes",
                                parallelize = FALSE, cluster.type = "FORK", n.cores = NULL) {

  py <- prepare_Dataset_forDescriptiveBN(y)
  descbn <- buildCBN(py, structure.learning.algorithm = structure.learning.algorithm,
                     structure.learning.args.list = structure.learning.args.list,
                     param.learning.method = param.learning.method,
                     forbid.GG = FALSE, forbid.DD = FALSE,
                     compile.junction = compile.junction,
                     parallelize = parallelize, cluster.type = cluster.type,
                     n.cores = n.cores
                     )

  return(descbn)
}
