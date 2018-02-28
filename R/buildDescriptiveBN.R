#'
#' @export

buildDescriptiveBN <- function(y, structure.learning.algorithm =  "hc",
                               structure.learning.args.list = list(),
                               compile.junction = TRUE,
                               output.marginals = TRUE,
                               param.learning.method = "bayes",
                               parallelize = FALSE, cluster.type = "FORK", n.cores = NULL) {

  py <- prepare_Dataset_forDescriptiveBN(y)
  descbn <- build.downscalingBN(py, structure.learning.algorithm = structure.learning.algorithm,
                      structure.learning.args.list = structure.learning.args.list,
                      param.learning.method = param.learning.method, forbid.GG = FALSE, forbid.DD = FALSE,
                      compile.junction = compile.junction,
                      output.marginals = output.marginals,
                      parallelize = parallelize, cluster.type = cluster.type, n.cores = n.cores)

  return(descbn)
}
