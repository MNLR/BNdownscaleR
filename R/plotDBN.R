#' @title Plot Bayesian Network
#' @param DBN Downscaling Bayesian Network, as returned by build.downscalingBN(). This is a wrapper for plot.restrictedgraph.R
#' @author M.N. Legasa
#' @export

plotDBN <- function(DBN, nodes = -1, node.size = 1, edge.arrow.size = 0.25, break.axis = 1, separation.ratio = 0.1, dev = FALSE){

  if (!(is.null(DBN$dynamic.args.list))){
    DBN$positions <- reallocateDynamicNodes(positions, break.axis, DBN$dynamic.args.list$epochs, separation.ratio = separation.ratio)

  }

  print(DBN$positions)
  plotLatLonDAG( bn = DBN$BN , positions = DBN$positions, distance = DBN$bnlearning.args.list$distance,
                       nodes = nodes, node.size = node.size, edge.arrow.size = edge.arrow.size , dev = dev, xlab = "Longitude", ylab = "Latitude")

  if (!(is.null(DBN$dynamic.args.list))){
    mn <- min(DBN$positions[break.axis, ])
    mx <- max(DBN$positions[break.axis, ])
    range <- abs( mx - mn )
    print(range)
    for (i in 1:(DBN$dynamic.args.list$epochs - 1)){
      abline(v = c(mn + i*range/DBN$dynamic.args.list$epochs))
    }
  }
}
