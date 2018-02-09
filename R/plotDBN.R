#' @title Plot Bayesian Network
#' @param DBN Downscaling Bayesian Network, as returned by build.downscalingBN(). This is a wrapper for plot.restrictedgraph.R
#' @author M.N. Legasa
#' @export

plotDBN <- function(DBN, nodes = -1, node.size = 1, edge.arrow.size = 0.25, dev = FALSE ){
  plotLatLonDAG( bn = DBN$BN , positions = DBN$positions, distance = DBN$bnlearning.args.list$distance,
                       nodes = nodes, node.size = node.size, edge.arrow.size = edge.arrow.size , dev = dev, xlab = "Longitude", ylab = "Latitude")
}
