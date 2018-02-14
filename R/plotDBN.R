#' @title Plot Bayesian Network
#' @param DBN Downscaling Bayesian Network, as returned by build.downscalingBN(). This is a wrapper for plot.restrictedgraph.R
#' @author M.N. Legasa
#' @export

plotDBN <- function(DBN, nodes = -1, node.size = 1, edge.arrow.size = 0.25, break.axis = 1, separation.ratio = 0.1, dev = FALSE, Nlabels = 3){

  par(xpd=TRUE)
  if (!(is.null(DBN$dynamic.args.list))){
    DBN$positions <- reallocateDynamicNodes(positions, break.axis, DBN$dynamic.args.list$epochs, separation.ratio = separation.ratio)
    axes <- FALSE
  } else { axes <- TRUE }

  plotLatLonDAG( bn = DBN$BN , positions = DBN$positions, distance = DBN$bnlearning.args.list$distance,
                       nodes = nodes, node.size = node.size, edge.arrow.size = edge.arrow.size , dev = dev, xlab = "Longitude", ylab = "Latitude", axes)

  if (!(is.null(DBN$dynamic.args.list))){
    mn <- min(DBN$positions[break.axis, ])
    mx <- max(DBN$positions[break.axis, ])
    range <- abs( mx - mn )
    for (i in 1:(DBN$dynamic.args.list$epochs - 1)){
      abline(v = c(mn + i*range/DBN$dynamic.args.list$epochs))
    }

    # Fix broken axis:
    N.atempnodes <- NCOL(DBN$positions)/DBN$dynamic.args.list$epochs
    print(N.atempnodes)
    sep <- attributes(DBN$positions)$separation
    eps <- DBN$dynamic.args.list$epochs

    min.axis <- min(DBN$positions[ break.axis , 1:N.atempnodes])
    max.axis <- max(DBN$positions[ break.axis , 1:N.atempnodes])
    print(min.axis)
    print(max.axis)
    range <- abs(max.axis - min.axis)
    label.sep <- range/Nlabels

    label.positions <- seq(min.axis, max.axis, by = label.sep)
    aux.label.positions <- label.positions
    for (ep in 1:(eps-1)){
        label.positions <- c(label.positions, aux.label.positions + ep*sep)
    }
    print(label.positions)
    print("labels")

    labelS <- sprintf(rep(aux.label.positions, eps), fmt = '%#.1f')
    axis(break.axis, at=label.positions, labels=labelS)
    axis(as.numeric(xor(1,break.axis-1)) + 1)

    # Broken axis fixed
    print(labelS)

  }
}
