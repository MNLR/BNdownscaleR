#' @title Plot Bayesian Network
#' @param DBN Downscaling Bayesian Network, as returned by build.downscalingBN().
#' @author M.N. Legasa
#' @export

plotDBN <- function(DBN, nodes = -1, node.size = 1, edge.arrow.size = 0.25, break.axis = 1, separation.ratio = 0.1, dev = FALSE, Nlabels = 4){

  if (!(is.null(DBN$dynamic.args.list))){
    DBN$positions <- reallocateDynamicNodes(DBN$positions, names.distribution = DBN$names.distribution, break.axis, DBN$dynamic.args.list$epochs,
                                            separation.ratio = separation.ratio)

    sep <- attributes(DBN$positions)$separation
    if (DBN$dynamic.args.list$remove.past.G){ # purged past G nodes
      nx <- DBN$NX
      ny <- DBN$NY
      epS <- DBN$dynamic.args.list$epochs

      purge.index <- 1:nx
      aux.purge.index <- purge.index
      #if (epS > 2){
      #  for (ep in 1:(epS-2)){
      #    purge.index <- c(purge.index , aux.purge.index + (nx+ny))
      #  }
      #}
      #DBN$positions <-DBN$positions[ , -purge.index]
    }

    axes <- FALSE
  } else { axes <- TRUE }

  plotLatLonDAG( bn = DBN$BN , positions = DBN$positions, distance = DBN$structure.learning.args.list$distance,
                       nodes = nodes, node.size = node.size, edge.arrow.size = edge.arrow.size,
                 dev = dev, xlab = "Longitude", ylab = "Latitude", axes)

  if (!(is.null(DBN$dynamic.args.list))){ # Aditional operations for dynamic node placement:
    mn <- min(DBN$positions[break.axis, ])
    mx <- max(DBN$positions[break.axis, ])
    range <- abs( mx - mn )
    for (i in 1:(DBN$dynamic.args.list$epochs - 1)){
      abline(v = c(mn + i*range/DBN$dynamic.args.list$epochs))
    }

    # Fix broken axis:
    N.atempnodes <- DBN$NX + DBN$NY
    eps <- DBN$dynamic.args.list$epochs

    min.axis <- min(DBN$positions[ break.axis , (ncol(DBN$positions)-N.atempnodes):ncol(DBN$positions)])
    max.axis <- max(DBN$positions[ break.axis , (ncol(DBN$positions)-N.atempnodes):ncol(DBN$positions)])

    range <- abs(max.axis - min.axis)

    label.positions <- seq(min.axis, max.axis, by = range/Nlabels)
    aux.label.positions <- label.positions

    for (ep in 1:(eps-1)){
      label.positions <- c(label.positions, aux.label.positions + ep*sep)
    }

    labelS <- sprintf(rep(aux.label.positions, eps), fmt = '%#.1f')
    axis(break.axis, at=label.positions, labels=labelS)
    axis(as.numeric(xor(1,break.axis-1)) + 1)
    # Broken axis fixed
  }
}
