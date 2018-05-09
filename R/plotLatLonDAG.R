#' @title Plot a Graph placing nodes in euclidean coordinates \code{positions}
#' @description
#' @author M.N. Legasa
#' @importFrom igraph graph_from_data_frame plot.igraph
#' @importFrom shape plotellipse
#' @export
#'
plotLatLonDAG <- function(bn, positions, distance = NULL, nodes = -1, no.colors = FALSE, no.labels = FALSE, vertex.label.dist = 1,
                          node.size = 4, edge.width = 0.5,  edge.arrow.size = 0.15, edges.color = c("gray", "black"),
                          dev = FALSE, axes = TRUE, xlab = "x", ylab = "y") {
  # Plots the graph of class bn with nodes in positions and shows the nodes dependance distance as a circle, for a given distance d assumed to be euclidean distance.
  #  ---- INPUT:
  # graph             An object of class bn whose Directed Acyclic Graph is going to be plotted.
  # positions         Sorted array of the locations of x, can only be 1 or 2 dimensional, must contain the same number of columns as the number of variables in x, and number of rows is the dimension.
  #                   Won't check column names, positions must be sorted the same way as the columns (Variables) of x, the data.frame used to learn graph.
  # distance          Maximum distance of dependancy. If no distance is given, no circle will be drawn (so nodes argument will be ignored)
  # nodes             Index of nodes whose dependancy is going to be shown, can be a vector for several nodes. By default, nodes = 0 plots circles for all nodes. -1 will plot no circle, still
  #                   plotting nodes in given positions.
  if (dev) {  dev.new()  }
  else { plot.new() }

  nodes_ <- names(bn$nodes)
  if (NROW(positions) == 1){
    positions <- rbind(positions, 0)
    minx <- min(positions)
    maxx <-  max(positions)
    miny <- -distance
    maxy <- distance
  }
  else {
    minx <- min(positions[1 , ])
    maxx <- max(positions[1 , ])
    miny <- min(positions[2 , ])
    maxy <- max(positions[2 , ])
  }

  Nnodes <- length(bn$nodes)

  NodeList <- data.frame(nodes_, positions[1, ] , positions[2, ])
  EdgeList <- data.frame(bn$arcs)
  a <- graph_from_data_frame(vertices = NodeList, d = EdgeList)

  if (length(nodes) == 1 && nodes == 0){
    nodes <- seq(1, Nnodes)
  }
  if (!no.colors){
    color <- c("red", "blue", "green", "yellow", "brown", "black", "pink", "cyan")
    color <- array( color, Nnodes)
    COL <-color[nodes]
  } else {COL <- "black"}
  if ( (length(nodes) == 1 && nodes != -1) | (length(nodes) != 1) ) {
    cpositions <- as.matrix(positions[ ,nodes] )
  }

  if (!is.null(edges.color)) {
    Nedges <- nrow(EdgeList)
    GtoD <- intersect(grep(pattern = "G", EdgeList[, 1]), grep(pattern = "D", EdgeList[, 2]) )
    DtoG <- intersect(grep(pattern = "D", EdgeList[, 1]), grep(pattern = "G", EdgeList[, 2]) )
    GtoG <- intersect(grep(pattern = "G", EdgeList[, 1]), grep(pattern = "G", EdgeList[, 2]) )
    # TXtoTY <- intersect(grep(pattern = "T", EdgeList[, 1]), grep(pattern = "G", EdgeList[, 2]))


    edge.color.pattern <- rep(edges.color[1], Nedges)
    if (length(edges.color) == 2) {
      edge.color.pattern[c(GtoD, DtoG, GtoG)] <- edges.color[2]
    }
    else if (length(edges.color) == 3){
      edge.color.pattern[c(GtoD, DtoG)] <- edges.color[2]
      edge.color.pattern[c(GtoG)] <- edges.color[3]
    }
    else if (length(edges.color) >= 4){
      edge.color.pattern[GtoD] <- edges.color[2]
      edge.color.pattern[DtoG] <- edges.color[3]
      edge.color.pattern[c(GtoG)] <- edges.color[4]
    }
  }

  if (no.labels){ plot.igraph(a, layout=t(positions),
                              vertex.size = node.size, vertex.label.dist = vertex.label.dist, vertex.color=COL, vertex.label=NA,  rescale=F,
                              edge.color = edge.color.pattern,
                              xlim=c(minx, maxx), ylim=c(miny, maxy), xlab = xlab, ylab = ylab, asp=FALSE , axes = axes,
                              edge.width = edge.width, edge.arrow.size = edge.arrow.size)
  } else { plot.igraph(a, layout=t(positions),
                       vertex.size = node.size, vertex.label.dist = vertex.label.dist, vertex.color=COL, rescale=F,
                       edge.color = edge.color.pattern,
                       xlim=c(minx, maxx), ylim=c(miny, maxy), xlab = xlab, ylab = ylab, asp = FALSE , axes = axes,
                       edge.width = edge.width, edge.arrow.size = edge.arrow.size)
  }

  if ( (length(nodes) == 1 && (nodes != -1 & !(is.null(distance))) ) | ( length(nodes) != 1 & !(is.null(distance)) )  ) {
    trash <- mapply(plotellipse, mid = split(cpositions, rep(1:ncol(cpositions), each = nrow(cpositions))), lcol = COL , MoreArgs = list( rx = distance, ry = distance, asp = FALSE))
  }

}

