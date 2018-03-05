#' @export

distance <- function( data, st1, st2 ){

  node.names <- data$Metadata$station_id

  if (is.character(st1)){ st1<- which(node.names == st1) }
  if (is.character(st2)){ st2<- which(node.names == st2) }

  x1 <- data$xyCoords[st1, 1]
  x2 <- data$xyCoords[st2, 1]
  y1 <- data$xyCoords[st1, 2]
  y2 <- data$xyCoords[st2, 2]

  dst <- sqrt((x1-x2)^2+(y1-y2)^2)
  attr(dst, "cases") <- Inf

  return(dst)
}
