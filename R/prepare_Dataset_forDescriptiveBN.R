#' @export
#'

prepare_Dataset_forDescriptiveBN <- function(y) {
  idS <- as.vector(sapply(y$Metadata$station_id, function(x){return(paste0("D.",x))})) # prepending of "D." is compulsory due to limitations in as.grain()
  data <- as.data.frame(y$Data)

  rownames(data) <- seq(1, nrow(y$Data))
  data <- data[ complete.cases(data) , ]
  for (j in 1:ncol(data)){
    data[, j] <- as.factor(data[ , j])
  }

  colnames(data) <- idS
  positions <- t(as.matrix(y$xyCoords))
  colnames(positions) <- idS
  return( list(data = data, positions = positions, Metadata = y$Metadata,
               nx = 0, ny = ncol(positions)) )
}
