#' @export
#'

prepare_Dataset_forDescriptiveBN <- function(y) {
  idS <- as.vector(sapply(y$Metadata$station_id, function(x){return(paste0("D.",x))})) # prepending of "D." is compulsory due to limitations in as.grain()
  data <- as.data.frame(y$Data[ complete.cases(y$Data) , ])
  data[] <- lapply( data, factor) # the "[]" keeps the dataframe structure
  names(data) <- idS
  positions <- t(as.matrix(y$xyCoords))
  colnames(positions) <- idS
  return( list(data = data, positions = positions, Metadata = y$Metadata,
               nx = 0, ny = ncol(positions)) )
}
