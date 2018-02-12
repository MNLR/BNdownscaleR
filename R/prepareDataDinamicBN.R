
prepareDataDinamicBN <- function(data, epochs){

  # Data conversion
  dinamic.data <- data$data[ 1:( nrow(data$data)-(epochs-1) ) , ]
  names <- colnames(data$data)
  nvars <- length(names)
  layers <- rep(0, nvars)

  names.distribution <- list( list(x.names = mapply( FUN = function(node, time) { return(paste0(node, paste0(".T", time))) }, node = data$x.names, time = rep(0, data$nx) )  ,
                                   y.names = mapply( FUN = function(node, time) { return(paste0(node, paste0(".T", time))) }, node = data$y.names, time = rep(0, data$ny) ))
                            )

  for (epoch in 1:(epochs-1)) {
    dinamic.data <- cbind.data.frame( dinamic.data  , data$data[ (epoch+1):(nrow(data$data)-(epochs-1-epoch)) , ] )
    names <- c(names, colnames(data$data))
    layers <- c(layers, rep(epoch, nvars))
    names.distribution[[epoch + 1]] <- list(x.names = as.vector(mapply( FUN = function(node, time) {
                                                                    return(paste0(node, paste0(".T", time))) }, node = data$x.names, time = rep(epoch, data$nx),
                                                              SIMPLIFY = TRUE
                                                              ))  ,
                                            y.names = as.vector(mapply( FUN = function(node, time) {
                                                                    return(paste0(node, paste0(".T", time))) }, node = data$y.names, time = rep(epoch, data$ny) )
                                                            ))
  }

  colnames(dinamic.data) <- mapply( FUN = function(node, time) { return(paste0(node, paste0(".T", time))) }, node = names, time = layers)
  data$data <- dinamic.data

  # Others
  data$x.names <- grep("^[G]", colnames(dinamic.data), value=TRUE)
  data$y.names <- grep("^[D]", colnames(dinamic.data), value=TRUE)
  data[["names.distribution"]] <- names.distribution
  return(data)
}
