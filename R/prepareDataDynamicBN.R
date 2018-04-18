#' prepareDataDinamicBN()
#' @title
#' @description
#' @param data Expects output from \code{\link[downscaleR.BN]{prepare_predictors.forBN()}}.
#' @details
#' @return
#' @author MN Legasa
#' @export
#' @examples

prepareDataDynamicBN <- function(data, epochs){

  # Data conversion
  dinamic.data <- data$data[ 1:( nrow(data$data)-(epochs-1) ) , ]
  names <- colnames(data$data)
  nvars <- length(names)
  layers <- rep(0, nvars)

  if (data$nx != 0){
    names.distribution <- list( list(x.names = as.vector(mapply( FUN = function(node, time) {
      return(paste0(node, paste0(".T", time))) }, node = data$x.names, time = rep(0, data$nx) )),
                                     y.names = as.vector(mapply( FUN = function(node, time) {
                                       return(paste0(node, paste0(".T", time)))
                                     }, node = data$y.names, time = rep(0, data$ny)) ))
                              )


    for (epoch in 1:(epochs-1)) {
      dinamic.data <- cbind.data.frame( dinamic.data  , data$data[ (epoch+1):(nrow(data$data)-(epochs-1-epoch)) , ] )
      names <- c(names, colnames(data$data))
      layers <- c(layers, rep(epoch, nvars))
      names.distribution[[epoch + 1]] <- list(
                                            x.names = as.vector(mapply(
                                            FUN = function(node, time) {
                                                    return(paste0(node, paste0(".T", time)))
                                                  },
                                            node = data$x.names,
                                            time = rep(epoch, data$nx),
                                            SIMPLIFY = TRUE
                                            )),
                                            y.names = as.vector(mapply(
                                              FUN = function(node, time) {
                                                      return(paste0(node,
                                                        paste0(".T", time))
                                                      )
                                                    },
                                              node = data$y.names, time = rep(epoch, data$ny)
                                              )
                                              )
                                            )
    }
    colnames(dinamic.data) <- mapply( FUN = function(node, time) {
      return(paste0(node, paste0(".T", time)))
    }, node = names, time = layers
    )
  }
  else {
    names.distribution <- rep(list(list(y.names = names)), epochs)
    for (epoch in 1:(epochs)){
      if (epoch != epochs){
        dinamic.data <- cbind.data.frame( dinamic.data,
                                          data$data[ (epoch+1):(nrow(data$data)-(epochs-1-epoch)), ]
                                        )
        layers <- c(layers, rep(epoch, nvars))
      }
      epnames <- paste0(names.distribution[[epochs]]$y.names, paste0(".T", epoch-1))
      names.distribution[[epoch]]$y.names <- epnames
    }
    colnames(dinamic.data) <- unlist(names.distribution)
  }


  data$data <- dinamic.data

  # Others
  data[["names.distribution"]] <- names.distribution
  data[["x.names"]] <- NULL
  data[["y.names"]] <- NULL
  data[["positions"]] <- t(rep(1, epochs) %x% t(data$positions))
  colnames(data$positions) <- colnames(dinamic.data)
  rownames(data$positions) <- c("x","y")

  return(data)
}
