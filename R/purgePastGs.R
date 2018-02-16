
#' @export

purgePastGs <- function(Ddata){

  nx <- Ddata$nx
  ny <- Ddata$ny
  epS <- ncol(Ddata$data)/ncol(Ddata$positions)

  purge.index <- 1:nx
  aux.purge.index <- purge.index

  if (epS > 2){
    for (ep in 1:(epS-2)){
      purge.index <- c(purge.index , aux.purge.index + (nx+ny))
      Ddata$names.distribution[[ep]]$x.names <- NULL
    }
  }

  Ddata$names.distribution[[epS-1]]$x.names <- NULL
  Ddata$data <- Ddata$data[ , -purge.index]

  return(Ddata)
}
