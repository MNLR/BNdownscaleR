#' @export
dependenceMatrix <- function( bin.data , measure = "phiCoef"){

  mS <- matrix(0, nrow = ncol(bin.data), ncol = ncol(bin.data))
  for (i in 1:42){
    for (j in 1:42){
      mS[i,j] <- do.call(measure, list(xy = bin.data[ , c(i,j)]))
    }
  }

  return(mS)
}
