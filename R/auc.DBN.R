#' @export
auc.DBN <- function(downscaled, realData, points = 100, plot.curves = TRUE){
  # Data must be binary
  station.names <- colnames(downscaled[1,,])
  aucS <- array(0, NCOL(realData))
  for (station in seq(1, NCOL(realData))) {
    if (!(all(is.na(realData[ , station])))){
      aucS[station] <-  auc( probabilities =   downscaled[, , station][ , "1"],
                             name = station.names[station],
                             real = realData[, station],
                             points = points,
                             plot.curve = plot.curves )
    } else{
        aucS[station] <- NA
    }
  }
  names(aucS) <- station.names
  return(aucS)
}
