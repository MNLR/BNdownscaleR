#' @export
auc.DBN <- function(downscaled, realData, points = 100, plot.curves = TRUE, return.YI = FALSE){
  # Data must be binary
  station.names <- colnames(downscaled[1,,])
  aucS <- array(0, NCOL(realData))
  YIS <- c()
  for (station in seq(1, NCOL(realData))) {
    if (!(all(is.na(realData[ , station])))){
      auc_ <-auc( probabilities =   downscaled[, , station][ , "1"],
                  name = station.names[station],
                  real = realData[, station],
                  points = points,
                  plot.curve = plot.curves,
                  return.YI = return.YI)
      if (return.YI){
        aucS[station] <- auc_$auc
        YIS[station] <- auc_$YI
      }
      else {
        aucS[station] <- auc_
      }
    } else{
        aucS[station] <- NA
    }
  }

  if (return.YI){
    names(YIS) <- station.names
    attr(aucS, "YIS") <- YIS
  }
  names(aucS) <- station.names
  return(aucS)
}
