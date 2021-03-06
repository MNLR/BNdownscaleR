#' @export

aucStation <- function(downscaled, realData, is.event = FALSE,
                   plot.curves = TRUE, points = 100, return.YI = FALSE){
  # realData must be binary
  if (is.event){ station.names <- colnames(downscaled) }
  else{ station.names <- colnames(downscaled[1,,])}
  aucS <- array(0, NCOL(realData))
  YIS <- c()
  for (station in seq(1, NCOL(realData))) {
    if (!(all(is.na(realData[ , station])))){
      if (is.event){ inp <- downscaled[, station] }
      else{inp <- downscaled[, , station][ , "1"]}
      auc_ <- auc( probabilities =  inp,
                   name = station.names[station],
                   real = realData[, station],
                   is.event = is.event,
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
    } else {
      aucS[station] <- NA
    }
  }

  if (return.YI){
    names(YIS) <- station.names
    attr(aucS, "YIS") <- YIS
  }
  names(aucS) <- station.names
  attr(aucS, "summary") <- c(min(aucS), max(aucS), mean(aucS), sd(aucS))
  names(attr(aucS, "summary")) <- c("min", "max", "mean", "sd")
  return(aucS)
}
