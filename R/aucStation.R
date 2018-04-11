#' @export

aucStation <- function(prediction, realData, is.event = FALSE,
                       plot.curves = TRUE
                       ){
  # realData must be binary
  if (is.event){ station.names <- colnames(prediction) }
  else{ station.names <- colnames(prediction[1,,]) }
  aucS <- array(0, NCOL(realData))
  for (station in seq(1, NCOL(realData))) {
    if (!(all(is.na(realData[ , station])))){
      if (is.event){ inp <- prediction[, station] }
      else{inp <- prediction[, , station]}
      auc_ <- auc( probabilities =  inp,
                   real = realData[, station],
                   name = station.names[station],
                   is.event = is.event,
                   plot.curve = plot.curves
                   )
      aucS[station] <- auc_
    } else{
      aucS[station] <- NA
    }
  }

  names(aucS) <- station.names
  return(aucS)
}
