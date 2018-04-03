#' @export
aucDBNvs <- function( realData, downscaled, is.event = FALSE, downscaled2 = NULL, is.event2 = FALSE,
                       plot.curves = TRUE, color1 = "black", color2 = "red", points = 100){
  # realData must be binary
  if (is.event){ station.names <- colnames(downscaled) }
  else{ station.names <- colnames(downscaled[1,,])}

  aucS1 <- array(0, NCOL(realData))
  aucS2 <- array(0, NCOL(realData))

  for (station in seq(1, NCOL(realData))) {
    if (!(all(is.na(realData[ , station])))){

      if (is.event){ inp <- downscaled[, station] }
      else{inp <- downscaled[, , station][ , "1"]}
      aucS1[station] <- auc( probabilities =  inp,
                   name = station.names[station],
                   real = realData[, station],
                   is.event = is.event,
                   points = points, color = color1,
                   plot.curve = plot.curves,
                   return.YI = FALSE)

      if (is.event2){ inp <- downscaled2[, station] }
      else{inp <- downscaled2[, , station][ , "1"]}
      aucS2[station] <- auc( probabilities =  inp,
                   name = station.names[station],
                   real = realData[, station],
                   is.event = is.event2,
                   points = points, color = color2,
                   plot.curve = plot.curves, points.curve = TRUE,
                   return.YI = FALSE)

    } else{
      aucS1[station] <- NA
      aucS2[station] <- NA
    }
  }

  names(aucS1) <- station.names
  names(aucS2) <- station.names

  return(list(aucS1, aucS2))
}
