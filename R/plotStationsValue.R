#' @export

plotStationsValue <- function(y, valueS, node.idS = TRUE, color.ramp.palette = c('red', 'green'), na.color = "black", pch = 20, cex = 5, dev = TRUE) {
  if (nrow(y$xyCoords) != length(valueS)) { stop("valueS must have a single value per each station.") }

  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(colors = color.ramp.palette)
  Col <- rbPal(length(valueS))[as.numeric(cut( valueS, breaks = length(valueS)))]

  if (dev){
    dev.new()
  }
  plot(y$xyCoords$x, y$xyCoords$y, pch = pch, cex = cex, col = Col, xlab = "Longitude", ylab = "Latitude", main = "")

  # mark NAs
  naS <- which(is.na(valueS))
  if (length(naS)>0){
    points(y$xyCoords$x[naS], y$xyCoords$y[naS], pch = pch, cex = cex, col = na.color)
  }

  if (node.idS){
    text(y$xyCoords$x, y = y$xyCoords$y, labels = y$Metadata$station_id)
  }

  #legend("topright", legend=valueS, pch=16, col=unique(Col))
}



