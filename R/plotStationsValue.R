#' @export

plotStationsValue <- function(y, valueS, color.ramp.palette = c('red', 'green'), name, pch = 20, cex = 5, dev = TRUE) {
  if (nrow(y$xyCoords) != length(valueS)) { stop("valueS must have a single value per each station.") }

  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(colors = color.ramp.palette)
  Col <- rbPal(length(valueS))[as.numeric(cut( valueS, breaks = length(valueS)))]

  if (dev){
    dev.new()
  }
  plot(y$xyCoords$x, y$xyCoords$y, pch = pch, cex = cex, col = Col, xlab = "Longitude", ylab = "Latitude", main = "")
}
