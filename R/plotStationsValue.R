#' @export

plotStationsValue <- function(y, valueS, numeric.values = FALSE, node.idS = FALSE, color.ramp.palette = c('red', 'green'), na.color = "black",
                              show.legend = TRUE, legend.breaks = 6, legend.loc = "topright", pch = 20, cex = 5, dev = TRUE) {
  if (nrow(y$xyCoords) != length(valueS)) { stop("valueS must have a single value per each station.") }

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

  if (numeric.values){
    text(y$xyCoords$x, y = y$xyCoords$y, labels = as.character(round(valueS, 2)))
  }

  if (node.idS){
    text(y$xyCoords$x, y = y$xyCoords$y, labels = y$Metadata$station_id)
  }
  if (show.legend){
    if (!is.null(attributes(valueS)$measure)){
      title_ <- attributes(valueS)$measure
      print
    } else {title_ <- NULL}
    valueS <- sort(valueS, decreasing = TRUE)
    Col <- sort(Col)
    Col <- Col[2:(length(Col))]
    valueS <- valueS[2:length(valueS)]
    legend.valueS <- round(valueS[seq(1,length(valueS), by = legend.breaks)], 2)
    legend.Col <- Col[seq(1,length(valueS)+1, by = legend.breaks)]
    legend(legend.loc, legend = legend.valueS, pch=16, col = legend.Col, title = title_)
  }
}



