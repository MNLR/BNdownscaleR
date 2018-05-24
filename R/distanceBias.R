#' @export

distanceBias <- function(real,  predictions = NULL, mdist = NULL,
                         measure = "phiCoef", return.measureMatrix = TRUE,
                         title = NULL, ylab = NULL,
                         colors = c("black", "red", "blue", "green", "cyan","yellow","pink"),
                         alpha_ = 0.25, lwd = 5, cex = 1.5,
                         show.legend = TRUE, legend_ = NULL,
                         ...) {

  # predictions may be a list of matrices, a matrix or NULL for just plotting real.
  # measure may be a list of measureMatrix outputs.

  if ( is.null(mdist) && is.list(real) && !is.null(real$xyCoords) ){
    mdist <- measureMatrix(real, measure = "distance")
  } else {
    if (is.null(mdist)){ stop("real$xyCoords is not present and no mdist was provided.") }
  }

  if (!is.matrix(real)){
    real <- measureMatrix(real, measure = measure, ... = ...)
  }

  if (!is.null(predictions)) {   # CORREGIR AQUI
    if (!is.list(predictions)){
        predictions <- list( measureMatrix(predictions, measure = measure, ... = ...) )
    }
    else {
        predictions <- lapply(predictions, FUN = measureMatrix, measure = measure, ... = ...)
    }
    ylim <- c( min( unlist(c(real, predictions)), na.rm = TRUE ),
               max( unlist(c(real, predictions)), na.rm = TRUE )
    )
  }
  else {ylim = NULL}

  ylab <- measure
  plotDistanceMeasure(real, mdist = mdist, ylim = ylim, title = title, ylab = ylab,
                      color = colors[1], alpha_ = alpha_, lwd = lwd, cex = cex, pch = 16)

  if (!is.null(predictions)){
    mapply(plotDistanceMeasure, predictions, color = colors[2:(length(predictions)+1) ],
           MoreArgs = list( mdist = mdist, ylim = ylim, points = TRUE, title = NULL,
                            alpha_ = alpha_, lwd = lwd, cex = cex, pch = 16, ... = ... )
          )
  }
  if (show.legend && !(is.null(legend_))){
    legend("topright", legend = legend_, col = colors, pch = 16)
  }

  if (return.measureMatrix){
    return( list(real = real, predictions = predictions, mdist = mdist) )
  }
}


plotDistanceMeasure <- function(measure.matrix, mdist, ylim, ylab = NULL,
                                points = FALSE, title = NULL,
                                color = "black", alpha_ = 0.25, lwd = 5,
                                cex = 1.5, pch = 16, ... ) {

  D <- data.frame(mm = as.vector(measure.matrix), dist = as.vector(mdist))

  smf <- loess(mm ~ dist, D)
  loessx <- seq(min(D$dist, na.rm = TRUE), max(D$dist, na.rm = TRUE), length.out = 10000)
  loessy <- predict(smf, loessx)

  if (!points){
    plot(mdist, measure.matrix, main = title, xlab = "Distance", ylab = ylab,
         cex=cex, pch = pch , col = adjustcolor(color, alpha.f = alpha_), ylim = ylim
    )
  } else {
    points(mdist, measure.matrix, main = title, xlab = "Distance",
           cex=cex, pch = pch , col = adjustcolor(color, alpha.f = alpha_), ylim = ylim
    )
  }
  points(loessx, loessy, type = 'l', lwd = lwd, col = color)
}
