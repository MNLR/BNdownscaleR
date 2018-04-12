#' @export

distanceBias <- function(real, prediction, mdist = NULL, third = NULL, fourth = NULL,
                         measure = "phiCoef",
                         val.r = NULL, val.p = NULL, val.t = NULL, val.f = NULL,
                         title = NULL, ylab = NULL,
                         plot_ = TRUE,
                         colpred = "red", colreal = "black", colthird="blue", colfourth="green",
                         alpha_ = 0.25, lwd=5, cex =1.5,
                         show.legend = TRUE, legend_ = NULL,
                         ...) {

  if ( is.null(mdist) && is.list(real) && !is.null(real$xyCoords) ){
    mdist <- measureMatrix(real, measure = "distance")
  } else {
    if (is.null(mdist)){ stop("real$xyCoords is not present and no mdist was provided.") }
  }
  if ( is.null(val.r) ){
    val.r <- measureMatrix(real, measure = measure, ... = ...)
  }
  if ( is.null(val.p) ){
    val.p <- measureMatrix(prediction, measure = measure, ... = ...)
  }

  ylim <- c(min(c(as.vector(val.r), as.vector(val.p)), na.rm = TRUE), max(c(as.vector(val.r), as.vector(val.p)), na.rm = TRUE))

  # REAL
  D <- data.frame(mm=as.vector(val.r), dist=as.vector(mdist))
  smf <- loess(mm~ dist, D)
  rx <- seq(min(D$dist, na.rm = TRUE), max(D$dist, na.rm = TRUE), length.out = 10000)
  ry <- predict(smf, rx)

  # PREDICTS
  D <- data.frame(mm=as.vector(val.p), dist=as.vector(mdist))
  smf <- loess(mm~ dist, D)
  px <- seq(min(D$dist, na.rm = TRUE), max(D$dist, na.rm = TRUE), length.out = 10000)
  py <- predict(smf, px)

  # THIRD
  if (!is.null(third)){
    val.t <- measureMatrix(third, measure = measure, ... = ...)
    D <- data.frame(mm=as.vector(val.t), dist=as.vector(mdist))
    smf <- loess(mm~ dist, D)
    tx <- seq(min(D$dist, na.rm = TRUE), max(D$dist, na.rm = TRUE), length.out=10000)
    ty <- predict(smf, tx)
  }
  # FOURTH
  if (!is.null(fourth)){
    val.f <- measureMatrix(fourth, measure = measure, ... = ...)
    D <- data.frame(mm=as.vector(val.f), dist=as.vector(mdist))
    smf <- loess(mm~ dist, D)
    fx <- seq(min(D$dist, na.rm = TRUE), max(D$dist, na.rm = TRUE), length.out=10000)
    fy <- predict(smf, fx)
  }



  if (plot_) {
    if (is.null(ylab)){ ylab <- measure }
    plot(mdist, val.r, main = title, xlab = "Distance", ylab = ylab, cex=cex, pch = 16, col = adjustcolor(colreal, alpha.f = alpha_), ylim = ylim)
    points(mdist, val.p, cex=cex, pch = 16, col = adjustcolor(colpred, alpha.f = alpha_))
    points(rx, ry, type = 'l', lwd=lwd, col = colreal)
    points(px, py, type = 'l', lwd=lwd, col = colpred)

    colS <- c(colreal, colpred)
    if (!is.null(third)) {
      points(mdist, val.t, cex=cex, pch = 16, col = adjustcolor(colthird, alpha.f = alpha_))
      points(tx, ty, type = 'l', lwd=lwd, col = colthird)
      colS <- c(colS, colthird)
    }
    if (!is.null(fourth)) {
      points(mdist, val.f, cex=cex, pch = 16, col = adjustcolor(colfourth, alpha.f = alpha_))
      points(fx, fy, type = 'l', lwd=lwd, col = colfourth)
      colS <- c(colS, colfourth)
    }

    if (show.legend){
      legend("topright", legend = legend_, col = colS, pch = 16)
    }

  }
}
