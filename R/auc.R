auc <- function(probabilities, name, real, is.event=FALSE, event = 1, not.event = 0,  points = 100,
                plot.curve = FALSE, color = "black", points.curve = FALSE, return.YI = FALSE ) {

  if (is.event){
    ctR <- cTableRates(cTable(predicted = probabilities, real = real))
    roc.xvalues1 <- seq(from = 0, to = ctR$FPR, length.out = floor(points*ctR$FPR))
    roc.yvalues1 <- seq(from = 0, to = ctR$TPR, length.out = floor(points*ctR$FPR))
    roc.xvalues2 <- seq(from = ctR$FPR, to = 1 , length.out = floor(points*(1-ctR$FPR)))
    roc.yvalues2 <- seq(from = ctR$TPR, to = 1, length.out = floor(points*(1-ctR$FPR)) )
    roc.xvalues <- c(roc.xvalues1, roc.xvalues2)
    roc.yvalues <- c(roc.yvalues1, roc.yvalues2)
    disc <- seq(from = 0, to = 1, length.out =  points)
  }
  else{
    disc <- seq(from = 0, to = 1, length.out =  points)
    occurence <- lapply(disc,
                        FUN = function( threshold , probabilities) {
                                  return( as.numeric(probabilities >= threshold) )
                              },
                        probabilities = probabilities )
    if (event != 1){
      real[real == event] <- 1
      occurence[occurence == event ] <- 1
    }
    if (not.event != 0) {
      real[real == not.event] <- 0
      occurence[occurence == not.event ] <- 0
    }
    ctables <- lapply( occurence ,  cTable, real = real)

    roc.xvalues <- rev( sapply( ctables, cTableRates, value = "FPR" ) )
    roc.yvalues <- rev( sapply( ctables, cTableRates, value = "TPR" ) )
  }

    auc <- integrate.xy(roc.xvalues ,  roc.yvalues, use.spline = FALSE)
    if (plot.curve){
      if (points.curve){
        points(roc.xvalues, roc.yvalues, type = "l",  main=paste0("ROC curve for ", name ),
             sub = paste("AUC =", as.character(auc)), col = color,
             xlab="False Positive Rate", ylab="True Positive Rate"
             )
      }
      else{
        plot(roc.xvalues, roc.yvalues, type = "l",  main=paste0("ROC curve for ", name ),
             sub = paste("AUC =", as.character(auc)), col = color,
             xlab="False Positive Rate", ylab="True Positive Rate"
             )
        lines(disc, disc, lty = 2)
      }
    }

    YoudensIndex <- which.max((1-roc.xvalues) + roc.yvalues)
    best.threshold <- disc[YoudensIndex]
    if (return.YI){
      return( list(auc = auc , YI = best.threshold) )
    }
    else{
      return( auc )
    }

}
