auc <- function(probabilities, real, name = "", is.event=FALSE, event = 1, not.event = 0,
                plot.curve = FALSE) {
  points <- 100

  if (is.event){
    ctR <- cTableRates(cTable(predicted = probabilities, real = real))
    roc.xvalues1 <- seq(from = 0, to = ctR$FPR, length.out = floor(points*ctR$FPR))
    roc.yvalues1 <- seq(from = 0, to = ctR$TPR, length.out = floor(points*ctR$FPR))
    roc.xvalues2 <- seq(from = ctR$FPR, to = 1 , length.out = floor(points*(1-ctR$FPR)))
    roc.yvalues2 <- seq(from = ctR$TPR, to = 1, length.out = floor(points*(1-ctR$FPR)) )
    roc.xvalues <- c(roc.xvalues1, roc.xvalues2)
    roc.yvalues <- c(roc.yvalues1, roc.yvalues2)
    disc <- seq(from = 0, to = 1, length.out =  points)
    auc_ <- integrate.xy(roc.xvalues ,roc.yvalues, use.spline = FALSE)
  }
  else{
    if (event != 1){
      real[real == event] <- 1
    }
    if (not.event != 0) {
      real[real == not.event] <- 0
    }
    if ( length(dim(probabilities))==3 ){
      p.event <- c(probabilities[ , as.character(event), ])
      real <- c(real)
    }
    else{
      p.event <- probabilities[ , as.character(event)]
    }
    if (length(real) != length(p.event)){stop("Real and probabilities mismatch.")  }

    auc_ <- roc.area(obs = real, pred = p.event)$A
  }

  if (plot.curve){
    roc.plot(real, pred = p.event, main = paste0(list(name, " AUC: ",
                                                      as.character(round(auc_, 2)))),
             xlab = "False Positive Rate", ylab = "True Positive Rate" , sub = as.character(auc_)
             )
  }

  return( auc_ )
}
