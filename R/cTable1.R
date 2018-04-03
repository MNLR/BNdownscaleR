cTable1 <- function(predicted, real){
  if (!is.factor(predicted) & !is.factor(real)) {
    predicted <- as.numeric(predicted)
    real <- as.numeric(real)
  } else {
    predicted <- as.numeric(predicted)
    real <- as.numeric(real)
    warning(list("Either predicted or real is factor.
            Conversion to numeric has been employed.
            Printing table(predicted) and table(real)... \n predicted:", names(table(predicted)),
                 ". real:", names(table(real))),
            immediate. = TRUE)
  }
  ct1 <- table( predicted - 2*real )
  r1 <- as.numeric(c(ct1["0"], ct1["-2"] ))
  r2 <- as.numeric(c(ct1["1"], ct1["-1"] ))
  CT <- matrix( c(r1,r2 ), ncol = 2, byrow = TRUE)
  colnames(CT) <- c("w0", "w1")
  rownames(CT) <- c("p0", "p1")
  CT[is.na(CT)] <- 0
  return(CT)
}
