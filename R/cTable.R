#' @export

cTable <- function(predicted, real){

  if (is.data.frame(real)){ real <- data.matrix(real) -1 }
  else { real <- data.matrix(real) }
  if (is.data.frame(predicted)){ predicted <- data.matrix(predicted) -1 }
  else { predicted <- data.matrix(predicted) }

  #if (!identical(names(table(real)), names(table(predicted)))){   # REMOVED due to mismatch when all 0, all 1
  #  stop("predicted and real events mismatch.")
  #}
  ct1 <- table( predicted - 2*real )
  r1 <- as.numeric(c(ct1["0"], ct1["-2"] ))
  r2 <- as.numeric(c(ct1["1"], ct1["-1"] ))
  CT <- matrix( c(r1,r2 ), ncol = 2, byrow = TRUE)
  colnames(CT) <- c("w0", "w1")
  rownames(CT) <- c("p0", "p1")
  CT[is.na(CT)] <- 0

  return(CT)
}
