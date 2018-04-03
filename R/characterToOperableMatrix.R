#' @export
characterToOperableMatrix <- function(matrix, type = "numeric") {
  factor.matrix <- as.data.frame(matrix(nrow = dim(matrix)[1], ncol = dim(matrix)[2] ))
  colnames(factor.matrix) <- colnames(matrix)
  if (type == "factor"){
    for (i in 1:ncol(matrix)){
      factor.matrix[ ,i] <- factor(matrix[ , i], levels = as.numeric(names(table(matrix[ , i]))),
                                   labels = names(table(matrix[ , i])))
    }
  }
  else{
    for (i in 1:ncol(matrix)){
      factor.matrix[ ,i] <- as.numeric(matrix[ , i])
    }
  }
  return(factor.matrix)
}
