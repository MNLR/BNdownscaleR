#' @export

cTable <- function(predicted, real, split.per.column = FALSE){
  if (!is.data.frame(predicted)){
    predicted <- as.data.frame(predicted)
    #warning("predicted is not a data.frame. It has been converted.", immediate. = TRUE)
  }
  if (!is.data.frame(real)){
    real <- as.data.frame(real)
    #warning("predicted is not a data.frame. It has been converted.", immediate. = TRUE)
  }

  tableS <- mapply(FUN = cTable1, predicted = predicted, real = real, SIMPLIFY = FALSE)
  if (!split.per.column){
    tableS <- Reduce('+', tableS)
  }
  return( tableS )
}
