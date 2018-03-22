initializeDummyGreylist <- function( slal , blackOrWhite ){
  if ( is.null(slal[blackOrWhite]) ){
    list <- matrix(, nrow = 0, ncol = 2)
    colnames(list) <- c("from", "to")
    slal[[blackOrWhite]] <-  list
  }
  return(slal)
}
