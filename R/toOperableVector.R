toOperableVector <- function( vector_ ) {
  #requires one dimensional thing

  if (is.data.frame(vector_)) {
    vector_ <- sapply( vector_, function(x) {return(as.numeric(as.character(x)))} )
  }
  else if (is.matrix(vector_)){
    vector_ <- apply(vector_, MARGIN = 2,
                     FUN = function(x) {return(as.numeric(as.character(x)))}
                     )

  }
  else if (is.vector(vector_)) {
    vector_ <- as.numeric(as.character(vector_))
  }
  return( vector_ )
}
