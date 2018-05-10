splitSpellsNA <- function( ppfBNdata ) {
  # expects gaps to be given by row number

  data_ <- ppfBNdata$data
  days <- as.numeric(rownames(data_))
  spells <- list()
  sp.index <- 1
  i0 <- 1
  for (i in 1:(length(days)-1)){
    if ( (days[i]+1) != days[i+1] ){
      spells[[sp.index]] <- ppfBNdata
      spells[[sp.index]]$data <- data_[i0:i, , drop = FALSE]
      i0 <- i+1
      sp.index <- sp.index + 1
    }
  }

  class(spells) <- "splitSpellsNA"
  return(spells)
}
