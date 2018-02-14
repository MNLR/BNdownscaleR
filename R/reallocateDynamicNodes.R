
reallocateDynamicNodes <- function(positions, break.axis = 1, Nepochs, separation.ratio = 0.1){

  range <- abs(max(positions[break.axis, ]) - min(positions[break.axis,]))
  separation <- range + range*separation.ratio

  auxpos <- positions
  auxnames <- colnames(positions)
  names <- as.vector(sapply(auxnames, FUN = function(x) { return(paste0(x, ".T0") ) } ))

  for (i in 1:(Nepochs -1)){
    auxpos[break.axis, ] <- auxpos[break.axis, ] + separation
    positions <- cbind(positions, auxpos)
    names <- c(names, as.vector(sapply(auxnames, FUN = function(x, ep) { return( paste0( x, paste0(".T", as.character(ep)) ) ) }, ep = i) ))
  }
  colnames(positions) <- names
  attr(positions, 'separation') <- separation
  return(positions)
}
