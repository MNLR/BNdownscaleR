compileJunction <- function( bn.fit ){
  print("Compiling junction...")
  junction <- tryCatch(
    {
      compile(as.grain(bn.fit), propagate = TRUE)
    },
    error=function(cond) {
    message("Warning: junction could not be compiled. Cause: ")
    message(cond)
    return(NA)
    }
  )
  print("Done.")
  return(junction)
}
