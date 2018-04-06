compileJunction <- function( bn.fit ){
  print("Compiling junction...")
  junction <- tryCatch(
    {
      compile(as.grain(bn.fit), propagate = TRUE)
      print("Done.")
    },
    error=function(cond) {
    message("Warning: junction could not be compiled. Cause: ")
    message(cond)
    return(NA)
    }
  )
}
