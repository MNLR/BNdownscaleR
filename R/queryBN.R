#' @export

queryBN <- function( ev, predictors, predictands, junction, type = "exact") {

  if (type == "exact"){
    evid <- setEvidence(junction, predictors, as.character(ev)) # Evidence must be provided as character
    print(evid)
    return( querygrain( object = evid, nodes = predictands, type = "marginal") )
  }
  else if (type == "simulation"){

  }
  else if (type == "hybrid"){
    stop("Not yet.")
  }
  else if (type == "inexact"){

  }
  else {
    stop("Please use a valid inference type: exact, simulation, hybrid, inexact.")
  }
}
