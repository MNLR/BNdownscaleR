#' @export

queryBN <- function( ev, predictors, junction, predictands, type = "exact") {

  if (type == "exact"){
    evid <- setEvidence(junction, predictors, as.character(ev)) # Evidence must be provided as character
    #return(evid)
    return( querygrain(evid, nodes = predictands, type = "marginal") )
  }
  else if (type == "simulation"){

  }
  else if (type == "hybrid"){

  }
  else if (type == "inexact"){

  }
  else {
    stop("Please use a valid inference type: exact, simulation, hybrid, inexact.")
  }
}
