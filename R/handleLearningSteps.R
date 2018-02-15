
#' @export
#'
handleLearningSteps <- function(data, structure.learning.steps, dynamic) {
  POS <- data$positions
  NX <- data$nx
  NY <- data$ny

  if (!dynamic){
    if (length(structure.learning.steps) == 1 && structure.learning.steps == 2){
      return( list(DATA = data$data[ , (NX+1):(NX+NY)], POS = POS[ , (NX+1):(NX+NY)], structure.learning.steps = 1) )
    } else { stop("For non Dynamic Bayesian Networks use either one step learning with structure.learning.steps = 1
                    or two step learning with structure.learning.steps = 2") }
  }
  else { # Dynamic Bayesian Network
    if ( length(structure.learning.steps) == 1 && structure.learning.steps == 2) {
      structure.learning.steps <- c("local-global", "past")
      print(paste0("Learning process set to default dynamic 2 step:", " c(\"local-global\", \"past\")"))
    }
    if ( length(structure.learning.steps) == 1 && structure.learning.steps == 3) {
      structure.learning.steps <- c("local", "global", "past")
      print(paste0("Learning process set to default dynamic 3 step:", " c(\"local\", \"global\", \"past\")"))
    }

    Nsteps <- length(structure.learning.steps)
    Nepochs <- length(data$names.distribution)

    if (Nsteps == 2){
      if (all(structure.learning.steps == c("local-global", "past"))){
        selected <- as.vector(unlist(data$names.distribution[[Nepochs]]))
        return( list(DATA = data$data[ , selected], POS = POS, structure.learning.steps = 1) )
      }
      if (all(structure.learning.steps == c("local-past", "global"))){
        selected <- as.vector(unlist(lapply(data$names.distribution, function(x) { return(x$y.names) })))
        return( list(DATA = data$data[ , selected], POS = POS[ , (NX+1):(NX+NY)], structure.learning.steps = 1) )
      }
      if (all(structure.learning.steps == c("local", "past-global"))){
        selected <- data$names.distribution[[Nepochs]]$y.names
        return( list(DATA = data$data[ , selected], POS = POS[ , (NX+1):(NX+NY)], structure.learning.steps = 1) )
      }
    }

    else { # 3 STEPS
      if (all(structure.learning.steps == c("local", "global", "past"))){
        selected <- data$names.distribution[[Nepochs]]$y.names
        return( list(DATA = data$data[ , selected], POS = POS[ , (NX+1):(NX+NY)], structure.learning.steps = c("local-global", "past")) )
      }
      if (all(structure.learning.steps == c("local", "past", "global"))){
        selected <- data$names.distribution[[Nepochs]]$y.names
        return( list(DATA = data$data[ , selected], POS = POS[ , (NX+1):(NX+NY)], structure.learning.steps = c("local-past", "global")) )
      }
    }
  }
}
