parseStructureLearningStepsArg <- function(structure.learning.steps, dynamic, remove.past.G){
  if ( dynamic & length(structure.learning.steps) == 1 && structure.learning.steps == 2) {
    structure.learning.steps <- c("local-global", "past")
    print(paste0("Learning process set to default dynamic 2 step:", " c(\"local-global\", \"past\")"))
  }
  if ( dynamic & length(structure.learning.steps) == 1 && structure.learning.steps == 3) {
    structure.learning.steps <- c("local", "global", "past")
    print(paste0("Learning process set to default dynamic 3 step:", " c(\"local\", \"global\", \"past\")"))
  }
  if ( !dynamic & length(structure.learning.steps) == 1 && structure.learning.steps == 3) {
    stop("3 step learning is only applicable for Dynamic Bayesian Networks. Either set dynamic = TRUE or use 2 steps.")
  }
  if ( !dynamic & length(structure.learning.steps) == 1 && structure.learning.steps == 2){
    structure.learning.steps <- c("local", "global")
  }
  if ( !remove.past.G && (identical(structure.learning.steps, c("local-past", "global")) |
                          identical(structure.learning.steps, c("past-local", "global")) ||
                          identical(structure.learning.steps, c("local", "past", "global"))
  )   ) {stop("Use remove.past.G = TRUE to inject past nodes before G nodes.")}

  return(structure.learning.steps)
}


