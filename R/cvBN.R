#' @export

cvBN <- function(x, y, folds = 4, type = "chronological", threshold.vector = NULL, plot.DBN = TRUE, return.model = FALSE,
                 event = "1",
                 cl = NULL,
                 ...) {
  preNAy <- length(y$Dates$start)
  preNAx <- length(x$Dates$start)
  y <- filterNA(y)
  x <- filterNA(x)
  if (preNAy != length(y$Dates$start)){
    warning(
      paste0(list(as.character(preNAy - length(y$Dates$start)),
                  " observations with NA values have been removed from y. ",
                  length(y$Dates$start), " left.")), immediate. = TRUE
      )
  }
  if (preNAx != length(x$Dates$start)){
    warning(
      paste0(list(as.character(preNAx - length(x$Dates$start)),
                  " observations with NA values have been removed from x. ",
                  length(x$Dates$start), " left." )), immediate. = TRUE
      )
  }

  y <- getTemporalIntersection(obs = y, prd = x, "obs" )
  x <- getTemporalIntersection(obs = y, prd = x, "prd" )
  data <- dataSplit(x,y, f = folds, type = type)

  p <- lapply(1:length(data), FUN = function(xx, return.model, ...) {
      print(paste0("Fold ", xx ,"... "))
      xT <- data[[xx]]$train$x ; yT <- data[[xx]]$train$y
      xt <- data[[xx]]$test$x  ; yt <- data[[xx]]$test$y

      grid <- prepareData(x = xT, y = yT)
      xT <- prepare_predictors.forBN(grid, rm.na = TRUE, rm.na.mode = "observations")
      xt <- prepareNewData(newdata = xt, predictor = grid)
      model <- build.downscalingBN(xT, ...)

      if (plot.DBN){
        plotDBN(model)
      }

      prob.py <- downscale.BN(DBN = model, x = xt, prediction.type = "probabilities", parallelize = TRUE,
                              cl = cl, cluster.type = "FORK")

      if (is.null(threshold.vector)){ threshold.vector  <- 1 - model$marginals[event, ] }
      event.py <- lapply(prob.py, FUN = is.mostLikely, event = event, threshold.vector =  threshold.vector)

      if (return.model){
        return(list(prob = prob.py, event = event.py, model = model))
      } else { return(list(prob = prob.py, event = event.py)) }
    }, return.model = return.model, ... = ...
  )

  probS <- lapply(p, FUN = function(x) {return(x$prob)} )
  evS <- lapply(p, FUN = function(x) {return(x$event)} )

  if (return.model){
    modelS <- lapply(p, FUN = function(x) {return(x$model)} )
  } else { modelS <- NULL}

  for (i in 1:length(probS[[1]]) ){
    prob <- lapply(1:length(probS[[1]]),
                   FUN = function(member, probS) {
                     return(lapply(probS, FUN = function(fold){ return(fold[[member]]) })) }, probS = probS)
    event <- lapply(1:length(evS[[1]]), FUN = function(member, evS) { return(lapply(evS, FUN = function(fold){ return(fold[[member]]) })) }, evS = evS)
  }

  prob <- lapply(prob, FUN = function(member) {do.call(abind, args = list(member, along = 1))} )
  event <- lapply(event, FUN = function(member) {do.call(rbind, member)} )

  return(list(prob = prob, event = event, real = y, modelS = modelS))
  }
