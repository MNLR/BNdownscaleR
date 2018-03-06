#' @export

cvBN <- function(x, y, folds = 4, type = "chronological", threshold.vector = NULL, event = "1",
                 scale = FALSE, global.vars = NULL, PCA = NULL, combined.only = TRUE, local.predictors = NULL,
                 ...) {

  y <- getTemporalIntersection(obs = y,prd = x, "obs" )
  x <- getTemporalIntersection(obs = y,prd = x, "prd" )
  data <- dataSplit(x,y, f = folds, type = type)

  p <- lapply(1:length(data), FUN = function(xx) {
    print(paste0("Fold:", xx ,"... "))
    xT <- data[[xx]]$train$x ; yT <- data[[xx]]$train$y
    xt <- data[[xx]]$test$x  ; yt <- data[[xx]]$test$y

    if (isTRUE(scale)) {
      xt <- localScaling(xt, base = xT, scale = scale)
      xT <- localScaling(xT, base = xT, scale = scale)
    }
    grid <- prepare_predictors(x = xT, y = yT, global.vars, PCA, combined.only, local.predictors)
    xT <- prepare_predictors.forBN(grid,
                                   rm.na = TRUE, rm.na.mode = "observations")
    xt <- prepare_newdata(newdata = xt, predictor = grid)
    model <- build.downscalingBN(xT, ...)

    #predx <- getTemporalIntersection(obs = y, prd = x, which.return = "prd")
    #ty <- getTemporalIntersection(obs = y, prd = x, which.return = "obs")
    #test <- prepare_newdata(newdata = predx,
    #                        predictor = grid)

    prob.py <- downscale.BN(DBN = model, x = xt, prediction.type = "probabilities", parallelize = TRUE, cluster.type = "FORK")

    if (is.null(threshold.vector)){ threshold.vector  <- 1 - model$marginals[event, ] }
    event.py <- lapply(prob.py, FUN = is.mostLikely, event = event, threshold.vector =  threshold.vector)


    return(list(prob = prob.py, event = event.py))
  }
  )

  probS <- lapply(p, FUN = function(x) {return(x$prob)} )
  evS <- lapply(p, FUN = function(x) {return(x$event)} )

  for (i in 1:length(probS[[1]]) ){
    prob <- lapply(1:length(probS[[1]]), FUN = function(member, probS) { return(lapply(probS, FUN = function(fold){ return(fold[[member]]) })) }, probS = probS)
    event <- lapply(1:length(evS[[1]]), FUN = function(member, evS) { return(lapply(evS, FUN = function(fold){ return(fold[[member]]) })) }, evS = evS)
  }

  prob <- lapply(prob, FUN = function(member) {do.call(abind, args = list(member, along = 1))} )
  event <- lapply(event, FUN = function(member) {do.call(rbind, member)} )

  return(list(prob = prob, event = event))
  }
