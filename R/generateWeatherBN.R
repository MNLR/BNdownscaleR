#' @export

generateWeatherBN <- function( wg, initial = NULL, n = 1, inference.type = NULL,
                               advance.type = "simulation"){
  # initial expects a data.frame with columns names EQUAL to name.convention
  # or | auto

  BN <- wg$BN
  BN.fit <- wg$BN.fit
  junction <- wg$junction
  epochs <- wg$dynamic.args.list$epochs
  NY <- wg$NY

  if (is.null(junction) && !(is.null(inference.type))) {
    print("Junction was not compiled at training stage.")
    junction <- compileJunction( BN.fit )
    print("Done.")
  }

  if (!(is.null(initial))){
    stop("NOT YET.")
    predictors <- colnames(initial)
    simulated <- initial
  }
  else{
    print("Generating weather...")
    simulated <- rbn(wg$BN.fit, n=1)[ , unlist(wg$names.distribution[1:(epochs-1)])]
    series <- matrix(toOperableVector(simulated), nrow = epochs-1, ncol = NY, byrow = TRUE)

    pb = txtProgressBar(min = 1, max = n+epochs, initial = 1)
    setTxtProgressBar( pb, 1 )

    predictors <- unlist(wg$names.distribution[1:(epochs-1)])
    predictands <- wg$names.distribution[[length(wg$names.distribution)]]$y.names

    colnames(series) <- predictands

    evidence_ <- c(t(series))
    setTxtProgressBar(pb, epochs)
    for (epoch in 1:n){
      simulated <- queryBN(evidence = evidence_, dbn = wg,
                            evidence.nodes = predictors,
                            predictands = predictands, type = "simulation"
                           )
      series <- rbind(series, toOperableVector(simulated))
      evidence_ <- c(t(series[ (nrow(series)-(epochs-2)):(nrow(series)) , ]))
      setTxtProgressBar(pb, epoch + epochs + 1 )
    }
  }

  colnames(series) <- unlist(strsplit(predictands,
                                      split = paste0(".T", as.character(epochs-1))
                                      )
                            )
  rownames(series) <- seq(-(epochs-2), n)
  return(series)
}
