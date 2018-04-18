#' @export

generateWeatherBN <- function( wg, initial = NULL, n = 1 ){
  # initial expects a data.frame with columns names EQUAL to name.convention
  # or | auto

  BN <- wg$BN
  BN.fit <- wg$BN.fit
  junction <- wg$junction
  epochs <- wg$dynamic.args.list$epochs
  NY <- wg$NY

  if (is.null(junction)) {
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
    simulated <- rbn(wg$BN.fit, n=1)[ , wg$names.distribution[[1]]$y.names]
    series <- matrix(0, nrow = 0, ncol = NY)
    series <- rbind(series, characterToOperableMatrix(simulated))

    simulated <- toOperableVector(simulated)
    pb = txtProgressBar(min = 1, max = n+epochs, initial = 1)
    setTxtProgressBar(pb, 1 )

    if (epochs > 2){
      predictors <- c()
      for (epoch in 2:(epochs-1)){
        predictors <- c(predictors, unlist(wg$names.distribution[[epoch-1]]))
        simulated2 <- queryBN(evidence = simulated, dbn = wg, evidence.nodes = predictors,
                             predictands = wg$names.distribution[[epoch+1]]$y.names,
                             type = "simulation"
                             )
        simulated2 <- simulated2[ match(wg$names.distribution[[epoch+1]]$y.names,
                                        colnames(simulated2)) ]
        series[epoch, ] <- characterToOperableMatrix(simulated2)
        simulated <- c(simulated, toOperableVector(simulated2))
        setTxtProgressBar(pb, epoch  )

      }
    }

    predictors <- unlist(wg$names.distribution[1:(epochs-1)])
    predictands <- wg$names.distribution[[length(wg$names.distribution)]]$y.names

    colnames(series) <- predictands


    if (n >= 2){

      for (epoch in 2:n){
        simulated2 <- queryBN(evidence = simulated, dbn = wg,
                              evidence.nodes = predictors,
                              predictands = predictands, type = "simulation"
                              )
        simulated <- c(simulated, toOperableVector(simulated2))
        simulated2 <- characterToOperableMatrix(simulated2)
        simulated2 <- simulated2[ , match(predictands, colnames(simulated2)) ]
        series <- rbind(series, simulated2)
        simulated <- simulated[(NY*(epochs-1)+1):length(simulated)]
        setTxtProgressBar(pb, epoch  + epochs)

      }
    }
  }

  colnames(series) <- unlist(strsplit(predictands,
                                      split = paste0(".T", as.character(epochs-1))
                                      )
                            )
  rownames(series) <- as.character(seq(1, n + epochs - 2))

  return(series)
}
