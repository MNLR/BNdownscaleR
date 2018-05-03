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
    junction <- compileJunction( BN.fit )
  }

  if (is.null(initial)){
    initial <- rbn(wg$BN.fit, n=1)[ , unlist(wg$names.distribution[1:(epochs-1)])]
    series <- matrix(toOperableVector(initial), nrow = epochs-1, ncol = NY, byrow = TRUE)
  }
  else {
    series <- toOperableMatrix(initial)
  }

  predictors <- unlist(wg$names.distribution[1:(epochs-1)])
  predictands <- wg$names.distribution[[length(wg$names.distribution)]]$y.names
  colnames(series) <- predictands

  evidence_ <- c(t(series))
  tt_ <- system.time(queryBN(evidence = evidence_, dbn = wg,
                             evidence.nodes = predictors,
                             predictands = predictands, type = "simulation"
                             )
                     )[3]*n
  if (tt_ > 60){
    tt_ <- paste0(list(as.character(floor(tt_/3600)), " hours and ", as.character(
      floor((tt_ - floor(tt_/3600)*3600)/60) ),  " minutes."
      ), collapse = ''
    )
    print(paste0("Process will approximately take ", tt_))
  }

  print("Generating series")
  pb = txtProgressBar(min = 0, max = n, initial = 1, style = 3)
  setTxtProgressBar(pb, 0 )

  for (epoch in 1:n){
    simulated <- queryBN(evidence = evidence_, dbn = wg,
                         evidence.nodes = predictors,
                         predictands = predictands, type = "simulation"
                         )
    series <- rbind(series, toOperableVector(simulated))
    evidence_ <- c(t(series[ (nrow(series)-(epochs-2)):(nrow(series)) , ]))
    setTxtProgressBar(pb, epoch )
  }

  colnames(series) <- unlist(strsplit(predictands,
                                      split = paste0(".T", as.character(epochs-1))
                                      )
                            )
  rownames(series) <- seq(-(epochs-2), n)
  return(series)
}
