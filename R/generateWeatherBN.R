#' @export

generateWeatherBN <- function( wg, initial = NULL, n = 1, x = NULL, inference.type = NULL,
                               advance.type = "simulation"){
  # expects x in the form prepare_newdata(newdata = tx, predictor = grid)
  # n overriden when x is not NULL.

  BN <- wg$BN
  BN.fit <- wg$BN.fit
  junction <- wg$junction
  epochs <- wg$dynamic.args.list$epochs
  NY <- wg$NY
  NX <- wg$NX

  if (!is.null(x)){
    if (is.list(x)){
      #todo: paralelizar members si hay más de uno
      return(
        lapply(x$x.global, FUN = function(member, wg, initial, inference.type,
                                          advance.type) {
                                  return(
                                    generateWeatherBN(wg = wg, initial = initial,
                                                      x = member,
                                                      inference.type = inference.type,
                                                      advance.type = advance.type)
                                  )
                                 },
               wg = wg, initial = initial, inference.type = inference.type,
               advance.type = advance.type
             )
      )
    } else if (!is.matrix(x)){
      stop("Provide a valid x.")
    }
  }

  if (is.null(junction) && !(is.null(inference.type))) {
    message("Warning: Compiling junction tree for weather generators might be (and usually
            is) unefficient.")
    junction <- compileJunction( BN.fit )
  }

  predictors <- as.vector(unlist(lapply(wg$names.distribution[1:(epochs-1)],
                                        function(x) {return(x$y.names)}
                                        )
                                 )
                          )
  predictands <- as.vector(wg$names.distribution[[length(wg$names.distribution)]]$y.names)

  if (!(is.null(x))) {
    predictorsG <- as.vector(
                    unlist(lapply(wg$names.distribution,
                      function(xx) {
                        return(xx$x.names)
                      }
                      )
                    )
    )

    predictors <- c(predictorsG, predictors)
    past.present.G <- sapply(wg$names.distribution,
                             function(x) return(length(x$x.names))
                             )
  }

  if (is.null(initial)){
    if (is.null(x)){
      initial <- rbn(wg$BN.fit, n=1)[ , unlist(wg$names.distribution[1:(epochs-1)])]
      series <- matrix(toOperableVector(initial), nrow = epochs-1, ncol = NY, byrow = TRUE)
      evidence_ <- c(t(series))
    }
    else {
      x.at <- sum(past.present.G)/NX
      evidence_ <- c( c(t(x))[1:sum(past.present.G)] )
      initial <- queryBN(evidence = evidence_, dbn = wg, evidence.nodes = predictorsG,
              predictands = as.vector(
                unlist(wg$names.distribution[1:(length(wg$names.distribution)-1)]
                       )
              )
      )
      initial <- toOperableVector(
        sapply(initial, function(x) {
          return(names(x)[order(x, decreasing = TRUE)][1] )
          }
        )
      )
      initial <- toOperableMatrix(initial)
      evidence_ <- c(c(t(x[1:x.at, ])), initial)
    }
    n <- nrow(x) - (x.at)
    x <- x[(x.at+1):nrow(x), ]
  }
  print("st")
  series <- toOperableMatrix(initial)
  colnames(series) <- predictands    #evidence_ <- c( c(t(x))[1:sum(past.present.G)], evidence_ )

  tt_ <- system.time(queryBN(evidence = evidence_, dbn = wg,
                             evidence.nodes = predictors,
                             predictands = predictands, type = advance.type
                             )
                     )[3]*n
  if (tt_ > 60){
    tt_ <- paste0(list(as.character(floor(tt_/3600)), " hours and ", as.character(
      floor((tt_ - floor(tt_/3600)*3600)/60) ),  " minutes."
      ), collapse = ''
    )
    print(paste0("Process will approximately take ", tt_))
  }

  print("Generating series...")
  pb = txtProgressBar(min = 0, max = n, initial = 1, style = 3)
  setTxtProgressBar(pb, 0)
   print(n)
   print(nrow(x))
  for (epoch in 1:n){
    simulated <- queryBN(evidence = evidence_, dbn = wg,
                         evidence.nodes = predictors,
                         predictands = predictands, type = advance.type
                        )
    series <- rbind(series, toOperableVector(simulated))
    evidence_ <- c(t(series[ (nrow(series)-(epochs-2)):(nrow(series)) , ]))
    if (!is.null(x) && epoch != n){
      evidence_ <- c(t( x[1:x.at, ] ), evidence_)
      x <- x[2:nrow(x), , drop = FALSE]
    }
    setTxtProgressBar(pb, epoch)
  }

  colnames(series) <- unlist(strsplit(predictands,
                                      split = paste0(".T", as.character(epochs-1))
                                      )
                            )
  rownames(series) <- seq(-(epochs-2), n)
  return(series)
}
