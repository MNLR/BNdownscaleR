#' @export

findClimatologyThreshold <- function(Probability.Table, event.marginalS, event =  "1",
                                               points = 100){
  disc <- seq(0, 1, length.out = points)
  climatologieS <- lapply(disc,
                          FUN = function(thr, Probability.Table, event){
                                         evS <- convertEvent(Probability.Table = Probability.Table,
                                                             event = event, threshold.vector = thr
                                                            )
                                         return( apply(X = evS, MARGIN = 2,
                                                       FUN = function(estev){
                                                              tab <- table(estev)
                                                              if (is.na(tab[event])){
                                                                nn <- 0
                                                              } else {
                                                                nn <- tab[event]
                                                              }
                                                              nn/sum(tab)
                                                              }
                                                       )
                                                )
                                  }, Probability.Table = Probability.Table, event = event
                          )
  climatologieS <- as.data.frame(t(sapply(X = climatologieS, c)))
  bestS <- mapply(FUN = function(estev, marginal){
                           return(
                             unique(
                               estev[which(min(abs(estev - marginal)) == abs(estev - marginal) )]
                             )
                           )
                        }, estev = climatologieS, marginal = event.marginalS
           )

  return(bestS)

}
