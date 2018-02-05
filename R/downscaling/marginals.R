marginals <- function(dbn, junction = NULL){
  BN <- dbn$BN
  BN.fit <- dbn$BN.fit
  Nglobal <- dbn$NX

  predictands <- names(BN$nodes)[- (1:Nglobal) ]
  if (is.null(junction)){
    junction <- compile( as.grain(BN.fit) )
  }

  MPT <- simplify2array(querygrain(junction, nodes = predictands))

  return( MPT[ ,match(predictands, colnames(MPT))] )
}
