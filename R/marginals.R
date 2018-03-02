marginals <- function(dbn, junction = NULL){
  BN <- dbn$BN
  BN.fit <- dbn$BN.fit
  Nglobal <- dbn$NX

  if (Nglobal > 0){
    predictands <- names(BN$nodes)[- (1:Nglobal) ]
  } else {predictands <- names(BN$nodes)}
  if (is.null(junction)){
    junction <- compile( as.grain(BN.fit) )
  }

  MPT <- simplify2array(querygrain(junction, nodes = predictands))

  return( MPT[ ,match(predictands, colnames(MPT))] )
}
