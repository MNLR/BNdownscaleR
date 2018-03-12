#' @export

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

  MPT <-sapply(predictands, function(pred, junction) {querygrain(junction, nodes = pred)[[1]]}, junction = junction)

  return( MPT )
}
