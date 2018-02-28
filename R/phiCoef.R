#' @export

phiCoef <- function( data, st1, st2, remove.nan =  TRUE, evidence.nodes = NULL, evidence = NULL){

  Data <- data$Data

  node.names <- data$Metadata$station_id
  if (is.character(st1)){ st1<- which(node.names == st1) }
  if (is.character(st2)){ st2<- which(node.names == st2) }
  if (!is.null(evidence.nodes)){
    for (i in 1:length(evidence.nodes)){
      if (is.character(evidence.nodes[i])){
        evidence.nodes[i] <- which(node.names == evidence.nodes[i])
      }
    }
    evidence.nodes <- as.numeric(evidence.nodes)
  }

  if (remove.nan){ Data <- Data[complete.cases(Data), ] }

  if (!is.null(evidence.nodes)){
    for (i in 1:length(evidence.nodes)){
      Data <- Data[ which(Data[ , evidence.nodes[i]] == evidence[i] ) ,  ]
    }
  }

  Data <- Data[ , c(st1, st2)]


  n11 <- as.numeric(table(Data[ ,1] == 1 & Data[ ,2] == 1)["TRUE"])
  if (is.na(n11)) {n11 <- 0}
  n00 <- as.numeric(table(Data[ ,1] == 0 & Data[ ,2] == 0)["TRUE"])
  if (is.na(n00)) {n00 <- 0}
  n10 <- as.numeric(table(Data[ ,1] == 1 & Data[ ,2] == 0)["TRUE"])
  if (is.na(n10)) {n10 <- 0}
  n01 <- as.numeric(table(Data[ ,1] == 0 & Data[ ,2] == 1)["TRUE"])
  if (is.na(n01)) {n01 <- 0}

  n1_ <- as.numeric(table(Data[ ,1] == 1)["TRUE"])
  n0_ <- as.numeric(table(Data[ ,1] == 0)["TRUE"])
  n_1 <- as.numeric(table(Data[ ,2] == 1)["TRUE"])
  n_0 <- as.numeric(table(Data[ ,2] == 0)["TRUE"])

  return( (n11*n00 - n10*n01)/(sqrt(n1_)*sqrt(n0_)*sqrt(n_1)*sqrt(n_0)) )
}
