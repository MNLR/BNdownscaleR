#' @title Parallelization Cluster Starter
#' @param type Cluster type, either FORK or PSOCK.
#' @author M.N. Legasa
#' @importFrom parallel detectCores makeCluster
#' @export

parallel.starter <- function(type, n.cores,
                             PSOCK.funcExports.list = list(),
                             PSOCK.varExports.list = list()){

  if ( is.null(n.cores) ){
    n.cores <- floor(detectCores()-1)
  }

  # Initiate cluster
  print("Starting cluster...")
  cl <- makeCluster( n.cores, type = type )

  if (type == "PSOCK") {
    #PSOCK.varExportsNames.list = list()
    #for ( i in 1:length(PSOCK.varExports.list) ) {
    #  PSOCK.varExportsNames.list[[i]] <- deparse(quote( PSOCK.varExports.list[[i]] ))
    #}
    PSOCK.exports <- c(PSOCK.funcExports.list, PSOCK.varExports.list)
    print(PSOCK.exports)
    clusterExport(cl, PSOCK.exports, envir = environment())
  }

  print("Cluster good to go.")

  return(cl)
}

