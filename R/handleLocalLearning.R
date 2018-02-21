#' @export
handleLocalLearning <- function(data, structure.learning.args.list, dynamic, keep.dynamic.distance,
                                exceptions = NULL){
  distance <- structure.learning.args.list$distance
  positions <- data$positions
  if ( !(dynamic) ){
    node.names <- colnames(positions)
    print(node.names)
    blacklist <- build.distanceBlacklist( node.names, positions, distance, exceptions = exceptions,
                                          debug = FALSE )
  }
  else if (!keep.dynamic.distance){
    to.be.Blacklisted.list <- lapply(data$names.distribution, function(x) {return(as.vector(unlist(x)))})
    blacklist.list <- lapply(to.be.Blacklisted.list,
                             FUN = function(names.per.epoch, positions, distance) {
                                    print(names.per.epoch)
                                    print(positions[ , names.per.epoch ])
                                    return( build.distanceBlacklist(names.per.epoch,
                                      positions = positions[ , names.per.epoch ],  distance)
                                    )
                                  }, positions = positions, distance = distance
                             )

    blacklist <- do.call(rbind, blacklist.list)
  }
  else{
    blacklist <- build.distanceBlacklist(as.vector(unlist(data$names.distribution)), positions,
                                         distance = distance)
  }

  if (is.null(structure.learning.args.list$blacklist) ) { structure.learning.args.list[["blacklist"]] <- blacklist }
  else { structure.learning.args.list$blacklist <- rbind(structure.learning.args.list$blacklist, blacklist) }

  return(structure.learning.args.list)

}
