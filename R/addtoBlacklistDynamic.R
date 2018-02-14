
addtoBlacklistDynamic <- function(structure.learning.args.list, names.distribution,
                                  forbid.backwards, forbid.dynamic.GD, forbid.dynamic.global.arcs, forbid.global.arcs, forbid.local.arcs, forbid.back.DD){
  if ( is.null(structure.learning.args.list$blacklist) ){
    blacklist <- matrix(, nrow = 0, ncol = 2)
    colnames(blacklist) <- c("from", "to")
    structure.learning.args.list[["blacklist"]] <-  blacklist
  }

  epochs <- length(names.distribution)
  if (forbid.backwards) {
    aux <- Filter(Negate(is.null), names.distribution[2:epochs]) # purges NULL elements
    blacklist.list <- mapply(FUN = function(froms, epoch, tos ) {return( buildBlacklist(as.vector(unlist(froms)), as.vector(unlist(tos[1:epoch])), bidirectional = FALSE) ) },
                             froms = aux, epoch = seq(1:epochs-1),
                             MoreArgs = list(tos = names.distribution)
    )
    structure.learning.args.list$blacklist <- rbind(structure.learning.args.list$blacklist, do.call(rbind, blacklist.list))
  }
  if (forbid.dynamic.GD) {
    blacklist.list <- mapply(FUN = function(froms, tos, epoch) {
                                      if (!(is.null(froms$x.names))){ # can be NULL if past G nodes have been purged
                                        return(buildBlacklist( froms$x.names, as.vector(sapply(tos[-epoch], function(x) {x$y.names})) , bidirectional = TRUE))
                                      }
                                    },
                             froms = Filter(Negate(is.null), names.distribution[1:epochs]), epoch = seq(1:epochs),
                             MoreArgs = list(tos = names.distribution),
                             SIMPLIFY = FALSE
    )
    structure.learning.args.list$blacklist <- rbind(structure.learning.args.list$blacklist, do.call(rbind, blacklist.list))
  }
  if (forbid.dynamic.global.arcs) {
    blacklist.list <- mapply(FUN = function(froms, tos, epoch) {
                                     return(buildBlacklist( froms$x.names, as.vector(sapply(tos[-(1:epoch)], function(x) {x$x.names})), bidirectional = TRUE ))
                                   },
                             froms = Filter( Negate(is.null), names.distribution[1:(epochs-1)] ), epoch = seq(1:(epochs-1)),
                             MoreArgs = list(tos = names.distribution),
                             SIMPLIFY = FALSE
    )
  structure.learning.args.list$blacklist <- rbind(structure.learning.args.list$blacklist, do.call(rbind, blacklist.list))
  }

  if (forbid.back.DD){

    structure.learning.args.list <- add.toBlacklist(
      as.vector(unlist(lapply( Ddata$names.distribution[1:(epochs-1)], function(epoch) {return(epoch$y.names)}))),
      structure.learning.args.list
    )

    #for (epoch in 1:(length(names.distribution)-1)){
    #  structure.learning.args.list <- add.toBlacklist(names.distribution[[epoch]]$y.names, structure.learning.args.list)
    #}
  }

  if (forbid.global.arcs){
    for (epoch in 1:length(names.distribution)){
      if (!(is.null(names.distribution[[epoch]]$x.names))){ # Can be NULL if past G nodes have been purged
        structure.learning.args.list <- add.toBlacklist(names.distribution[[epoch]]$x.names, structure.learning.args.list)
      }
    }
  }
  if (forbid.local.arcs){
    for (epoch in 1:length(names.distribution)){
      structure.learning.args.list <- add.toBlacklist(names.distribution[[epoch]]$y.names, structure.learning.args.list)
    }
  }
  return(structure.learning.args.list)
}
