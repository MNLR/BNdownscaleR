#' @title NA removal
#' @param data_ loadeR data to be purged
#' @author M.N. Legasa
#' @export

removeNA <- function(data_) {
  cc <- complete.cases(data_$Data)
  data_$Data <- data_$Data[cc, ]
  data_$Dates$start <- data_$Dates$start[cc]
  data_$Dates$end <- data_$Dates$end[cc]
  return(data_)
}
