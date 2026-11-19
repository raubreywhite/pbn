data_clean <- function(d) {
  data.table::setnames(d, tolower(names(d)))
  return(d)
}
