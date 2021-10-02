#' Delete multiple columns from a data.table
#'
#' @param dt a data.table
#' @param cols a vector of column names
#'
#' @return a new data.table with specified columns deleted

DTnull<-function(dt,cols){
  dt[, (cols) := lapply(cols, function(x)
  {
    do.call(function(y) NULL ,c(list(get(x))))
  })]
  dt
}
