#' Create a dynamic data.
#'
#' table

#' @name dynDT
#' @param x  \code{[any]}
#' @return \code{dynDT}: \code{[R6(dynDataTable)]}
#' @examples

#'  dynDT(x = mtcars)
#' @export
dynDT <- function(x) {
    # Create a dynamic data.table
    assert_any(x, checkmate::checkDataFrame(),  checkmate::checkDataTable(),  checkmate::checkMatrix())
    dynDataTable$new(data = x)

    # Returns: \code{[R6(dynDataTable)]}
}
