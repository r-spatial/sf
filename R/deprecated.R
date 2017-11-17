#' Deprecated functions in `sf`
#'
#' These functions are provided for compatibility with older version of `sf`.
#' They may eventually be completely removed.
#' @md
#' @rdname sf-deprecated
#' @name sf-deprecated
#' @param ... Parameters to be passed to the modern version of the function
#' @docType package
#' @export  st_read_db st_write_db
#' @aliases st_read_db, st_write_db
#' @section Details:
#' \tabular{rl}{
#'   \code{st_read_db} \tab now a synonym for \code{\link{st_read}}\cr
#'   \code{st_write_db} \tab now a synonym for \code{\link{st_write}}\cr
#' }
#'
st_read_db <- function(...) {
	.Deprecated("st_read")
	st_read(...)
}

st_write_db <- function(...) {
	.Deprecated("st_write")
	st_write(...)
}
