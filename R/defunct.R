#' Deprecated functions in `sf`
#'
#' These functions are provided for compatibility with older version of `sf`.
#' They may eventually be completely removed.
#' @md
#' @rdname sf-defunct
#' @name sf-defunct
#' @param conn open database connection
#' @param table table name
#' @param geom_column deprecated. Geometry column name
#' @details The `geom_column` argument is deprecated. The function will
#' automatically find the `geometry` type columns. For the `RPostgreSQL` drivers
#' it will try to cast all the character columns, which can be long for very wide
#' tables.
#' @inheritParams st_read
#' @docType package
#' @export  st_read_db st_write_db
#' @aliases st_read_db, st_write_db
#' @section Details:
#' \tabular{rl}{
#'   \code{st_read_db} \tab now a synonym for \code{\link{st_read}}\cr
#'   \code{st_write_db} \tab now a synonym for \code{\link{st_write}}\cr
#' }
#'
#' @export
st_read_db <- function(conn = NULL, table = NULL, query = NULL,
					   geom_column = NULL, EWKB = TRUE, ...) {
	.Defunct("st_read")
}

#' @inheritDotParams dbWriteTable
#' @export
st_write_db <- function(conn = NULL, obj, table = deparse(substitute(obj)), ...,
						drop = FALSE, append = FALSE) {
	.Defunct("st_write")
}
