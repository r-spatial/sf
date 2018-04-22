#' Deprecated functions in `sf`
#'
#' These functions are provided for compatibility with older version of `sf`.
#' They may eventually be completely removed.
#' @md
#' @rdname sf-deprecated
#' @name sf-deprecated
#' @param conn open database connection
#' @param table table name
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
	.Deprecated("st_read")
	st_read(dsn = conn, layer = table, query = query,
			geom_column = geom_column, EWKB = EWKB, ...)
}

#' @inheritDotParams dbWriteTable
#' @export
st_write_db <- function(conn = NULL, obj, table = deparse(substitute(obj)), ...,
						drop = FALSE, append = FALSE) {
	.Deprecated("st_write")
	st_write(obj = obj, dsn = conn, layer = table, ..., overwrite = drop, append = append)
}
