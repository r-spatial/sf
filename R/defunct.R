#' @title Deprecated functions in `sf`
#' @name sf-defunct
#' @description
#' These functions are provided for compatibility with older version of `sf`.
#' They will eventually be completely removed.
#' 
#' * Use [st_read()] instead of `st_read_db()`.
#' * Use [st_write()] instead_of `st_write_db()`
#' @param conn open database connection
#' @param table table name
#' @param geom_column deprecated. Geometry column name
#' @details The `geom_column` argument is deprecated. The function will
#' automatically find the `geometry` type columns. For the `RPostgreSQL` drivers
#' it will try to cast all the character columns, which can be long for very wide
#' tables.
#' @inheritParams st_read
#' @export
#' @keywords internal
st_read_db <- function(conn = NULL, table = NULL, query = NULL,
					   geom_column = NULL, EWKB = TRUE, ...) {
	.Defunct("st_read")
}

#' @rdname sf-defunct
#' @inheritParams DBI::dbWriteTable
#' @export
st_write_db <- function(conn = NULL, obj, table = deparse(substitute(obj)), ...,
						drop = FALSE, append = FALSE) {
	.Defunct("st_write")
}
