#' Read PostGIS table directly, using DBI and binary conversion
#' 
#' Read PostGIS table directly through DBI and RPostgreSQL interface, converting binary
#' @param conn open database connection
#' @param table table name
#' @param query SQL query to select records
#' @param geom_column character or integer: indicator of name or position of the geometry column; if not provided, the last column of type character is chosen
#' @param EWKB logical; is the WKB is of type EWKB? defaults to TRUE if \code{conn} is of class code{PostgreSQLConnection} or \code{PqConnection}
#' @examples 
#' if (Sys.getenv("USER") %in% c("travis", "edzer")) {
#'   library(RPostgreSQL)
#'   conn = dbConnect(PostgreSQL(), dbname = "postgis")
#'   st_read_db(conn, query = "select * from meuse limit 3;")
#'   dbDisconnect(conn)
#' }
#' @name st_read
#' @details in case geom_column is missing: if table is missing, this function will try to read the name of the geometry column from table \code{geometry_columns}, in other cases, or when this fails, the geom_column is assumed to be the last column of mode character.
#' @export
st_read_db = function(conn = NULL, table, query = paste("select * from ", table, ";"),
		geom_column = NULL, EWKB, ...) {
	if (is.null(conn))
		stop("no connection provided")
  	# suppress warning about unknown type "geometry":
	suppressWarnings(tbl <- dbGetQuery(conn, query))
	if (is.null(geom_column)) { # try find the geometry column:
		gc = try(dbReadTable(conn, "geometry_columns"))
		geom_column = if (class(gc) == "try-error" || missing(table))
				tail(which(sapply(tbl, is.character)), 1) # guess it's the last character column
			else
				gc [ gc$f_table_name == table, "f_geometry_column"]
	}
	if (missing(EWKB))
		EWKB = inherits(conn, "PostgreSQLConnection") || inherits(conn, "PqConnection")
    tbl[[geom_column]] = st_as_sfc(structure(tbl[[geom_column]], class = "WKB"), EWKB = EWKB)
	st_as_sf(tbl, ...)
}

#' Write simple feature table to a spatial database
#' 
#' Write simple feature table to a spatial database
#' @param conn open database connection
#' @param obj object of class \code{sf}
#' @param table name for the table in the database
#' @param geom_name name of the geometry column in the database
#' @param ... arguments passed on to \code{dbWriteTable}
#' @param dropTable logical; should \code{table} be dropped first?
#' @param binary logical; use well-known-binary for transfer?
#' @export
#' @examples
#' if (Sys.getenv("USER") %in% c("travis", "edzer")) {
#'   library(sp)
#'   data(meuse)
#'   sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
#'   library(RPostgreSQL)
#'   conn = dbConnect(PostgreSQL(), dbname = "postgis")
#'   st_write_db(conn, sf, "meuse_tbl", dropTable = FALSE)
#' }
st_write_db = function(conn = NULL, obj, table = substitute(obj), geom_name = "wkb_geometry",
		..., dropTable = FALSE, binary = TRUE) {
	if (is.null(conn))
		stop("if no provided")
	if (dropTable)
		dbGetQuery(conn, paste("drop table", table, ";"))
	df = obj
	df[[attr(df, "sf_column")]] = NULL
	class(df) = "data.frame"
	if (dropTable)
		dbSendQuery(conn, paste("drop table ", table, ";"))
	dbWriteTable(conn, table, df, ...)
	geom = st_geometry(obj)
	DIM = nchar(class(geom[[1]])[1]) # FIXME: is this correct? XY, XYZ, XYZM
	SRID = attr(obj, "epsg")
	if (is.null(SRID) || is.na(SRID))
		SRID = 0
	TYPE = class(geom[[1]])[2]
	query = paste0("SELECT AddGeometryColumn('','", table, "','", geom_name, 
			"','", SRID, "','", TYPE, "',", DIM, ");")
	dbGetQuery(conn, query)
	rn = row.names(obj)
	if (! binary) {
		wkt = st_as_text(geom)
		for (r in seq_along(rn)) {
			cmd = paste0("UPDATE ", table, " SET ", geom_name, 
				" = ST_GeomFromText('", wkt[r], "') WHERE \"row.names\" = '", rn[r], "';")
			dbGetQuery(conn, cmd)
		}
	} else {
		wkb = st_as_binary(geom)
		for (r in seq_along(rn)) {
			cmd = paste0("UPDATE ", table, " SET ", geom_name, " = '", CPL_raw_to_hex(wkb[[r]]), 
				"' WHERE \"row.names\" = '", rn[r], "';")
			dbGetQuery(conn, cmd)
		}
	}
}
