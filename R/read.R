#' read simple features from file or database
#'
#' read simple features from file or database
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder)
#' @param layer layer name (varies by driver, may be a file name without extension)
#' @param ... parameter(s) passed on to \link{st_as_sf}
#' @param quiet logical; suppress info on name, driver, size and spatial reference
#' @param iGeomField integer; in case of multiple geometry fields, which one to take?
#' @details for iGeomField, see also \url{https://trac.osgeo.org/gdal/wiki/rfc41_multiple_geometry_fields}
#' @examples
#' if (Sys.getenv("USER") == "travis") { # load meuse to postgis
#'  library(sp)
#'  example(meuse, ask = FALSE, echo = FALSE)
#'  if (require(rgdal))
#'    writeOGR(meuse, "PG:dbname=postgis", "meuse", driver = "PostgreSQL")
#' }
#' if (Sys.getenv("USER") %in% c("travis", "edzer")) {
#'  (s = st_read("PG:dbname=postgis", "meuse"))
#'  summary(s)
#' }
#' (s = st_read(system.file("shapes/", package="maptools"), "sids"))[1:10,]
#' summary(s)
#' @name st_read
#' @export
st_read = function(dsn, layer, ..., quiet = FALSE, iGeomField = 1L) {
	x = CPL_read_ogr(dsn, layer, quiet, iGeomField - 1L)
	geom = x$geometry
	x$geometry = NULL
	x = as.data.frame(x)
	x$geometry = st_sfc(geom)
	st_as_sf(x, ...)
}

#' read PostGIS table directly, using DBI and wkb conversion
#' 
#' read PostGIS table directly through DBI and RPostgreSQL interface, converting wkb
#' @param conn (open) database connection
#' @param query SQL query to select records
#' @param dbname character; database name, only used if cn is \code{NULL}
#' @param geom_column character or integer: indicator of name or position of the geometry column; if not provided, the last column of type character is chosen
#' @examples 
#' if (Sys.getenv("USER") %in% c("travis", "edzer")) {
#'   st_read_pg(dbname = "postgis", query = "select * from meuse limit 3;")
#' }
#' @name st_read
#' @export
st_read_pg = function(conn = NULL, query, dbname, geom_column = NULL, ...) {
	if (!requireNamespace("RPostgreSQL", quietly = TRUE))
		stop("package RPostgreSQL required for st_read_pg")
	if (is.null(conn))
		conn = RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), dbname = dbname)
  	# suppress warning about unknown type "geometry":
	suppressWarnings(tbl <- RPostgreSQL::dbGetQuery(conn, query))
	if (is.null(geom_column)) # find the geometry column - guess it's the last character column:
		geom_column = tail(which(sapply(tbl, is.character)), 1)
    tbl[[geom_column]] = st_as_sfc(structure(tbl[[geom_column]], class = "WKB"), EWKB = TRUE)
	st_as_sf(tbl, ...)
}
