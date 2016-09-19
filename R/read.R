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
#' if (Sys.getenv("USER") %in% c("edzer", "travis")) { # load meuse to postgis
#'  (s = st_read("PG:dbname=postgis", "meuse"))
#'  summary(s)
#' }
#' (s = st_read(system.file("shapes/", package="maptools"), "sids"))[1:10,]
#' summary(s)
#' @name st_read
#' @export
st_read = function(dsn, layer, ..., quiet = FALSE, iGeomField = 1L) {
	x = CPL_read_ogr(dsn, layer, quiet, iGeomField - 1L)
	which.geom = which(sapply(x, function(f) inherits(f, "sfc")))
	nm = names(x)[which.geom]
	geom = x[[which.geom]]
	x[[which.geom]] = NULL
	x = as.data.frame(x)
	x[[nm]] = st_sfc(geom)
	st_as_sf(x, ...)
}

#' write simple features from file or database
#'
#' write simple features from file or database
#' @param obj object of class \code{sf} or \code{sfc}
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder)
#' @param layer layer name (varies by driver, may be a file name without extension)
#' @param driver character; OGR driver name to be used
#' @param ... ignored
#' @param quiet logical; suppress info on name, driver, size and spatial reference
#' @param factorsAsCharacter logical; convert \code{factor} objects into character strings (default), else into numbers by \code{as.numeric}.
#' @details columns (variables) of a class not supported are dropped with a warning.
#' @examples
#' if (Sys.getenv("USER") == "travis") { # load meuse to postgis
#'  library(sp)
#'  example(meuse, ask = FALSE, echo = FALSE)
#'  st_write(st_as_sf(meuse), "PG:dbname=postgis", "meuse_sf", driver = "PostgreSQL")
#'  sids = st_read(system.file("shapes/", package="maptools"), "sids")
#'  st_write(sids, "PG:dbname=postgis", "sids", driver = "PostgreSQL")
#' }
#' @export
st_write = function(obj, dsn, layer, driver = "ESRI Shapefile", ..., quiet = FALSE,
		factorsAsCharacter = TRUE) {

	if (inherits(obj, "sfc"))
		obj = st_sf(id = 1:length(obj), geom = obj)
	stopifnot(inherits(obj, "sf"))
	geom = st_geometry(obj)
	obj[[attr(obj, "sf_column")]] = NULL
	if (factorsAsCharacter)
		obj = lapply(obj, function(x) if (is.factor(x)) as.character(x) else x)
	else
		obj = lapply(obj, function(x) if (is.factor(x)) as.numeric(x) else x)
	ccls = sapply(obj, function(x) class(x)[1])
	ccls.ok = ccls %in% c("character", "integer", "numeric")
	if (any(!ccls.ok)) {
		cat("ignoring columns with unsupported type:\n")
		print(cbind(names(obj)[!ccls.ok], ccls[!ccls.ok]))
		obj = obj[ccls.ok]
	}
	attr(obj, "colclasses") = sapply(obj, class)
	dim = class(geom[[1]])[1]
	CPL_write_ogr(obj, dsn, layer, driver, geom, dim, quiet)
}

#' read PostGIS table directly, using DBI and wkb conversion
#' 
#' read PostGIS table directly through DBI and RPostgreSQL interface, converting wkb
#' @param conn open database connection
#' @param table table name
#' @param query SQL query to select records
#' @param geom_column character or integer: indicator of name or position of the geometry column; if not provided, the last column of type character is chosen
#' @examples 
#' if (Sys.getenv("USER") %in% c("travis", "edzer")) {
#'   library(RPostgreSQL)
#'   conn = dbConnect(PostgreSQL(), dbname = "postgis")
#'   st_read_db(conn, query = "select * from meuse limit 3;")
#'   dbDisconnect(conn)
#' }
#' @name st_read
#' @export
st_read_db = function(conn = NULL, table, query = paste("select * from ", table, ";"),
		geom_column = NULL, ...) {
	if (is.null(conn))
		stop("no connection provided")
  	# suppress warning about unknown type "geometry":
	suppressWarnings(tbl <- dbGetQuery(conn, query))
	if (is.null(geom_column)) # find the geometry column - guess it's the last character column:
		geom_column = tail(which(sapply(tbl, is.character)), 1)
	EWKB = inherits(conn, "PostgreSQLConnection")
    tbl[[geom_column]] = st_as_sfc(structure(tbl[[geom_column]], class = "WKB"), EWKB = EWKB)
	st_as_sf(tbl, ...)
}

#' write simple feature table to a spatial database
#' 
#' write simple feature table to a spatial database
#' @param conn open database connection
#' @param obj object of class \code{sf}
#' @param table_name name for the table in the database
#' @param geom_name name of the geometry column in the database
#' @param ... arguments passed on to \code{dbWriteTable}
#' @param dropTable logical; should \code{table_name} be dropped first?
#' @export
#' @examples
#' library(sp)
#' data(meuse)
#' sf = st_as_sf(meuse, coords = c("x", "y"), epsg = 28992)
#' library(RPostgreSQL)
#' conn = dbConnect(PostgreSQL(), dbname = "postgis")
#' st_write_db(conn, sf, "meuse", dropTable = FALSE)
st_write_db = function(conn = NULL, obj, table_name = substitute(obj), geom_name = "wkb_geometry",
		..., dropTable = FALSE, wkb = TRUE) {
	if (is.null(conn))
		stop("if no provided")
	if (dropTable)
		dbGetQuery(conn, paste("drop table", table_name, ";"))
	df = obj
	df[[attr(df, "sf_column")]] = NULL
	class(df) = "data.frame"
	if (dropTable)
		dbSendQuery(conn, paste("drop table ", table_name, ";"))
	dbWriteTable(conn, table_name, df, ...)
	geom = st_geometry(obj)
	DIM = nchar(class(geom[[1]])[1]) # FIXME: is this correct? XY, XYZ, XYZM
	SRID = attr(obj, "epsg")
	if (is.null(SRID) || is.na(SRID))
		SRID = 0
	TYPE = class(geom[[1]])[2]
	query = paste0("SELECT AddGeometryColumn('','", table_name, "','", geom_name, 
			"','", SRID, "','", TYPE, "',", DIM, ");")
	dbGetQuery(conn, query)
	rn = row.names(obj)
	if (! wkb) {
		wkt = st_as_wkt(geom)
		for (r in seq_along(rn)) {
			cmd = paste0("UPDATE ", table_name, " SET ", geom_name, 
				" = ST_GeomFromText('", wkt[r], "') WHERE \"row.names\" = '", rn[r], "';")
			dbGetQuery(conn, cmd)
		}
	} else {
		wkb = st_as_wkb(geom)
		for (r in seq_along(rn)) {
			cmd = paste0("UPDATE ", table_name, " SET ", geom_name, " = '", CPL_raw_to_hex(wkb[[r]]), 
				"' WHERE \"row.names\" = '", rn[r], "';")
			dbGetQuery(conn, cmd)
		}
	}
	dbDisconnect(conn)
}
