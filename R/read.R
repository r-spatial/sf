#' read simple features from file or database
#'
#' read simple features from file or database
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder)
#' @param layer layer name (varies by driver, may be a file name without extension); see details
#' @param ... parameter(s) passed on to \link{st_as_sf}
#' @param options character; driver dependent dataset open options; multiple options supported.
#' @param quiet logical; suppress info on name, driver, size and spatial reference
#' @param iGeomField integer; in case of multiple geometry fields, which one to take?
#' @param type integer; ISO number of desired simple feature type; see details. If left zero, in case of mixed feature geometry types, conversion to the highest numeric type value found will be attempted.
#' @param promote_to_multi logical; in case of a mix of LineString and MultiLineString, or of Polygon and MultiPolygon, convert all to the Multi variety; defaults to \code{TRUE}
#' @details for iGeomField, see also \url{https://trac.osgeo.org/gdal/wiki/rfc41_multiple_geometry_fields}; for \code{type} values see \url{https://en.wikipedia.org/wiki/Well-known_text#Well-known_binary}, but note that not every target value may lead to succesful conversion. The typical conversion from POLYGON (3) to MULTIPOLYGON (6) should work; the other way around (type=3), secondary rings from MULTIPOLYGONS may be dropped without warnings. 
#' @details layer name may be guessed in some cases e.g. when \code{dsn} contains the full path of a geopackage or a shapefile (with extension .shp), and it will try to do so; this may fail in other cases, though; it is often a good idea to specify both basename and layer name.
#' @return object of class \link{sf}
#' @examples
#' if (Sys.getenv("USER") %in% c("edzer", "travis")) { # load meuse to postgis
#'  library(sp)
#'  example(meuse, ask = FALSE, echo = FALSE)
#'  st_write(st_as_sf(meuse), "PG:dbname=postgis", "meuse", driver = "PostgreSQL", 
#'    options = "OVERWRITE=true")
#'  (s = st_read("PG:dbname=postgis", "meuse"))
#'  summary(s)
#' }
#' # nc = st_read(system.file("gpkg/nc.gpkg", package="sf"), "nc.gpkg", crs = 4267)
#' nc = st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267)
#' summary(nc)
#' @name st_read
#' @export
st_read = function(dsn, layer = default_layer(dsn), ..., 
		options = NULL, quiet = FALSE, iGeomField = 1L, type = 0,
		promote_to_multi = TRUE) {

	x = CPL_read_ogr(dsn, layer, as.character(options), quiet, iGeomField - 1L, type, 
		promote_to_multi)
	which.geom = which(sapply(x, function(f) inherits(f, "sfc")))
	nm = names(x)[which.geom]
	geom = x[[which.geom]]
	x[[which.geom]] = NULL
	if (length(x) == 0)
		x = data.frame(row.names = seq_along(geom))
	else
		x = as.data.frame(x)
	crs = if (is.null(attr(geom, "proj4string")))
			NA_integer_
		else
			attr(geom, "proj4string")
	x[[nm]] = st_sfc(geom, crs = crs)
	st_as_sf(x, ...)
}

#' write simple features object to file or database
#'
#' write simple features object to file or database
#' @param obj object of class \code{sf} or \code{sfc}
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder)
#' @param layer layer name (varies by driver, may be a file name without extension)
#' @param driver character; OGR driver name to be used
#' @param ... ignored
#' @param dataset_options character; driver dependent dataset creation options; multiple options supported.
#' @param layer_options character; driver dependent layer creation options; multiple options supported.
#' @param quiet logical; suppress info on name, driver, size and spatial reference
#' @param factorsAsCharacter logical; convert \code{factor} objects into character strings (default), else into numbers by \code{as.numeric}.
#' @details columns (variables) of a class not supported are dropped with a warning.
#' @examples
#' if (Sys.getenv("USER") %in% c("edzer", "travis")) { # load meuse to postgis
#'  library(sp)
#'  example(meuse, ask = FALSE, echo = FALSE)
#'  st_write(st_as_sf(meuse), "PG:dbname=postgis", "meuse_sf", driver = "PostgreSQL",
#'    layer_options = c("OVERWRITE=yes", "LAUNDER=true"))
#'  demo(nc, ask = FALSE)
#'  st_write(nc, "PG:dbname=postgis", "sids", driver = "PostgreSQL", 
#'    layer_options = "OVERWRITE=true")
#' }
#' nc = st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267)
#' st_write(nc, ".", "nc")
#' @export
st_write = function(obj, dsn, layer, driver = "ESRI Shapefile", ..., dataset_options = NULL,
		layer_options = NULL, quiet = FALSE, factorsAsCharacter = TRUE) {

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
	ccls.ok = ccls %in% c("character", "integer", "numeric", "Date", "POSIXct")
	if (any(!ccls.ok)) {
		# nocov start
		cat("ignoring columns with unsupported type:\n")
		print(cbind(names(obj)[!ccls.ok], ccls[!ccls.ok]))
		obj = obj[ccls.ok]
		# nocov end
	}
	attr(obj, "colclasses") = sapply(obj, function(x) class(x)[1])
	dim = class(geom[[1]])[1]
	CPL_write_ogr(obj, dsn, layer, driver, 
		as.character(dataset_options), as.character(layer_options), 
		geom, dim, quiet)
}

#' read PostGIS table directly, using DBI and binary conversion
#' 
#' read PostGIS table directly through DBI and RPostgreSQL interface, converting binary
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

#' write simple feature table to a spatial database
#' 
#' write simple feature table to a spatial database
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


#' get GDAL drivers
#' 
#' get a list of the available GDAL drivers
#' @param what character: "vector" or "raster", anything else will return all drivers.
#' @details The drivers available will depend on the installation of GDAL/OGR, and can vary; the \code{st_drivers()} function shows which are available, and which may be written (but all are assumed to be readable). Note that stray files in data source directories (such as *.dbf) may lead to suprious errors that accompanying *.shp are missing.
#' @return a \code{data.frame} with driver metadata
#' @export
#' @examples
#' st_drivers()
st_drivers = function(what = "vector") {
	ret = as.data.frame(CPL_get_rgdal_drivers(0))
	names(ret) = c("name", "long_name", "write", "copy", "is_raster", "is_vector")
	if (what == "vector")
		ret[ret$is_vector,]
	else if (what == "raster")
		ret[ret$is_raster,]
	else
		ret
}

default_layer = function(dsn) {
	bn = basename(dsn)
	ext = tools::file_ext(bn)
	nm = tools::file_path_sans_ext(bn)
	if (bn == "." || ext == "" || nm == "")
		stop("cannot guess layer name from this datasource")
	switch(ext, 
		shp = nm, # shapefile: skip path and extension
		gpkg = bn, # geopackage: use basename (incl. extension)
		geojson = "OGRGeoJSON", # fixed layer name
		{ warning(paste("Unknown extension! Guessing", bn, "as layer name...")); bn })
}
