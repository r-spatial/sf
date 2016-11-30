#' Read simple features or layers from file or database
#'
#' Read simple features from file or database, or retrieve layer names and their geometry type(s)
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder, or contain the name and access credentials of a database)
#' @param layer layer name (varies by driver, may be a file name without extension); in case \code{layer} is missing, \code{st_read} will read the first layer of \code{dsn}, give a warning and (unless \code{quiet = TRUE}) print a message when there are multiple layers, or give an error if there are no layers in \code{dsn}.
#' @param ... parameter(s) passed on to \link{st_as_sf}
#' @param options character; driver dependent dataset open options, multiple options supported.
#' @param quiet logical; suppress info on name, driver, size and spatial reference, or signaling no or multiple layers
#' @param iGeomField integer; in case of multiple geometry fields, which one to take?
#' @param type integer; ISO number of desired simple feature type; see details. If left zero, in case of mixed feature geometry types, conversion to the highest numeric type value found will be attempted.
#' @param promote_to_multi logical; in case of a mix of LineString and MultiLineString, or of Polygon and MultiPolygon, convert all to the Multi variety; defaults to \code{TRUE}
#' @details for iGeomField, see also \url{https://trac.osgeo.org/gdal/wiki/rfc41_multiple_geometry_fields}; for \code{type} values see \url{https://en.wikipedia.org/wiki/Well-known_text#Well-known_binary}, but note that not every target value may lead to succesful conversion. The typical conversion from POLYGON (3) to MULTIPOLYGON (6) should work; the other way around (type=3), secondary rings from MULTIPOLYGONS may be dropped without warnings. 
#' @param stringsAsFactors logical; logical: should character vectors be converted to factors?  The `factory-fresh' default is \code{TRUE}, but this can be changed by setting \code{options(stringsAsFactors = FALSE)}.  
#' @return object of class \link{sf} when a layer was succesfully read; in case argument \code{layer} is missing and data source \code{dsn} does not contain a single layer, an object of class \code{sf_layers} is returned with the layer names, each with their geometry type(s). Note that the number of layers may also be zero.
#' @examples
#' if (Sys.getenv("USER") %in% c("edzer", "travis")) { # load meuse to postgis
#'  library(sp)
#'  example(meuse, ask = FALSE, echo = FALSE)
#'  st_write(st_as_sf(meuse), "PG:dbname=postgis", "meuse", 
#'      layer_options = "OVERWRITE=true")
#'  (s = st_read("PG:dbname=postgis", "meuse"))
#'  summary(s)
#' }
#' # nc = st_read(system.file("gpkg/nc.gpkg", package="sf"))
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' summary(nc)
#' @name st_read
#' @note The use of \code{system.file} in examples make sure that examples run regardless where R is installed: typical users will not use \code{system.file} but give the file name directly, either with full path or relative to the current working directory (see \link{getwd}). "Shapefiles" consist of several files with the same basename that reside in the same directory, only one of them having extension \code{.shp}. 
#' @export
st_read = function(dsn, layer, ..., options = NULL, quiet = FALSE, iGeomField = 1L, type = 0,
		promote_to_multi = TRUE, stringsAsFactors = default.stringsAsFactors()) {

	if (missing(dsn))
		stop("dsn should specify a data source or filename")
  
	if (missing(layer))
		layer = character(0)
	
	if (file.exists(dsn))
		dsn = normalizePath(dsn)

	x = CPL_read_ogr(dsn, layer, as.character(options), quiet, iGeomField - 1L, type, 
		promote_to_multi)
	which.geom = which(sapply(x, function(f) inherits(f, "sfc")))
	nm = names(x)[which.geom]
	geom = x[[which.geom]]
	x[[which.geom]] = NULL
	if (length(x) == 0)
		x = data.frame(row.names = seq_along(geom))
	else
		x = as.data.frame(x, stringsAsFactors = stringsAsFactors)
	x[[nm]] = st_sfc(geom, crs = attr(geom, "crs")) # computes bbox
	x = st_as_sf(x, ...)
	if (! quiet)
		print(x, n = 0)
	else 
		x
}

#' Write simple features object to file or database
#'
#' Write simple features object to file or database
#' @param obj object of class \code{sf} or \code{sfc}
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder or contain a database name)
#' @param layer layer name (varies by driver, may be a file name without extension); if layer is missing, the \link{basename} of \code{dsn} is taken.
#' @param driver character; OGR driver name to be used, if missing, a driver name is guessed from \code{dsn}.
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
#'  st_write(st_as_sf(meuse), "PG:dbname=postgis", "meuse_sf",
#'    layer_options = c("OVERWRITE=yes", "LAUNDER=true"))
#'  demo(nc, ask = FALSE)
#'  st_write(nc, "PG:dbname=postgis", "sids", layer_options = "OVERWRITE=true")
#' }
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' st_write(nc, "nc.shp")
#' @export
st_write = function(obj, dsn, layer = basename(dsn), driver = guess_driver(dsn), ..., 
		dataset_options = NULL, layer_options = NULL, quiet = FALSE, factorsAsCharacter = TRUE) {

	if (inherits(obj, "sfc"))
		obj = st_sf(id = 1:length(obj), geom = obj)
	stopifnot(inherits(obj, "sf"))
	
	if (missing(dsn))
		stop("dsn should specify a data source or filename")
	
	if (file.exists(dsn))
		dsn = normalizePath(dsn)
	
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

#' Get GDAL drivers
#' 
#' Get a list of the available GDAL drivers
#' @param what character: "vector" or "raster", anything else will return all drivers.
#' @details The drivers available will depend on the installation of GDAL/OGR, and can vary; the \code{st_drivers()} function shows which are available, and which may be written (but all are assumed to be readable). Note that stray files in data source directories (such as *.dbf) may lead to suprious errors that accompanying *.shp are missing.
#' @return a \code{data.frame} with driver metadata
#' @export
#' @examples
#' st_drivers()
st_drivers = function(what = "vector") {
	ret = as.data.frame(CPL_get_rgdal_drivers(0))
	names(ret) = c("name", "long_name", "write", "copy", "is_raster", "is_vector")
	row.names(ret) = ret$name
	if (what == "vector")
		ret[ret$is_vector,]
	else if (what == "raster")
		ret[ret$is_raster,]
	else
		ret
}

#' @export
print.sf_layers = function(x, ...) {
	x$geomtype = sapply(x$geomtype, function(x) paste(x, collapse = ", "))
	cat(paste("Driver:", x$driver, "\n")) 
	x$driver = NULL
	x$features[x$features < 0] = NA
	cat("Available layers:\n")
	if (length(x$name) == 0) {
		cat("<none>\n")
		invisible(x)
	} else {
		df = data.frame(unclass(x))
		names(df) = c("layer_name", "geometry_type", "features", "fields")
		print(df)
		invisible(df)
	}
}

#' List layers in a datasource
#'
#' List layers in a datasource
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder, or contain the name and access credentials of a database)
#' @param options character; driver dependent dataset open options, multiple options supported.
#' @param do_count logical; if TRUE, count the features by reading them, even if their count is not reported by the driver
#' @export
st_list = function(dsn, options = character(0), do_count = FALSE) {
	if (missing(dsn))
		stop("dsn should specify a data source or filename")
	if (file.exists(dsn))
		dsn = normalizePath(dsn)
	CPL_get_layers(dsn, options, do_count)
}

guess_driver = function(dsn) {
    ext_map <- matrix (c(
                       "bna",    "BNA",
                       "csv",    "CSV",
                       "e00",    "AVCE00",
                       "gdb",    "FileGDB",
                       "geojson","GeoJSON",
                       "gml",    "GML",
                       "gmt",    "GMT",
                       "gpkg",   "GPKG",
                       "gps",    "GPSBabel",
                       "gtm",    "GPSTrackMaker",   
                       "gxt",    "Geoconcept",
                       "jml",    "JML",
                       "map",    "WAsP",
                       "mdb",    "Geomedia",
                       "nc",     "netCDF",
                       "ods",    "ODS",
                       "osm",    "OSM",
                       "pbf",    "OSM",
                       "shp",    "ESRI Shapefile",
                       "sqlite", "SQLite",
                       "vdv",    "VDV",
                       "xls",    "xls",
                       "xlsx",   "XLSX"
                     ), ncol = 2, byrow = TRUE)
	prefix_map = matrix(c(
                       "couchdb", "CouchDB",
                       "DB2ODBC", "DB2ODBC",
                       "DODS",    "DODS",
                       "GFT",     "GFT",
                       "MSSQL",   "MSSQLSpatial",
                       "MySQL",   "MySQL",
                       "OCI",     "OCI",
                       "ODBC",    "ODBC",
                       "PG",      "PostgreSQL",
                       "SDE",     "SDE"
                     ), ncol = 2, byrow = TRUE)
	drv = ext_map[,2]
	names(drv) = ext_map[,1]
	prfx = prefix_map[,2]
	names(prfx) = tolower(prefix_map[,1])

	# find match: try extension first
	drv = drv[tolower(tools::file_ext(dsn))]
	if (is.na(drv)) { # try prefix, like "PG:dbname=sth"
		if (length(grep(":", dsn)))
			drv = prfx[tolower(strsplit(dsn, ":")[[1]][1])]
		if (is.na(drv)) # no match
			stop("no driver specified, cannot guess driver from dsn")
	}
	drivers = st_drivers()
	i = match(drv, drivers$name)
	if (is.na(i))
		stop(paste("guess_driver:", drv, "not available in supported drivers, see `st_drivers()'"))
	if (! drivers[i, "write"])
		stop(paste("Driver", drv, "has no write capability, see `st_drivers()'"))
	drv
}
