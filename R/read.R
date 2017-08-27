set_utf8 = function(x) {
	n = names(x)
	Encoding(n) = "UTF-8"
	to_utf8 = function(x) {
		if (is.character(x))
			Encoding(x) = "UTF-8"
		x
	}
	structure(lapply(x, to_utf8), names = n)
}


#' Read simple features or layers from file or database
#'
#' Read simple features from file or database, or retrieve layer names and their geometry type(s)
#' @param dsn data source name (interpretation varies by driver - for some drivers, \code{dsn} is a file name, but may also be a folder,
#' or contain the name and access credentials of a database); in case of GeoJSON, \code{dsn} may be the character string holding the geojson data
#' @param layer layer name (varies by driver, may be a file name without extension); in case \code{layer} is missing,
#' \code{st_read} will read the first layer of \code{dsn}, give a warning and (unless \code{quiet = TRUE}) print a
#' message when there are multiple layers, or give an error if there are no layers in \code{dsn}.
#' @param ... parameter(s) passed on to \link{st_as_sf}
#' @param options character; driver dependent dataset open options, multiple options supported.
#' @param quiet logical; suppress info on name, driver, size and spatial reference, or signaling no or multiple layers
#' @param geometry_column integer or character; in case of multiple geometry fields, which one to take?
#' @param type integer; ISO number of desired simple feature type; see details. If left zero, and \code{promote_to_multi}
#' is \code{TRUE}, in case of mixed feature geometry
#' types, conversion to the highest numeric type value found will be attempted. A vector with different values for each geometry column
#' can be given.
#' @param promote_to_multi logical; in case of a mix of Point and MultiPoint, or of LineString and MultiLineString, or of
#' Polygon and MultiPolygon, convert all to the Multi variety; defaults to \code{TRUE}
#' @param stringsAsFactors logical; logical: should character vectors be converted to factors?  The `factory-fresh' default
#' is \code{TRUE}, but this can be changed by setting \code{options(stringsAsFactors = FALSE)}.
#' @param int64_as_string logical; if TRUE, Int64 attributes are returned as string; if FALSE, they are returned as double
#' and a warning is given when precision is lost (i.e., values are larger than 2^53).
#' @details for \code{geometry_column}, see also \url{https://trac.osgeo.org/gdal/wiki/rfc41_multiple_geometry_fields}; for \code{type}
#' values see \url{https://en.wikipedia.org/wiki/Well-known_text#Well-known_binary}, but note that not every target value
#' may lead to successful conversion. The typical conversion from POLYGON (3) to MULTIPOLYGON (6) should work; the other
#' way around (type=3), secondary rings from MULTIPOLYGONS may be dropped without warnings. \code{promote_to_multi} is handled on a per-geometry column basis; \code{type} may be specified for each geometry column.
#'
#' In case of problems reading shapefiles from USB drives on OSX, please see \url{https://github.com/r-spatial/sf/issues/252}.
#' @return object of class \link{sf} when a layer was successfully read; in case argument \code{layer} is missing and
#' data source \code{dsn} does not contain a single layer, an object of class \code{sf_layers} is returned with the
#' layer names, each with their geometry type(s). Note that the number of layers may also be zero.
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' summary(nc) # note that AREA was computed using Euclidian area on lon/lat degrees
#'
#' \dontrun{
#'   library(sp)
#'   example(meuse, ask = FALSE, echo = FALSE)
#'   st_write(st_as_sf(meuse), "PG:dbname=postgis", "meuse",
#'        layer_options = "OVERWRITE=true")
#'   st_meuse = st_read("PG:dbname=postgis", "meuse")
#'   summary(st_meuse)
#' }

#' @name st_read
#' @note The use of \code{system.file} in examples make sure that examples run regardless where R is installed:
#' typical users will not use \code{system.file} but give the file name directly, either with full path or relative
#' to the current working directory (see \link{getwd}). "Shapefiles" consist of several files with the same basename
#' that reside in the same directory, only one of them having extension \code{.shp}.
#' @export
st_read = function(dsn, layer, ..., options = NULL, quiet = FALSE, geometry_column = 1L, type = 0,
		promote_to_multi = TRUE, stringsAsFactors = default.stringsAsFactors(),
		int64_as_string = FALSE) {

	if (missing(dsn))
		stop("dsn should specify a data source or filename")

	if (missing(layer))
		layer = character(0)

	if (file.exists(dsn))
		dsn = enc2utf8(normalizePath(dsn))

	if (length(promote_to_multi) > 1)
		stop("`promote_to_multi' should have length one, and applies to all geometry columns")

	x = CPL_read_ogr(dsn, layer, as.character(options), quiet, type, promote_to_multi, int64_as_string)

	# TODO: take care of multiple geometry colums:
	which.geom = which(vapply(x, function(f) inherits(f, "sfc"), TRUE))
	nm = names(x)[which.geom]
	Encoding(nm) = "UTF-8"
	geom = x[which.geom]

	lc.other = setdiff(which(vapply(x, is.list, TRUE)), which.geom) # non-sfc list-columns
	list.cols = x[lc.other]
	nm.lc = names(x)[lc.other]

	x = if (length(x) == length(geom)) # ONLY geometry column(s)
		data.frame(row.names = seq_along(geom[[1]]))
	else
		as.data.frame(set_utf8(x[-c(lc.other, which.geom)]), stringsAsFactors = stringsAsFactors)

	for (i in seq_along(lc.other))
		x[[ nm.lc[i] ]] = list.cols[[i]]

	for (i in seq_along(geom))
		x[[ nm[i] ]] = st_sfc(geom[[i]], crs = attr(geom[[i]], "crs")) # computes bbox

	x = st_as_sf(x, ...,
		sf_column_name = if (is.character(geometry_column)) geometry_column else nm[geometry_column])
	if (! quiet)
		print(x, n = 0)
	else
		x
}

#' @name st_read
#' @export
#' @details \code{read_sf} and \code{write_sf} are aliases for \code{st_read} and \code{st_write}, respectively, with some
#' modified default arguments.
#' \code{read_sf} and \code{write_sf} are quiet by default: they do not print information
#' about the data source.
#' \code{write_sf} delete layers by default: it overwrites existing files.
#' @examples
#' # read geojson from string:
#' geojson_txt <- paste("{\"type\":\"MultiPoint\",\"coordinates\":",
#'    "[[3.2,4],[3,4.6],[3.8,4.4],[3.5,3.8],[3.4,3.6],[3.9,4.5]]}")
#' x = read_sf(geojson_txt)
#' x
read_sf <- function(..., quiet = TRUE, stringsAsFactors = FALSE) {
	if (! requireNamespace("tibble", quietly = TRUE))
		stop("package tibble not available: install first?")
	st_as_sf(tibble::as_tibble(as.data.frame(
		st_read(..., quiet = quiet, stringsAsFactors = stringsAsFactors))))
}

clean_columns = function(obj, factorsAsCharacter) {
	permitted = c("character", "integer", "numeric", "Date", "POSIXct")
	for (i in seq_along(obj)) {
		if (is.factor(obj[[i]])) {
			obj[[i]] = if (factorsAsCharacter)
					as.character(obj[[i]])
				else
					as.numeric(obj[[i]])
		}
		if (! inherits(obj[[i]], permitted)) {
			if (inherits(obj[[i]], "POSIXlt"))
				obj[[i]] = as.POSIXct(obj[[i]])
			else if (is.numeric(obj[[i]]))
				obj[[i]] = as.numeric(obj[[i]]) # strips class
		}
	}
	ccls.ok = vapply(obj, function(x) inherits(x, permitted), TRUE)
	if (any(!ccls.ok)) {
		# nocov start
		cat("ignoring columns with unsupported type:\n")
		print(paste(names(obj)[!ccls.ok], collapse = " "))
		obj = obj[ccls.ok]
		# nocov end
	}
	colclasses = vapply(obj, function(x) permitted[ which(inherits(x, permitted, which = TRUE) > 0)[1] ] , "")
	structure(obj, colclasses = colclasses)
}

abbreviate_shapefile_names = function(x) {
# from: rgdal/pkg/R/ogr_write.R:
    fld_names <- names(x)
#   if (!is.null(encoding)) {
#       fld_names <- iconv(fld_names, from=encoding, to="UTF-8")
#   }
	if (any(nchar(fld_names) > 10)) {
		fld_names <- abbreviate(fld_names, minlength = 7)
		warning("Field names abbreviated for ESRI Shapefile driver")
		if (any(nchar(fld_names) > 10)) 
			fld_names <- abbreviate(fld_names, minlength = 5) # nocov
	}
# fix for dots in DBF field names 121124
	if (length(wh. <- grep("\\.", fld_names) > 0))
		fld_names[wh.] <- gsub("\\.", "_", fld_names[wh.])

	if (length(fld_names) != length(unique(fld_names)))
		stop("Non-unique field names") # nocov

	names(x) = fld_names
	x
}

#' Write simple features object to file or database
#'
#' Write simple features object to file or database
#' @param obj object of class \code{sf} or \code{sfc}
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a
#' folder or contain a database name)
#' @param layer layer name (varies by driver, may be a file name without extension); if layer is missing, the
#' \link{basename} of \code{dsn} is taken.
#' @param driver character; driver name to be used, if missing, a driver name is guessed from \code{dsn};
#' \code{st_drivers()} returns the drivers that are available with their properties; links to full driver documentation
#' are found at \url{http://www.gdal.org/ogr_formats.html}.
#' @param dataset_options character; driver dependent dataset creation options; multiple options supported.
#' @param layer_options character; driver dependent layer creation options; multiple options supported.
#' @param quiet logical; suppress info on name, driver, size and spatial reference
#' @param factorsAsCharacter logical; convert \code{factor} objects into character strings (default), else into numbers by
#' \code{as.numeric}.
#' @param update logical; \code{FALSE} by default for single-layer drivers but \code{TRUE} by default for database drivers
#' as defined by \code{db_drivers}.
#' For database-type drivers (e.g. GPKG) \code{TRUE} values will make \code{GDAL} try
#' to update (append to) the existing data source, e.g. adding a table to an existing database.
#' @param delete_dsn logical; delete data source \code{dsn} before attempting to write?
#' @param delete_layer logical; delete layer \code{layer} before attempting to write? (not yet implemented)
#' @details columns (variables) of a class not supported are dropped with a warning. When deleting layers or
#' data sources is not successful, no error is emitted. \code{delete_dsn} and \code{delete_layers} should be
#' handled with care; the former may erase complete directories or databases.
#' @seealso \link{st_drivers}
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' st_write(nc, "nc.shp")
#' st_write(nc, "nc.shp", delete_layer = TRUE) # overwrites
#' data(meuse, package = "sp") # loads data.frame from sp
#' meuse_sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
#' st_write(meuse_sf, "meuse.csv", layer_options = "GEOMETRY=AS_XY") # writes X and Y as columns
#' st_write(meuse_sf, "meuse.csv", layer_options = "GEOMETRY=AS_WKT", delete_dsn=TRUE) # overwrites
#' \dontrun{
#' library(sp)
#' example(meuse, ask = FALSE, echo = FALSE)
#' st_write(st_as_sf(meuse), "PG:dbname=postgis", "meuse_sf",
#'     layer_options = c("OVERWRITE=yes", "LAUNDER=true"))
#' demo(nc, ask = FALSE)
#' st_write(nc, "PG:dbname=postgis", "sids", layer_options = "OVERWRITE=true")}
#' @name st_write
#' @export
st_write = function(obj, dsn, layer = file_path_sans_ext(basename(dsn)),
		driver = guess_driver_can_write(dsn), ...,
		dataset_options = NULL, layer_options = NULL, quiet = FALSE, factorsAsCharacter = TRUE,
		update = driver %in% db_drivers, delete_dsn = FALSE, delete_layer = FALSE) {

	if (length(list(...)))
		stop(paste("unrecognized argument(s)", unlist(list(...)), "\n"))

	if (inherits(obj, "sfc"))
		obj = st_sf(id = 1:length(obj), geom = obj)
	stopifnot(inherits(obj, "sf"))

	if (missing(dsn))
		stop("dsn should specify a data source or filename")

	if (file.exists(dsn))
		dsn = enc2utf8(normalizePath(dsn))

	geom = st_geometry(obj)
	obj[[attr(obj, "sf_column")]] = NULL

	if (driver == "ESRI Shapefile") { # remove trailing .shp from layer name
		layer = sub(".shp$", "", layer)
		obj = abbreviate_shapefile_names(obj)
	}

	obj = clean_columns(as.data.frame(obj), factorsAsCharacter)
	# this attaches attr colclasses

	dim = if (length(geom) == 0)
			"XY"
		else
			class(geom[[1]])[1]

	CPL_write_ogr(obj, dsn, layer, driver,
		as.character(dataset_options), as.character(layer_options),
		geom, dim, quiet, update, delete_dsn, delete_layer)
}

#' @name st_write
#' @export
write_sf <- function(..., quiet = TRUE, delete_layer = TRUE) {
	st_write(..., quiet = quiet, delete_layer = delete_layer)
}

#' Get GDAL drivers
#'
#' Get a list of the available GDAL drivers
#' @param what character: "vector" or "raster", anything else will return all drivers.
#' @details The drivers available will depend on the installation of GDAL/OGR, and can vary; the \code{st_drivers()}
#' function shows which are available, and which may be written (but all are assumed to be readable). Note that stray
#' files in data source directories (such as *.dbf) may lead to spurious errors that accompanying *.shp are missing.
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
	n_gt = max(sapply(x$geomtype, length))
	x$geomtype = vapply(x$geomtype, function(x) paste(x, collapse = ", "), "")
	cat(paste("Driver:", x$driver, "\n"))
	x$driver = NULL
	x$features[x$features < 0] = NA
	cat("Available layers:\n")
	if (length(x$name) == 0) {
		cat("<none>\n") # nocov
		invisible(x)    # nocov
	} else {
		df = data.frame(unclass(x))
		gt = if (n_gt > 1)
				"geometry_types"
			else
				"geometry_type"
		names(df) = c("layer_name", gt, "features", "fields")
		print(df)
		invisible(df)
	}
}

#' List layers in a datasource
#'
#' List layers in a datasource
#' @param dsn data source name (interpretation varies by driver - for some drivers, \code{dsn} is a file name, but may also be a
#' folder, or contain the name and access credentials of a database)
#' @param options character; driver dependent dataset open options, multiple options supported.
#' @param do_count logical; if TRUE, count the features by reading them, even if their count is not reported by the driver
#' @name st_layers
#' @export
st_layers = function(dsn, options = character(0), do_count = FALSE) {
	if (missing(dsn))
		stop("dsn should specify a data source or filename")
	if (file.exists(dsn))
		dsn = enc2utf8(normalizePath(dsn))
	CPL_get_layers(dsn, options, do_count)
}

guess_driver = function(dsn) {
  stopifnot(is.character(dsn))
  stopifnot(length(dsn) == 1)

	# find match: try extension first
	drv = extension_map[tolower(tools::file_ext(dsn))]
	if (any(grep(":", gsub(":[/\\]", "/", dsn))))
		drv = prefix_map[tolower(strsplit(dsn, ":")[[1]][1])]

	drv <- unlist(drv)

	if (is.null(drv)) {
	  # no match
	  return(NA)
	}
	drv
}

guess_driver_can_write = function(dns, drv = guess_driver(dns)) {
  if(is.na(drv)) {
    stop("Could not guess driver for ", dns, call. = FALSE)
  }
  if(!is_driver_available(drv)) {
    stop(unlist(drv), " driver not available in supported drivers, see `st_drivers()'", call. = FALSE)
  }
  if(!is_driver_can(drv, operation = "write")) {
    stop("Driver ", drv, " cannot write. see `st_drivers()'", call. = FALSE)
  }
  return(drv)
}

#' Check if driver is available
#'
#' Search through the driver table if driver is listed
#' @param drv character. Name of driver
#' @param drivers data.frame. Table containing driver names and support. Default
#' is from \code{\link{st_drivers}}
is_driver_available = function(drv, drivers = st_drivers()) {
  i = match(drv, drivers$name)
  if (is.na(i))
    return(FALSE)

  return(TRUE)
}

#' Check if a driver can perform an action
#'
#' Search through the driver table to match a driver name with
#' an action (e.g. \code{"write"}) and check if the action is supported.
#' @param drv character. Name of driver
#' @param drivers data.frame. Table containing driver names and support. Default
#' is from \code{\link{st_drivers}}
#' @param operation character. What action to check
is_driver_can = function(drv, drivers = st_drivers(), operation = "write") {
  stopifnot(operation %in% names(drivers))
  i = match(drv, drivers$name)
  if (!drivers[i, operation])
    return(FALSE)

  return(TRUE)
}

#' Map extension to driver
#' @docType data
extension_map <- list(
        "bna" = "BNA",
        "csv" = "CSV",
        "e00" = "AVCE00",
        "gdb" = "OpenFileGDB",
        "geojson" = "GeoJSON",
        "gml" = "GML",
        "gmt" = "GMT",
        "gpkg" = "GPKG",
        "gps" = "GPSBabel",
        "gtm" = "GPSTrackMaker",
        "gxt" = "Geoconcept",
        "jml" = "JML",
        "kml" = "KML",
        "map" = "WAsP",
        "mdb" = "Geomedia",
        "nc" = "netCDF",
        "ods" = "ODS",
        "osm" = "OSM",
        "pbf" = "OSM",
        "shp" = "ESRI Shapefile",
        "sqlite" = "SQLite",
        "vdv" = "VDV",
        "xls" = "xls",
        "xlsx" = "XLSX")

#' Map prefix to driver
#' @docType data
prefix_map <- list(
        "couchdb" = "CouchDB",
        "db2odbc" = "DB2ODBC",
        "dods" = "DODS",
        "gft" = "GFT",
        "mssql" = "MSSQLSpatial",
        "mysql" = "MySQL",
        "oci" = "OCI",
        "odbc" = "ODBC",
        "pg" = "PostgreSQL",
        "sde" = "SDE")

#' Drivers for which update should be \code{TRUE} by default
#' @docType data
db_drivers <- c(unlist(prefix_map), "GPKG", "SQLite")
