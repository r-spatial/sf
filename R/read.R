# read feature id from layer:
readFeature = function(layer, id) {
	# xylst2mtrx cbinds list(x=...,y=...) instances to matrix in nested lists:
	xylst2mtrx = function(x) 
		if (all(c("x", "y") %in% names(x))) # we're at the deepest level
			do.call(cbind, x) 
		else 
			lapply(x, xylst2mtrx)
	ft = rgdal2::getFeature(layer, id)
	geom = rgdal2::getGeometry(ft)
	pts = xylst2mtrx(rgdal2::getPoints(geom, nested = TRUE))
	switch (rgdal2::getGeometryType(geom),
      	POINT25D = ,
       	POINT = st_point(unlist(pts)),
      	LINESTRING25D = ,
       	LINESTRING = st_linestring(pts),
       	MULTILINESTRING = st_multilinestring(pts),
       	POLYGON25D = ,
       	POLYGON = st_polygon(pts),
       	MULTIPOLYGON25D = ,
       	MULTIPOLYGON = st_multipolygon(pts),
       	MULTIPOINT = ,
       	MULTIPOINT25D = st_multipoint(pts),
       	LINEARRING = ,
       	MULTILINESTRING25D = ,
			stop(paste("geometry type", rgdal2::getGeometryType(geom), "not supported"))
	)
}

#' read simple features from file or database
#'
#' read simple features from file or database
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder)
#' @param layer layer name (varies by driver, may be a file name without extension)
#' @param ... parameter(s) passed on to \link{st_as_sf}
#' @examples
#' if (Sys.getenv("USER") == "edzer") {
#'  (s = st_read("PG:dbname=test", "test"))
#'  summary(s)
#' }
#' (s = st_read(system.file("shapes/", package="maptools"), "sids"))[1:10,]
#' summary(s)
#' @name st_read
#' @export
st_read = function(dsn, layer, ...) {
	if (!requireNamespace("rgdal2", quietly = TRUE))
		stop("package rgdal2 required for st_read; try devtools::install_github(\"edzer/rgdal2\")")
	o = rgdal2::openOGRLayer(dsn, layer)
	ids = rgdal2::getIDs(o)
	srs = rgdal2::getSRS(o)
	p4s = if (is.null(srs)) as.character(NA) else rgdal2::getPROJ4(srs)
	geom = st_sfc(lapply(ids, function(id) readFeature(o, id)), proj4string = p4s)
	f = lapply(ids, function(id) rgdal2::getFields(rgdal2::getFeature(o, id)))
	df = data.frame(row.names = ids, apply(do.call(rbind, f), 2, unlist))
	df$geom = geom
	st_as_sf(df, ...)
}

#' @name st_read
#' @param sf object of class \code{sf}
#' @param driver driver name
#' @param opts options to pass on to driver
#' @export
st_write = function(sf, dsn = ".", layer, driver = "ESRI Shapefile", opts = character(), ...) {
	if (!requireNamespace("rgdal2", quietly = TRUE))
		stop("package rgdal2 required for st_read; try devtools::install_github(\"edzer/rgdal2\")")
	o = rgdal2::newOGRDatasource(driver = driver, fname = dsn, opts = opts)
	geomType = class(geometry(sf)[[1]])[1]
	rgdal2::addLayer(o, layer, geomType = geomType, srs = rgdal2::newSRS(p4s(sf)), opts = opts)
	# how to add fields and features to the layer?
	stop("adding fields and features to layers not yet implemented")
}

#' read PostGIS table directly, using DBI and wkb conversion
#' 
#' read PostGIS table directly through DBI and RPostgreSQL interface, converting wkb
#' @param cn open database connection
#' @param query SQL query to select records
#' @param dbname character; database name, only used if cn is \code{NULL}
#' @param geom_column character or integer: indicator of name or position of the geometry column; if not provided, the last column of type character is chosen
#' @examples 
#' if (Sys.getenv("USER") == "edzer") {
#'   st_read_pg(dbname = "postgis", query = "select * from meuse2 limit 3;")
#' }
#' @name st_read
#' @export
st_read_pg = function(cn = NULL, query, dbname, geom_column = NULL, ...) {
	if (!requireNamespace("RPostgreSQL", quietly = TRUE))
		stop("package RPostgreSQL required for st_read_pg")
	if (is.null(cn))
		cn = RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), dbname = dbname)
  	w = options("warn")[[1]]
  	options(warn = -1)
	tbl = RPostgreSQL::dbGetQuery(cn, query)
  	options(warn = w)
	if (is.null(geom_column))
	# find the geometry column:
		geom_column = tail(which(sapply(tbl, is.character)), 1)
	wkbCharToRaw = function(y) {
		stopifnot((nchar(y) %% 2) == 0)
		n = nchar(y)/2
		as.raw(as.numeric(paste0("0x", sapply(1:n, function(x) substr(y, (x-1)*2+1, x*2)))))
	}
    wkb = structure(lapply(tbl[[geom_column]], wkbCharToRaw), class = "WKB")
    tbl[[geom_column]] = st_as_sfc(wkb, EWKB = TRUE)
	st_as_sf(tbl, ...)
}
