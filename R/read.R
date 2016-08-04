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
       	POINT = ST_Point(unlist(pts)),
      	LINESTRING25D = ,
       	LINESTRING = ST_LineString(pts),
       	MULTILINESTRING = ST_MultiLineString(pts),
       	POLYGON25D = ,
       	POLYGON = ST_Polygon(pts),
       	MULTIPOLYGON25D = ,
       	MULTIPOLYGON = ST_MultiPolygon(pts),
       	MULTIPOINT = ,
       	MULTIPOINT25D = ST_MultiPoint(pts),
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
#' @param ... parameter(s) passed on to \link{ST_as.sf}
#' @examples
#' if (Sys.getenv("USER") == "edzer") {
#'  (s = ST_read("PG:dbname=test", "test"))
#'  summary(s)
#' }
#' (s = ST_read(system.file("shapes/", package="maptools"), "sids"))[1:10,]
#' summary(s)
#' @export
ST_read = function(dsn, layer, ...) {
	if (!requireNamespace("rgdal2", quietly = TRUE))
		stop("package rgdal2 required for ST_read; try devtools::install_github(\"edzer/rgdal2\")")
	o = rgdal2::openOGRLayer(dsn, layer)
	ids = rgdal2::getIDs(o)
	srs = rgdal2::getSRS(o)
	p4s = if (is.null(srs)) as.character(NA) else rgdal2::getPROJ4(srs)
	geom = ST_sfc(lapply(ids, function(id) readFeature(o, id)), proj4string = p4s)
	f = lapply(ids, function(id) rgdal2::getFields(rgdal2::getFeature(o, id)))
	df = data.frame(row.names = ids, apply(do.call(rbind, f), 2, unlist))
	df$geom = geom
	ST_as.sf(df, ...)
}
