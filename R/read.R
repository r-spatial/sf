#' read simple features from file or database
#'
#' read simple features from file or database
#' @param dsn data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder)
#' @param layer layer name (varies by driver, may be a file name without extension)
#' @examples
#' if (Sys.getenv("USER") == "edzer") {
#'  (s = read.sf("PG:dbname=test", "test"))
#'  summary(s)
#' }
#' @export
read.sf = function(dsn, layer) {
	flatten = function(x)
		if (all(c("x", "y") %in% names(x))) # we're at the deepest level
			do.call(cbind, x) 
		else 
			lapply(x, flatten)
	readFeature = function(layer, id) {
		ft = getFeature(layer, id)
		geom = getGeometry(ft)
		pts = flatten(getPoints(geom, nested = TRUE))
		switch (getGeometryType(geom),
           	POINT25D = ,
           	POINT = POINT(unlist(pts)),
           	LINESTRING25D = ,
           	LINESTRING = LINESTRING(pts),
           	MULTILINESTRING = MULTILINESTRING(pts),
           	POLYGON25D = ,
           	POLYGON = POLYGON(pts),
           	MULTIPOLYGON25D = ,
           	MULTIPOLYGON = MULTIPOLYGON(pts),
           	MULTIPOINT = ,
           	MULTIPOINT25D = MULTIPOINT(pts),
           	LINEARRING = ,
           	MULTILINESTRING25D = stop(paste(getGeometryType(geom), "not supported"))
		)
	}
	o = openOGRLayer(dsn, layer)
	ids = getIDs(o)
	geom = sfc(lapply(ids, function(id) readFeature(o, id)))
	f = lapply(ids, function(id) getFields(getFeature(o, id)))
	df = data.frame(F_IDS = ids, apply(do.call(rbind, f), 2, unlist))
	df$geom = geom
	sf(df)
}
