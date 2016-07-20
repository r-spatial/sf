# convert to, and from, sp::Spatial objects

#' convert foreign object to an sf object
#'
#' convert foreign object to an sf object
#' @export
ST_as.sf = function(x) UseMethod("ST_as.sf")

#' @name ST_as.sf
#' @param x object deriving from \link[sp]{Spatial}
#' @examples
#' library(sp)
#' x = rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))
#' x1 = 0.1 * x + 0.1
#' x2 = 0.1 * x + 0.4
#' x3 = 0.1 * x + 0.7
#' y = x + 3
#' y1 = x1 + 3
#' y2 = x2 + 3
#' y3 = x3 + 3
#' p = Polygons(list( Polygon(x[5:1,]), Polygon(x2), Polygon(y2), Polygon(x3), 
#'    Polygon(y[5:1,]), Polygon(y1), Polygon(x1), Polygon(y3)), "ID1")
#' if (require("rgeos")) {
#'   r = createSPComment(SpatialPolygons(list(p)))
#'   comment(r)
#'   comment(r@polygons[[1]])
#'   scan(text = comment(r@polygons[[1]]), quiet = TRUE)
#'   library(sf)
#'   a = ST_as.sf(r)
#'   summary(a)
#' }
#' demo(meuse, ask = FALSE, echo = FALSE)
#' summary(ST_as.sf(meuse))
#' summary(ST_as.sf(meuse.grid))
#' summary(ST_as.sf(meuse.area))
#' summary(ST_as.sf(meuse.riv))
#' summary(ST_as.sf(as(meuse.riv, "SpatialLines")))
#' pol.grd = as(meuse.grid, "SpatialPolygonsDataFrame")
#' summary(ST_as.sf(pol.grd))
#' summary(ST_as.sf(as(pol.grd, "SpatialLinesDataFrame")))
#' @export
ST_as.sf.Spatial = function(x) {
	if ("data" %in% slotNames(x))
		df = x@data
	else 
		df = data.frame(row.names = row.names(x)) # empty
	df$geom = ST_as.sfc(geometry(x))
	ST_sf(df)
}

setCRS = function(lst, x) {
	p4 = x@proj4string@projargs
	if (is.na(p4))
		return(ST_sfc(lst, epsg = as.integer(NA), proj4string = as.character(NA)))
	getEPSG = function(x) { # gets EPSG code out of proj4string:
		spl = strsplit(x, " ")[[1]]
		w = grep("+init=epsg:", spl)
		if (length(w) == 1) {
			epsg = as.numeric(strsplit(spl[w], "+init=epsg:")[[1]][2])
		} else
			NA
	}
	epsg = getEPSG(p4)
	ST_sfc(lst, epsg, proj4string = p4)
}

#' convert foreign geometry object to an sfc object
#'
#' convert foreign geometry object to an sfc object
#' @param x object to convert
#' @param ... further arguments
#' @param forceMulti logical; if \code{TRUE}, force coercion into \code{MULTIPOLYGON} or \code{MULTILINE} objects, else autodetect
#' @name ST_as.sfc
#' @export
ST_as.sfc = function(x, ...) UseMethod("ST_as.sfc")

#' @export
ST_as.sfc.SpatialPoints = function(x,...) {
	cc = x@coords
	lst = lapply(seq_len(nrow(cc)), function(x) ST_Point(cc[x,]))
	setCRS(lst, x)
}
#' @export
ST_as.sfc.SpatialPixels = function(x,...) {
	ST_as.sfc(as(x, "SpatialPoints"))
}
#' @export
ST_as.sfc.SpatialMultiPoints = function(x,...) {
	lst = lapply(x@coords, ST_MultiPoint)
	setCRS(lst, x)
}
#' @name ST_as.sfc
#' @export
ST_as.sfc.SpatialLines = function(x, ..., forceMulti = FALSE) {
	lst = if (forceMulti || any(sapply(x@lines, function(x) length(x@Lines)) != 1))
		lapply(x@lines, 
			function(y) ST_MultiLineString(lapply(y@Lines, function(z) z@coords)))
	else
		lapply(x@lines, function(y) ST_LineString(y@Lines[[1]]@coords))
	setCRS(lst, x)
}
#' @name ST_as.sfc
#' @export
ST_as.sfc.SpatialPolygons = function(x, ..., forceMulti = FALSE) {
	lst = if (forceMulti || any(sapply(x@polygons, function(x) moreThanOneOuterRing(x@Polygons)))) {
		if (comment(x) == "FALSE") {
			if (!requireNamespace("rgeos", quietly = TRUE))
				stop("package rgeos required for finding out which hole belongs to which exterior ring")
			x = rgeos::createSPComment(x)
		}
		lapply(x@polygons, function(y) 
			ST_MultiPolygon(Polygons2MULTIPOLYGON(y@Polygons, comment(y))))
	} else
		lapply(x@polygons, function(y) ST_Polygon(Polygons2POLYGON(y@Polygons)))
	setCRS(lst, x)
}

moreThanOneOuterRing = function(PolygonsLst) {
	holes = sapply(PolygonsLst, function(x) x@hole)
	length(holes) - length(which(holes)) > 1
}

Polygons2MULTIPOLYGON = function(PolygonsLst, cmt) {
	idx = scan(text = cmt, quiet = TRUE)
	# idx tells which outer rings (0) enclose which holes (idx == which(idx == 0))
	outer_rings = which(idx == 0)
	# loop over outer_rings:
	lapply(outer_rings, function(x) Polygons2POLYGON(PolygonsLst[c(x, which(idx == x))]))
}

Polygons2POLYGON = function(PolygonsLst) {
	# here we have one outer ring, followed by (0+) holes inside this ring
	lapply(PolygonsLst, function(x) x@coords[rev(1:nrow(x@coords)),])
}

setAs("sf", "Spatial", function(from) {
	df = from
	# remove geometry column:
	df[[attr(df, "sf_column")]] = NULL
	addAttrToGeom(as(geometry(from), "Spatial"), as.data.frame(df), match.ID = FALSE)
})

setAs("sfc", "Spatial", function(from) {
	switch(attr(from, "type"),
		"POINT" = , "POINT Z" = sfc2SpatialPoints(from),
		"MULTIPOINT" = , "MULTIPOINT Z" = sfc2SpatialMultiPoints(from),
		"LINESTRING" = , "MULTILINESTRING" = sfc2SpatialLines(from),
		"POLYGON" = , "MULTIPOLYGON" = sfc2SpatialPolygons(from),
		"POINT ZM" = , "MULTIPOINT ZM" = ,
		"POINT M" = , "MULTIPOINT M" = , "LINESTRING M" = , "POLYGON M" = , 
		"MULTIPOLYGON M" = , "MULTILINESTRING M" = 
			stop("geometries containing M not supported by sp"),
		"LINESTRING Z" = , "LINESTRING ZM" = , "MULTILINESTRING Z" = , "MULTILINESTRING ZM" = 
			stop("Z or ZM linestrings not supported by sp"),
		"POLYGON Z" = , "POLYGON ZM" = , "MULTIPOLYGON Z" = , "MULTIPOLYGON ZM" = 
			stop("Z or ZM (multi)polygons not supported by sp"),
		stop("converstion from this feature type to sp is not supported")
	)
})

sfc2SpatialPoints = function(from)
	SpatialPoints(do.call(rbind(from)), proj4string = CRS(attr(from, "proj4string")))

sfc2SpatialMultiPoints = function(from)
	SpatialMultiPoints(from, proj4string = CRS(attr(from, "proj4string")))

sfc2SpatialLines = function(from, IDs = paste0("ID", 1:length(from))) {
	l = if (attr(from, "type") == "MULTILINESTRING")
		lapply(from, function(x) Lines(lapply(x, Line)))
	else lapply(from, function(x) Lines(list(Line(x))))
	for (i in 1:length(from))
		l[[i]]@ID = IDs[i]
	SpatialLines(l, proj4string = CRS(attr(from, "proj4string")))
}

sfc2SpatialPolygons = function(from, IDs = paste0("ID", 1:length(from))) {
	l = if (attr(from, "type") == "MULTIPOLYGON")
		lapply(from, function(x)  # for each sfc item, return a Polygons
				Polygons(unlist(lapply(x, function(y) # to each sub-polygon,
					lapply(y, function(z) Polygon(z[rev(1:nrow(z)),]))), 
						recursive = FALSE), "ID"))
	else lapply(from, function(x) 
		Polygons(lapply(x, function(y) Polygon(y[rev(1:nrow(y)),])), "ID"))
	for (i in 1:length(from))
		l[[i]]@ID = IDs[i]
	SpatialPolygons(l, proj4string = CRS(attr(from, "proj4string")))
	# TODO: add comments() ?
}
