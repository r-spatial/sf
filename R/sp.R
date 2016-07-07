# convert to, and from, sp::Spatial objects

#' convert foreign object to an sf object
#'
#' convert foreign object to an sf object
#' @param x object to convert
#' @export
#' @examples
#' x = rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))
#' x1 = 0.1 * x + 0.1
#' x2 = 0.1 * x + 0.4
#' x3 = 0.1 * x + 0.7
#' y = x + 3
#' y1 = x1 + 3
#' y2 = x2 + 3
#' y3 = x3 + 3
#' library(sp)
#' p = Polygons(list( Polygon(x[5:1,]), Polygon(x2), Polygon(y2), Polygon(x3), 
#'    Polygon(y[5:1,]), Polygon(y1), Polygon(x1), Polygon(y3)), "ID1")
#' r = rgeos::createSPComment(SpatialPolygons(list(p)))
#' comment(r)
#' comment(r@polygons[[1]])
#' scan(text = comment(r@polygons[[1]]), quiet = TRUE)
#' library(sf)
#' a = as.sf(r)
#' summary(a)
as.sf = function(x) UseMethod("as.sf")

#' convert foreign object to an sf object
#'
#' convert foreign object to an sf object
#' @param x object deriving from \link[sp]{Spatial}
#' @examples
#' library(sp)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' summary(as.sf(meuse))
#' summary(as.sf(meuse.grid))
#' summary(as.sf(meuse.area))
#' summary(as.sf(meuse.riv))
#' summary(as.sf(as(meuse.riv, "SpatialLines")))
#' pol.grd = as(meuse.grid, "SpatialPolygonsDataFrame")
#' summary(as.sf(pol.grd))
#' summary(as.sf(as(pol.grd, "SpatialLinesDataFrame")))
#' @export
as.sf.Spatial = function(x) {
	if ("data" %in% slotNames(x))
		df = x@data
	else 
		df = data.frame(row.names = row.names(x)) # empty
	df$geom = as.sfc(geometry(x))
	sf(df)
}

setCRS = function(lst, x) {
	p4 = x@proj4string@projargs
	if (is.na(p4))
		return(sfc(lst, epsg = NA, proj4string = NA))
	getEPSG = function(x) { # gets EPSG code out of proj4string:
		spl = strsplit(x, " ")[[1]]
		w = grep("+init=epsg:", spl)
		if (length(w) == 1) {
			epsg = as.numeric(strsplit(spl[w], "+init=epsg:")[[1]][2])
		} else
			NA
	}
	epsg = getEPSG(p4)
	sfc(lst, epsg, proj4string = p4)
}

#' convert foreign geometry object to an sfc object
#'
#' convert foreign geometry object to an sfc object
#' @param x object to convert
#' @param ... further arguments
#' @export
as.sfc = function(x, ...) UseMethod("as.sfc")

#' @export
as.sfc.SpatialPoints = function(x,...) {
	cc = x@coords
	lst = lapply(seq_len(nrow(cc)), function(x) POINT(cc[x,]))
	setCRS(lst, x)
}
#' @export
as.sfc.SpatialPixels = function(x,...) {
	as.sfc(as(x, "SpatialPoints"))
}
#' @export
as.sfc.SpatialMultiPoints = function(x,...) {
	lst = lapply(x@coords, MULTIPOINT)
	setCRS(lst, x)
}
#' coerce SpatialLines object into sfc
#'
#' coerce SpatialLines object into sfc
#' @param x object of class \link[sp]{SpatialLines}
#' @param ... ignored
#' @param forceMulti logical: force coercion into MultiPolygon objects (TRUE), or autodetect
#' @export
as.sfc.SpatialLines = function(x, ..., forceMulti = FALSE) {
	lst = if (forceMulti || any(sapply(x@lines, function(x) length(x@Lines)) != 1))
		lapply(x@lines, 
			function(y) MULTILINESTRING(lapply(y@Lines, function(z) z@coords)))
	else
		lapply(x@lines, function(y) LINESTRING(y@Lines[[1]]@coords))
	setCRS(lst, x)
}
#' coerce SpatialPolygons object into sfc
#'
#' coerce SpatialPolygons object into sfc
#' @param x object of class \link[sp]{SpatialPolygons}
#' @param ... ignored
#' @param forceMulti logical: force coercion into MultiPolygon objects (TRUE), or autodetect
#' @export
as.sfc.SpatialPolygons = function(x, ..., forceMulti = FALSE) {
	lst = if (forceMulti || any(sapply(x@polygons, function(x) moreThanOneOuterRing(x@Polygons)))) {
		if (comment(x) == "FALSE")
			x = rgeos::createSPComment(x)
		lapply(x@polygons, function(y) 
			MULTIPOLYGON(Polygons2MULTIPOLYGON(y@Polygons, comment(y))))
	} else
		lapply(x@polygons, function(y) POLYGON(Polygons2POLYGON(y@Polygons)))
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
	lapply(PolygonsLst, function(x) x@coords)
}
