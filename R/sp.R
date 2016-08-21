#' @name ST_as.sf
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
ST_as.sf.Spatial = function(x, ...) {
	if ("data" %in% slotNames(x))
		df = x@data
	else 
		df = data.frame(row.names = row.names(x)) # empty
	df$geom = ST_as.sfc(geometry(x))
	ST_as.sf(df)
}

setCRS = function(lst, x) {
	p4 = x@proj4string@projargs
	if (is.na(p4))
		return(ST_sfc(lst, epsg = NA_integer_, proj4string = NA_character_))
	getEPSG = function(x) { # gets EPSG code out of proj4string:
		spl = strsplit(x, " ")[[1]]
		w = grep("+init=epsg:", spl)
		if (length(w) == 1)
			as.numeric(strsplit(spl[w], "+init=epsg:")[[1]][2])
		else
			NA_integer_
	}
	ST_sfc(lst, epsg = getEPSG(p4), proj4string = p4)
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

#' @name ST_as.sfc
#' @export
ST_as.sfc.SpatialPoints = function(x,...) {
	cc = x@coords
	lst = lapply(seq_len(nrow(cc)), function(x) ST_Point(cc[x,]))
	setCRS(lst, x)
}

#' @name ST_as.sfc
#' @export
ST_as.sfc.SpatialPixels = function(x,...) {
	ST_as.sfc(as(x, "SpatialPoints"))
}

#' @name ST_as.sfc
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
	geom = geometry(from)
	from[[attr(from, "sf_column")]] = NULL # remove sf column list
	addAttrToGeom(as_Spatial(geom), data.frame(from), match.ID = FALSE)
})

#setAs("sfc", "Spatial", function(from) {
as_Spatial = function(from) {
	zm = class(from[[1]])[1]
	if (zm %in% c("XYM", "XYZM"))
		stop("geometries containing M not supported by sp")
	StopZ = function(zm) { if (zm %in% c("XYZ", "XYZM")) stop("Z not supported") }
	switch(class(from)[1],
		"sfc_POINT" = sfc2SpatialPoints(from),
		"sfc_MULTIPOINT" = sfc2SpatialMultiPoints(from),
		"sfc_LINESTRING" = , "sfc_MULTILINESTRING" = { StopZ(zm); sfc2SpatialLines(from) },
		"sfc_POLYGON" = , "sfc_MULTIPOLYGON" = { StopZ(zm); sfc2SpatialPolygons(from) },
		stop(paste("conversion from feature type", class(from)[1], "to sp is not supported"))
	)
}
#)

sfc2SpatialPoints = function(from)
	SpatialPoints(do.call(rbind, from), proj4string = CRS(attr(from, "proj4string")))

sfc2SpatialMultiPoints = function(from)
	SpatialMultiPoints(lapply(from, unclass), proj4string = CRS(attr(from, "proj4string")))

sfc2SpatialLines = function(from, IDs = paste0("ID", 1:length(from))) {
	l = if (class(from)[1]  == "sfc_MULTILINESTRING")
		lapply(from, function(x) Lines(lapply(x, Line)))
	else 
		lapply(from, function(x) Lines(list(Line(x))))
	for (i in 1:length(from))
		l[[i]]@ID = IDs[i]
	SpatialLines(l, proj4string = CRS(attr(from, "proj4string")))
}

sfc2SpatialPolygons = function(from, IDs = paste0("ID", 1:length(from))) {
	l = if (class(from)[1] == "sfc_MULTIPOLYGON")
		lapply(from, function(x)  # for each sfc item, return a Polygons
				Polygons(unlist(lapply(x, function(y) # to each sub-polygon,
					lapply(y, function(z) Polygon(z[rev(1:nrow(z)),]))), 
						recursive = FALSE), "ID"))
	else lapply(from, function(x) 
		Polygons(lapply(x, function(y) Polygon(y[rev(1:nrow(y)),])), "ID"))
	for (i in 1:length(from))
		l[[i]]@ID = IDs[i]
	SpatialPolygons(l, proj4string = CRS(attr(from, "proj4string")))
	# TODO: add comments()
}
