## Method coordinates
## @name coordinates
## @exportMethod coordinates
#if (!isGeneric("coordinates"))
#    setGeneric("coordinates", function(obj, ...)
#		standardGeneric("coordinates"))
#
#setMethod("coordinates", "sfc_POINT",
#	function(obj, ...)
#		do.call(rbind, obj)
#)
#
#setMethod("coordinates", "sfc",
#	function(obj, ...)
#		stop("coordinates for this object type not implemented")
#)
#
#setMethod("coordinates", "sf",
#	function(obj, ...)
#		coordinates(st_geometry(obj), ...)
#)


#' @name st_as_sf
#' @examples
#' library(sp)
#' x = rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))
#' x1 = 0.1 * x + 0.1
#' x2 = 0.1 * x + 0.4
#' x3 = 0.1 * x + 0.7
#' y = x + 3
#' y1 = x1 + 3
#' y3 = x3 + 3
#' m = matrix(c(3, 0), 5, 2, byrow = TRUE)
#' z = x + m
#' z1 = x1 + m
#' z2 = x2 + m
#' z3 = x3 + m
#' p1 = Polygons(list( Polygon(x[5:1,]), Polygon(x2), Polygon(x3), 
#'    Polygon(y[5:1,]), Polygon(y1), Polygon(x1), Polygon(y3)), "ID1")
#' p2 = Polygons(list( Polygon(z[5:1,]), Polygon(z2), Polygon(z3), Polygon(z1)),
#'   "ID2")
#' if (require("rgeos")) {
#'   r = createSPComment(SpatialPolygons(list(p1,p2)))
#'   comment(r)
#'   comment(r@polygons[[1]])
#'   scan(text = comment(r@polygons[[1]]), quiet = TRUE)
#'   library(sf)
#'   a = st_as_sf(r)
#'   summary(a)
#' }
#' demo(meuse, ask = FALSE, echo = FALSE)
#' summary(st_as_sf(meuse))
#' summary(st_as_sf(meuse.grid))
#' summary(st_as_sf(meuse.area))
#' summary(st_as_sf(meuse.riv))
#' summary(st_as_sf(as(meuse.riv, "SpatialLines")))
#' pol.grd = as(meuse.grid, "SpatialPolygonsDataFrame")
#' summary(st_as_sf(pol.grd))
#' summary(st_as_sf(as(pol.grd, "SpatialLinesDataFrame")))
#' @export
st_as_sf.Spatial = function(x, ...) {
	if ("data" %in% slotNames(x))
		df = x@data
	else 
		df = data.frame(row.names = row.names(x)) # empty
	if ("geometry" %in% names(df))
		warning("column \"geometry\" will be overwritten by geometry column")
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	df$geometry = st_as_sfc(sp::geometry(x), ...)
	st_as_sf(df)
}


#' Convert foreign geometry object to an sfc object
#'
#' Convert foreign geometry object to an sfc object
#' @param x object to convert
#' @param ... further arguments
#' @param forceMulti logical; if \code{TRUE}, force coercion into \code{MULTIPOLYGON} or \code{MULTILINE} objects, else autodetect
#' @name st_as_sfc
#' @export
st_as_sfc = function(x, ...) UseMethod("st_as_sfc")

#' @name st_as_sfc
#' @export
st_as_sfc.SpatialPoints = function(x,...) {
	cc = x@coords
	dimnames(cc) = NULL
	lst = lapply(seq_len(nrow(cc)), function(x) st_point(cc[x,]))
	do.call(st_sfc, c(lst, crs = x@proj4string@projargs))
}

#' @name st_as_sfc
#' @export
st_as_sfc.SpatialPixels = function(x,...) {
	st_as_sfc(as(x, "SpatialPoints"))
}

#' @name st_as_sfc
#' @export
st_as_sfc.SpatialMultiPoints = function(x,...) {
	lst = lapply(x@coords, st_multipoint)
	do.call(st_sfc, c(lst, crs = x@proj4string@projargs))
}

#' @name st_as_sfc
#' @export
st_as_sfc.SpatialLines = function(x, ..., forceMulti = FALSE) {
	lst = if (forceMulti || any(sapply(x@lines, function(x) length(x@Lines)) != 1))
		lapply(x@lines, 
			function(y) st_multilinestring(lapply(y@Lines, function(z) z@coords)))
	else
		lapply(x@lines, function(y) st_linestring(y@Lines[[1]]@coords))
	do.call(st_sfc, c(lst, crs = x@proj4string@projargs))
}

#' @name st_as_sfc
#' @export
st_as_sfc.SpatialPolygons = function(x, ..., forceMulti = FALSE) {
	lst = if (forceMulti || any(sapply(x@polygons, function(x) moreThanOneOuterRing(x@Polygons)))) {
		if (is.null(comment(x)) || comment(x) == "FALSE") {
			if (!requireNamespace("rgeos", quietly = TRUE))
				stop("package rgeos required for finding out which hole belongs to which exterior ring")
			x = rgeos::createSPComment(x)
		}
		lapply(x@polygons, function(y) 
			st_multipolygon(Polygons2MULTIPOLYGON(y@Polygons, comment(y))))
	} else
		lapply(x@polygons, function(y) st_polygon(Polygons2POLYGON(y@Polygons)))
	do.call(st_sfc, c(lst, crs = x@proj4string@projargs))
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

setAs("Spatial", "sf", function(from) st_as_sf(from))

setAs("Spatial", "sfc", function(from) st_as_sfc(from))

setAs("sf", "Spatial", function(from) {
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	geom = st_geometry(from)
	from[[attr(from, "sf_column")]] = NULL # remove sf column list
	sp::addAttrToGeom(as(geom, "Spatial"), data.frame(from), match.ID = FALSE)
})

setAs("sfc", "Spatial", function(from) as_Spatial(from))

# setAs("sfg", "Spatial", function(from) as(st_sfc(from), "Spatial"))
##  doesn't work for:
## as(st_point(0:1), "Spatial")

#' Convert to sp object
#' 
#' @param from sfc to convert
#' @param cast logical. Coerce GEOMETRIES to a type using `st_cast` (default TRUE)
as_Spatial = function(from, cast = TRUE) {
  if (cast) {
    from <- st_cast(from)
  }
	zm = class(from[[1]])[1]
	if (zm %in% c("XYM", "XYZM"))
		stop("geometries containing M not supported by sp")
	StopZ = function(zm) { if (zm %in% c("XYZ", "XYZM")) 
		stop("Z not supported: try st_drop_zm first?") }
	switch(class(from)[1],
		"sfc_POINT" = sfc2SpatialPoints(from),
		"sfc_MULTIPOINT" = sfc2SpatialMultiPoints(from),
		"sfc_LINESTRING" = , "sfc_MULTILINESTRING" = { StopZ(zm); sfc2SpatialLines(from) },
		"sfc_POLYGON" = , "sfc_MULTIPOLYGON" = { StopZ(zm); sfc2SpatialPolygons(from) },
		stop(paste("conversion from feature type", class(from)[1], "to sp is not supported"))
	)
}

sfc2SpatialPoints = function(from) {
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	sp::SpatialPoints(do.call(rbind, from), proj4string = sp::CRS(attr(from, "crs")$proj4string))
}

sfc2SpatialMultiPoints = function(from) {
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	sp::SpatialMultiPoints(lapply(from, unclass), proj4string = 
		sp::CRS(attr(from, "crs")$proj4string))
}

sfc2SpatialLines = function(from, IDs = paste0("ID", 1:length(from))) {
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	l = if (class(from)[1]  == "sfc_MULTILINESTRING")
		lapply(from, function(x) sp::Lines(lapply(x, function(y) sp::Line(unclass(y))), "ID"))
	else 
		lapply(from, function(x) sp::Lines(list(sp::Line(unclass(x))), "ID"))
	for (i in 1:length(from))
		l[[i]]@ID = IDs[i]
	sp::SpatialLines(l, proj4string = sp::CRS(attr(from, "crs")$proj4string))
}

sfc2SpatialPolygons = function(from, IDs = paste0("ID", 1:length(from))) {
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	l = if (class(from)[1] == "sfc_MULTIPOLYGON")
		lapply(from, function(x)  # for each sfc item, return a Polygons
				sp::Polygons(unlist(lapply(x, function(y) # to each sub-polygon,
					lapply(seq_along(y), function(i) sp::Polygon(y[[i]], i > 1))), 
						recursive = FALSE), "ID"))
	else lapply(from, function(x) 
		sp::Polygons(lapply(seq_along(x), function(i) sp::Polygon(x[[i]], i > 1)), "ID"))

	# set comment: ?Polygons: "Exterior rings are coded zero, while interior rings are 
	# coded with the 1-based index of the exterior ring to which they belong.":
	for (i in 1:length(from)) {
		l[[i]]@ID = IDs[i]
		if (class(from)[1] == "sfc_MULTIPOLYGON")
			comm = get_comment(from[[i]])
		else
			comm = c(0, rep(1, length(from[[i]])-1))
		comment(l[[i]]) = paste(as.character(comm), collapse = " ")
	}
	sp::SpatialPolygons(l, proj4string = sp::CRS(attr(from, "crs")$proj4string))
}

get_comment = function(mp) { # for MULTIPOLYGON
	l = lapply(mp, function(from) c(0, rep(1, length(from) - 1)))
	offset = 0
	for (i in 1:length(l)) {
		l[[i]] = l[[i]] + offset
		offset = offset + length(l[[i]])
		l[[i]][1] = 0
	}
	unlist(l)
}
