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
#' r = SpatialPolygons(list(p1,p2))
#' a = suppressWarnings(st_as_sf(r))
#' summary(a)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' summary(st_as_sf(meuse))
#' summary(st_as_sf(meuse.grid))
#' summary(st_as_sf(meuse.area))
#' summary(st_as_sf(meuse.riv))
#' summary(st_as_sf(as(meuse.riv, "SpatialLines")))
#' pol.grd = as(meuse.grid, "SpatialPolygonsDataFrame")
#' # summary(st_as_sf(pol.grd))
#' # summary(st_as_sf(as(pol.grd, "SpatialLinesDataFrame")))
#' @export
st_as_sf.Spatial = function(x, ...) {
	if ("data" %in% slotNames(x))
		df = x@data
	else
		df = data.frame(row.names = row.names(x)) # empty
	if ("geometry" %in% names(df))
		warning("column \"geometry\" will be overwritten by geometry column")
	if (! requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	if (sp::gridded(x) && sp::fullgrid(x))
		sp::fullgrid(x) = FALSE
	df$geometry = st_as_sfc(sp::geometry(x), ...)
	st_as_sf(df)
}


#' Convert foreign geometry object to an sfc object
#'
#' Convert foreign geometry object to an sfc object
#' @param x object to convert
#' @param ... further arguments
#' @param precision precision value; see \link{st_as_binary}
#' @param forceMulti logical; if \code{TRUE}, force coercion into \code{MULTIPOLYGON} or \code{MULTILINE} objects, else autodetect
#' @name st_as_sfc
#' @export
st_as_sfc = function(x, ...) UseMethod("st_as_sfc")

handle_bbox = function(sfc, sp) {
	bb = structure(bb_wrap(as.vector(sp::bbox(sp)[1:2,])), class = "bbox")
	structure(sfc, "bbox" = bb)
}

#' @name st_as_sfc
#' @export
st_as_sfc.SpatialPoints = function(x, ..., precision = 0.0) {
	cc = x@coords
	dimnames(cc) = NULL
	lst = lapply(seq_len(nrow(cc)), function(x) st_point(cc[x,]))
	handle_bbox(do.call(st_sfc, append(lst, list(crs = st_crs(x@proj4string), 
		precision = precision))), x)
}

#' @name st_as_sfc
#' @export
st_as_sfc.SpatialPixels = function(x, ..., precision = 0.0) {
	handle_bbox(st_as_sfc(as(x, "SpatialPoints"), precision = precision), x)
}


#' @name st_as_sfc
#' @export
st_as_sfc.SpatialMultiPoints = function(x, ..., precision = 0.0) {
	lst = lapply(x@coords, st_multipoint)
	handle_bbox(do.call(st_sfc, append(lst, list(crs = st_crs(x@proj4string),
		precision = precision))), x)
}

#' @name st_as_sfc
#' @export
st_as_sfc.SpatialLines = function(x, ..., precision = 0.0, forceMulti = FALSE) {
	lst = if (forceMulti || any(sapply(x@lines, function(x) length(x@Lines)) != 1))
		lapply(x@lines, function(y) {
                    crd_list <- lapply(y@Lines, function(z) z@coords)
                    crd_list1 <- lapply(crd_list, function(z) {
                         if (nrow(z) < 2L) res <- z[0,]
                         else res <- z
                         res
                    })
                    st_multilinestring(crd_list1)
                })
	else
		lapply(x@lines, function(y) {
                    crds = y@Lines[[1]]@coords
                    if (nrow(crds) < 2L) res = st_linestring()
                    else res = st_linestring(crds)
                    res
                })
	handle_bbox(do.call(st_sfc, append(lst, list(crs = st_crs(x@proj4string),
		precision = precision))), x)
}

#' @name st_as_sfc
#' @export
st_as_sfc.SpatialPolygons = function(x, ..., precision = 0.0, forceMulti = FALSE) {
	lst = if (forceMulti || any(sapply(x@polygons, function(x) moreThanOneOuterRing(x@Polygons)))) {
		if (is.null(comment(x)) || comment(x) == "FALSE") {
#			if (!requireNamespace("rgeos", quietly = TRUE))
#				stop("package rgeos required for finding out which hole belongs to which exterior ring")
#			x = rgeos::createSPComment(x)
			# https://github.com/r-spatial/sf/pull/1869/files/7f1921c9acc1000b92a81b3a0aa7126330d4ef12..cfa303c8fcdd0b9a7ea33eae402c1135bb8e50ba :
			# warning("no comment found showing which hole belongs to which exterior ring")
			# (warning causes revdep problem in pkg amt)
			process_pl_comment <- function(pl) {
				ID <- slot(pl, "ID")
				crds <- lapply(slot(pl, "Polygons"), function(xx) slot(xx, "coords"))
				raw <- st_sfc(st_polygon(crds))
				val <- st_make_valid(raw)
				if (inherits(val, "sfc_GEOMETRYCOLLECTION"))
					val = st_collection_extract(val, "POLYGON")
				res <- slot(as(val, "Spatial"), "polygons")[[1]]
				slot(res, "ID") <- ID
				res
			}
			slot(x, "polygons") <- lapply(slot(x, "polygons"), process_pl_comment)
			comment(x) <- "TRUE"
		}
		lapply(x@polygons, function(y)
			st_multipolygon(Polygons2MULTIPOLYGON(y@Polygons, comment(y))))
	} else
		lapply(x@polygons, function(y) st_polygon(Polygons2POLYGON(y@Polygons)))
	handle_bbox(do.call(st_sfc, append(lst, list(crs = st_crs(x@proj4string),
		precision = precision))), x)
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

#' @name as
#' @rdname coerce-methods
#' @aliases coerce,Spatial,sf-method
setAs("Spatial", "sf", function(from) st_as_sf(from))

#' @name as
#' @rdname coerce-methods
#' @aliases coerce,Spatial,sfc-method
setAs("Spatial", "sfc", function(from) st_as_sfc(from))

#' @name as
#' @rdname coerce-methods
#' @aliases coerce,sf,Spatial-method
setAs("sf", "Spatial", function(from) {
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	geom = st_geometry(from)
	from[[attr(from, "sf_column")]] = NULL # remove sf column list
	sp::addAttrToGeom(as_Spatial(geom, IDs = row.names(from)),
		data.frame(from), match.ID = FALSE)
})

#' @name as
#' @rdname coerce-methods
#' @aliases coerce,sfc,Spatial-method
setAs("sfc", "Spatial", function(from) as_Spatial(from))

# create empy class
setOldClass("XY")
setAs("XY", "Spatial", function(from) as(st_sfc(from), "Spatial"))

#' Methods to coerce simple features to `Spatial*` and `Spatial*DataFrame` objects
#'
#' [as_Spatial()] allows to convert `sf` and `sfc` to `Spatial*DataFrame` and
#' `Spatial*` for `sp` compatibility. You can also use `as(x, "Spatial")` To transform
#' `sp` objects to `sf` and `sfc` with `as(x, "sf")`.
#' @rdname coerce-methods
#' @name as_Spatial
#' @md
#' @param from object of class `sf`, `sfc_POINT`, `sfc_MULTIPOINT`, `sfc_LINESTRING`,
#' `sfc_MULTILINESTRING`, `sfc_POLYGON`, or `sfc_MULTIPOLYGON`.
#' @param cast logical; if `TRUE`, [st_cast()] `from` before converting, so that e.g.
#' `GEOMETRY` objects with a mix of `POLYGON` and `MULTIPOLYGON` are cast to `MULTIPOLYGON`.
#' @param IDs character vector with IDs for the `Spatial*` geometries
#' @details Package \code{sp} supports three dimensions for `POINT` and `MULTIPOINT` (`SpatialPoint*`).
#' Other geometries must be two-dimensional (`XY`). Dimensions can be dropped using
#' [st_zm()] with `what = "M"` or `what = "ZM"`.
#'
#' For converting simple features (i.e., \code{sf} objects) to their \code{Spatial} counterpart, use \code{as(obj, "Spatial")}
#' @return geometry-only object deriving from `Spatial`, of the appropriate class
#' @export
#' @examples
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' # convert to SpatialPolygonsDataFrame
#' spdf <- as_Spatial(nc)
#' # identical to
#' spdf <- as(nc, "Spatial")
#' # convert to SpatialPolygons
#' as(st_geometry(nc), "Spatial")
#' # back to sf
#' as(spdf, "sf")
as_Spatial = function(from, cast = TRUE, IDs = paste0("ID", seq_along(from))) {
	if (inherits(from, "sf")) {
		geom = st_geometry(from)
		from[[attr(from, "sf_column")]] = NULL # remove sf column list
		sp::addAttrToGeom(as_Spatial(geom, cast = cast, IDs = row.names(from)),
						  data.frame(from), match.ID = FALSE)
	} else {
		.as_Spatial(from, cast, IDs)
	}
}

.as_Spatial = function(from, cast = TRUE, IDs = paste0("ID", seq_along(from))) {
	if (cast)
		from = st_cast(from)
	zm = class(from[[1]])[1]
	if (zm %in% c("XYM", "XYZM"))
		stop("geometries containing M not supported by sp\n",
			 'use `st_zm(..., what = "M")`')
	if (any(st_is_empty(from)))
		stop("empty geometries are not supported by sp classes: conversion failed")
	StopZ = function(zm) { 
		if (zm == "XYZ")
			stop("sp supports Z dimension only for POINT and MULTIPOINT.\n",
				 'use `st_zm(...)` to coerce to XY dimensions')
	}
	switch(class(from)[1],
		"sfc_POINT" = sfc2SpatialPoints(from),
#		"sfc_POINT" = sfc2SpatialPoints(from, IDs),
		"sfc_MULTIPOINT" = sfc2SpatialMultiPoints(from),
		"sfc_LINESTRING" = , "sfc_MULTILINESTRING" = { StopZ(zm); sfc2SpatialLines(from, IDs) },
		"sfc_POLYGON" = , "sfc_MULTIPOLYGON" = { StopZ(zm); sfc2SpatialPolygons(from, IDs) },
		stop(paste("conversion from feature type", class(from)[1], "to sp is not supported"))
	)
}

sfc2SpatialPoints = function(from, IDs) {
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	sp::SpatialPoints(do.call(rbind, from), proj4string = as(st_crs(from), "CRS"))
}

sfc2SpatialMultiPoints = function(from) {
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	sp::SpatialMultiPoints(lapply(from, unclass), 
		proj4string = as(st_crs(from), "CRS"))
}

sfc2SpatialLines = function(from, IDs = paste0("ID", seq_along(from))) {
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	l = if (class(from)[1]  == "sfc_MULTILINESTRING")
		lapply(from, function(x) sp::Lines(lapply(x, function(y) sp::Line(unclass(y))), "ID"))
	else
		lapply(from, function(x) sp::Lines(list(sp::Line(unclass(x))), "ID"))
	for (i in seq_along(from))
		l[[i]]@ID = IDs[i]
	sp::SpatialLines(l, proj4string = as(st_crs(from), "CRS"))
}

sfc2SpatialPolygons = function(from, IDs = paste0("ID", seq_along(from))) {
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
	for (i in seq_along(from)) {
		l[[i]]@ID = IDs[i]
		if (class(from)[1] == "sfc_MULTIPOLYGON")
			comm = get_comment(from[[i]])
		else
			comm = c(0, rep(1, length(from[[i]])-1))
		comment(l[[i]]) = paste(as.character(comm), collapse = " ")
	}
	sp::SpatialPolygons(l, proj4string = as(st_crs(from), "CRS"))
}

get_comment = function(mp) { # for MULTIPOLYGON
	l = lapply(mp, function(from) c(0, rep(1, length(from) - 1)))
	offset = 0
	for (i in seq_along(l)) {
		l[[i]] = l[[i]] + offset
		offset = offset + length(l[[i]])
		l[[i]][1] = 0
	}
	unlist(l)
}

#' @name as
#' @rdname coerce-methods
#' @aliases coerce,crs,CRS-method
setAs("crs", "CRS", function(from) CRS_from_crs(from))
CRS_from_crs = function(from) {
	if (! requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	nm <- "CRS"
	attr(nm, "package") <- "sp" # See ?new:
	obj <- new(nm, projargs = from$proj4string)
	if (!is.na(from$wkt) && CPL_proj_version() >= "6.0.0" && CPL_gdal_version() >= "3.0.0")
		comment(obj) <- from$wkt
	obj
	# we don't use sp::CRS(SRS_string = from$wkt) as rgdal may not be available,
	# which would break, and from$wkt has already been validated by GDAL:
}
