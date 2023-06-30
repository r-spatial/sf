#' @export
str.sfc <- function(object, ...) {
	n <- length(object)
	cat(paste0(class(object)[1], " of length ", n))
	if (n > 0) {
		cat("; first list element: ")
		str(object[[1]], ...)
	}
}

#' @export
format.sfc = function(x, ..., width = 30) {
	vapply(x, format, "", ..., width = width)
}

#' Create simple feature geometry list column
#'
#' Create simple feature geometry list column, set class, and add coordinate reference system and precision
#'
#' @name sfc
#' @aliases sfc_POINT sfc_LINESTRING sfc_POLYGON sfc_MULTIPOINT sfc_MULTILINESTRING sfc_MULTIPOLYGON sfc_GEOMETRYCOLLECTION
#' @param ... zero or more simple feature geometries (objects of class \code{sfg}), or a single list of such objects; \code{NULL} values will get replaced by empty geometries.
#' @param crs coordinate reference system: integer with the EPSG code, or character with proj4string
#' @param precision numeric; see \link{st_as_binary}
#' @param check_ring_dir see \link{st_read}
#' @param dim character; if this function is called without valid geometries, this argument may carry the right dimension to set empty geometries
#' @param recompute_bbox logical; use \code{TRUE} to force recomputation of the bounding box
#' @return an object of class \code{sfc}, which is a classed list-column with simple feature geometries.
#'
#' @details A simple feature geometry list-column is a list of class
#' \code{c("stc_TYPE", "sfc")} which most often contains objects of identical type;
#' in case of a mix of types or an empty set, \code{TYPE} is set to the
#' superclass \code{GEOMETRY}.
#' @examples
#' pt1 = st_point(c(0,1))
#' pt2 = st_point(c(1,1))
#' (sfc = st_sfc(pt1, pt2))
#' d = st_sf(data.frame(a=1:2, geom=sfc))
#' @export
st_sfc = function(..., crs = NA_crs_, precision = 0.0, check_ring_dir = FALSE, dim,
				  recompute_bbox = FALSE) {
	lst = list(...)
	# if we have only one arg, which is already a list with sfg's, but NOT a geometrycollection:
	# (this is the old form of calling st_sfc; it is way faster to call st_sfc(lst) if lst
	# already contains a zillion sfg objects, than do.call(st_sfc, lst) ...
	if (length(lst) && inherits(lst[[1]], "sf"))
		stop("use st_as_sfc() to extract geometries from an sf object")
	if (length(lst) == 1 && is.list(lst[[1]]) && !inherits(lst[[1]], "sfg")
			&& (length(lst[[1]]) == 0 || inherits(lst[[1]][[1]], "sfg") || is.null(lst[[1]][[1]])))
		lst = lst[[1]]
	stopifnot(is.numeric(crs) || is.character(crs) || inherits(crs, "crs"))

	points_in_attr <- !is.null(attr(lst, "points"))

	# check for NULLs:
	a = attributes(lst)
	is_null = if (points_in_attr)
			rep(FALSE, length(lst))
		else
			sfc_is_null(lst)
	lst = unclass(lst)
	if (!points_in_attr)
		lst = lst[! is_null]

	attributes(lst) = a

	dims_and_types = if (points_in_attr)
			list(class_dim = attr(lst, "points_dim"), class_type = "POINT")
		else
			sfc_unique_sfg_dims_and_types(lst)
	
	cls = if (length(lst) == 0) # empty set: no geometries read
		c("sfc_GEOMETRY", "sfc")
	else {
		# class: do we have a mix of geometry types?
		single = if (!is.null(attr(lst, "single_type"))) # set by CPL_read_wkb:
				attr(lst, "single_type")
			else
				length(dims_and_types[[2]]) == 1L
		attr(lst, "single_type") = NULL # clean up
		if (single)
			c(paste0("sfc_", dims_and_types[[2]][1]), "sfc")
		else
			c("sfc_GEOMETRY", "sfc")    # the mix
	}

	if (any(is_null)) {
		if (missing(dim)) {
			dim = if (length(lst) == 0) # we have no clue:
					"XY"
				else
					dims_and_types[[1]][1]
		}
		ret = vector("list", length(is_null))
		ret[!is_null] = lst
		ret[ is_null] = list(typed_empty(cls, nchar(dim), dim = dim))
		attributes(ret) = attributes(lst)
		lst = ret
		dims_and_types = sfc_unique_sfg_dims_and_types(lst)
	}

	# set class:
	class(lst) = cls

	# set precision
	if (! missing(precision) || is.null(attr(lst, "precision")))
		attr(lst, "precision") = precision

	# compute bbox, if not set:
	bb = attr(lst, "bbox")
	if (is.null(bb) || anyNA(bb) || recompute_bbox)
		attr(lst, "bbox") = compute_bbox(lst)

	# compute z_range, if dims permit and not set
	zr = attr(lst, "z_range")
	if (is.null(zr) || anyNA(zr)) {
		u <- dims_and_types[[1]]
		if( "XYZM" %in% u ) {
			attr(lst, "z_range") = compute_z_range(lst)
			attr(lst, "m_range") = compute_m_range(lst)
		} else if ( "XYZ" %in% u ) {
			attr(lst, "z_range") = compute_z_range(lst)
		} else if ("XYM" %in% u ) {
			attr(lst, "m_range") = compute_m_range(lst)
		}
	}

	# check ring directions:
	if (check_ring_dir) # also GEOMETRYCOLLECTION?
		lst = check_ring_dir(lst)

	# get & set crs:
	if (is.na(crs) && !is.null(attr(lst, "crs")))
		crs = attr(lst, "crs")
	st_crs(lst) = crs

	# set classes attr in case of GEOMETRY
	if (inherits(lst, "sfc_GEOMETRY")) # recompute, as NULL's may have been substituted:
		attr(lst, "classes") = vapply(lst, class, rep(NA_character_, 3))[2L,]

	# set n_empty, check XY* is uniform:
	if (is.null(attr(lst, "n_empty")) || any(is_null)) { # n_empty is set by CPL_read_wkb:
		attr(lst, "n_empty") = sum(sfc_is_empty(lst))
# 		https://github.com/r-spatial/sf/issues/1592 :
#		if (length(u <- unique(sfg_classes[1L,])) > 1)
#			stop(paste("found multiple dimensions:", paste(u, collapse = " ")))
	}
	lst
}

#' @export
"[.sfc" = function(x, i, j, ..., op = st_intersects) {
	if (!missing(i) && (inherits(i, "sf") || inherits(i, "sfc") || inherits(i, "sfg")))
		i = lengths(op(x, i, ...)) != 0
	if (inherits(x, "sfc_POINT") && !is.null(attr(x, "points")))
		st_sfc(restore_points(x, i), crs = st_crs(x), precision = st_precision(x),
			dim = if(length(x)) class(x[[1]])[1] else "XY")
	else
		st_sfc(NextMethod() , crs = st_crs(x), precision = st_precision(x),
			dim = if(length(x)) class(x[[1]])[1] else "XY")
}


#' @export
"[<-.sfc" = function (x, i, value) {
	if (is.null(value) || inherits(value, "sfg")) {
		is_points = inherits(value, "POINT")
		value = list(value)
	} else
		is_points = inherits(value, "sfc_POINT")
	if (inherits(x, "sfc_POINT") && !is.null(attr(x, "points"))) {
		if (is_points) {
			repl = if (!is.null(pts <- attr(value, "points")))
					pts
				else
					do.call(rbind, value)
			attr(x, "points")[i, ] = repl
			return(structure(x, n_empty = sum(is.na(attr(x, "points")[,1])))) # RETURNS
		} else
			x = x[] # realize
	} 
	x = unclass(x) # becomes a list, but keeps attributes
	ret = st_sfc(NextMethod(), recompute_bbox = TRUE)
	structure(ret, n_empty = sum(sfc_is_empty(ret)))
}

#' @export
c.sfc = function(..., recursive = FALSE) {
	lst = list(...)
	chk_equal_crs(lst)
	classes = sapply(lst, function(x) class(x)[1])
	le = lengths(lst)
	if (any(le > 0))
		classes = classes[le > 0] # removes the empty set GEOMETRY objects
	ucls = unique(classes)
	cls = if (length(ucls) > 1) # a mix:
			c("sfc_GEOMETRY", "sfc")
		else
			c(ucls, "sfc")

	points_attr = sapply(lst, function(x) !is.null(attr(x, "points")))
	if (any(points_attr) && !all(points_attr)) {
		for (i in seq_along(lst))
			lst[[i]] = lst[[i]][] # realize
		points_attr = FALSE
	}

	ret = unlist(lapply(lst, unclass), recursive = FALSE)
	attributes(ret) = attributes(lst[[1]]) # crs
	if (all(points_attr))
		attr(ret, "points") = do.call(rbind, lapply(lst, attr, "points"))
	class(ret) = cls
	attr(ret, "bbox") = compute_bbox(ret) # dispatch on class
	attr(ret, "n_empty") = sum(sapply(lst, attr, which = "n_empty"))
	if (inherits(ret, "sfc_GEOMETRY"))
		attr(ret, "classes") = vapply(ret, class, rep("", 3))[2L,]
	ret
}

#' @export
print.sfc = function(x, ..., n = 5L, what = "Geometry set for", append = "") {
	sep = if (length(x) != 1) "s" else ""
	cls = substr(class(x)[1], 5, nchar(class(x)[1]))
	cat(paste0(what, " ", length(x), " feature", sep, " ", append))
	if (! is.null(attr(x, "n_empty"))) {
		ne = attr(x, "n_empty")
		if (ne > 0)
			cat(paste0(" (with ", ne, ifelse(ne > 1, " geometries ", " geometry "), "empty)"))
	}
	cat("\n")
	if (length(x)) {
		cat(paste0("Geometry type: ", cls, "\n"))
		u = sort(unique(sapply(x, function(x) class(x)[1])))
		cat(paste0("Dimension:     ", paste(u, collapse = ", "), "\n"))
	}
	cat(    paste0("Bounding box:  "))
	bb = signif(attr(x, "bbox"), options("digits")$digits)
	cat(paste(paste(names(bb), bb[], sep = ": "), collapse = " "))
	cat("\n")
	if( !is.null( attr(x, "z_range"))) {
		cat(paste0("z_range:       "))
		zb = signif(attr(x, "z_range"), options("digits")$digits)
		cat(paste(paste(names(zb), zb[], sep = ": "), collapse = " "))
		cat("\n")
	}
	if( !is.null( attr(x, "m_range"))) {
		cat(paste0("m_range:       "))
		mb = signif(attr(x, "m_range"), options("digits")$digits)
		cat(paste(paste(names(mb), mb[], sep = ": "), collapse = " "))
		cat("\n")
	}
	# attributes: epsg, proj4string, precision
	crs = st_crs(x)
	if (is.na(crs))
		cat(paste0("CRS:           NA\n"))
	else {
		p = crs_parameters(crs)
		if (p$Name == "unknown") {
			if (is.character(crs$input) && !is.na(crs$input) && crs$input != "unknown")
				p$Name = crs$input
			else
				p$Name = crs$proj4string
		}
		if (p$IsGeographic)
			cat(paste0("Geodetic CRS:  ", p$Name, "\n"))
		else
			cat(paste0("Projected CRS: ", p$Name, "\n"))
	}
	if (attr(x, "precision") != 0.0) {
		cat(    paste0("Precision:     "))
		if (attr(x, "precision") < 0.0)
			cat("float (single precision)\n")
		else
			cat(paste(attr(x, "precision"), "\n"))
	} # else cat("double (default; no precision model)\n")
	if (length(x) > n && n > 0)
		cat(paste0("First ", n, " geometries:\n"))
	for (i in seq_len(min(n, length(x))))
		if (inherits(x[[i]], "sfg"))
			print(x[[i]], width = 50)
		else
			print(x[[i]])
	invisible(x)
}

#' Summarize simple feature column
#'
#' Summarize simple feature column
#' @param object object of class \code{sfc}
#' @param ... ignored
#' @param maxsum maximum number of classes to summarize the simple feature column to
#' @param maxp4s maximum number of characters to print from the PROJ string
#' @method summary sfc
#' @export
summary.sfc = function(object, ..., maxsum = 7L, maxp4s = 10L) {
	u = factor(vapply(object, function(x) WKT_name(x, FALSE), ""))
    epsg = paste0("epsg:", st_crs(object)$epsg)
	levels(u) = c(levels(u), epsg)
    p4s = attr(object, "crs")$proj4string
	if (!is.na(p4s)) {
		if (nchar(p4s) > maxp4s)
			p4s = paste0(substr(p4s, 1L, maxp4s), "...")
		levels(u) = c(levels(u), p4s)
	}
    summary(u, maxsum = maxsum, ...)
}

#' @export
as.data.frame.sfc = function(x, ...) {
	ret = data.frame(row.names = seq_along(x))
	ret$geometry = x
	ret
}


#' @name st_geometry
#' @export
st_geometry.sfc = function(obj, ...) obj

#' Return geometry type of an object
#'
#' Return geometry type of an object, as a factor
#' @param x object of class \link{sf} or \link{sfc}
#' @param by_geometry logical; if \code{TRUE}, return geometry type of each geometry,
#' else return geometry type of the set
#' @return a factor with the geometry type of each simple feature geometry
#' in \code{x}, or that of the whole set
#' @export
st_geometry_type = function(x, by_geometry = TRUE) {
	x = st_geometry(x)
	f = if (by_geometry)
			vapply(x, function(y) class(y)[2], "")
		else
			substring(class(x)[1], 5)
	factor(f, levels =
		c("GEOMETRY",
		"POINT",
		"LINESTRING",
		"POLYGON",
		"MULTIPOINT",
		"MULTILINESTRING",
		"MULTIPOLYGON",
		"GEOMETRYCOLLECTION",
		"CIRCULARSTRING",
		"COMPOUNDCURVE",
		"CURVEPOLYGON",
		"MULTICURVE",
		"MULTISURFACE",
		"CURVE",
		"SURFACE",
		"POLYHEDRALSURFACE",
		"TIN",
		"TRIANGLE"))
}

#' Drop or add Z and/or M dimensions from feature geometries
#'
#' Drop Z and/or M dimensions from feature geometries, resetting classes appropriately
#' @param x object of class \code{sfg}, \code{sfc} or \code{sf}
#' @param ... ignored
#' @param drop logical; drop, or (FALSE) add?
#' @param what character which dimensions to drop or add
#' @details Only combinations \code{drop=TRUE}, \code{what = "ZM"}, and \code{drop=FALSE}, \code{what="Z"} are supported so far. In case \code{add=TRUE}, \code{x} should have \code{XY} geometry, and zero values are added for \code{Z}.
#' @examples
#' st_zm(st_linestring(matrix(1:32,8)))
#' x = st_sfc(st_linestring(matrix(1:32,8)), st_linestring(matrix(1:8,2)))
#' st_zm(x)
#' a = st_sf(a = 1:2, geom=x)
#' st_zm(a)
#' @export
st_zm <- function(x, ..., drop = TRUE, what = "ZM") UseMethod("st_zm")

#' @export
st_zm.sf <- function(x, ..., drop = TRUE, what = "ZM") {
	st_geometry(x) = st_zm(st_geometry(x), drop = drop, what = what)
	x
}

#' @export
st_zm.sfc <- function(x, ..., drop = TRUE, what = "ZM") {
	st_sfc(lapply(x, st_zm, drop = drop, what = what), crs = st_crs(x))
}

#' @export
st_zm.sfg <- function(x, ..., drop = TRUE, what = "ZM") {
	if (drop && what == "ZM") {
		ret = if (is.list(x))
			lapply(x, st_zm, drop = drop, what = what)
		else if (is.matrix(x))
			x[, 1:2, drop = FALSE]
		else
			x[1:2]
		structure(ret, class = c("XY", class(x)[2:3]))
	} else if (!drop && what == "Z") {
		if (class(x)[1] != "XY")
			stop("adding Z only supported for XY geometries")
		ret = if (is.list(x))
			lapply(x, st_zm, drop = drop, what = what)
		else if (is.matrix(x))
			cbind(unclass(x), 0)
		else
			c(unclass(x), 0)
		structure(ret, class = c("XYZ", class(x)[2:3]))
	} else
		stop("this combination of `x', `drop' and `what' is not implemented")
}

#' @export
st_zm.list <- function(x, ..., drop = TRUE, what = "ZM")
	lapply(x, st_zm, drop = drop, what = what)

#' @export
st_zm.matrix <- function(x, ..., drop = TRUE, what = "ZM")  {
	if (drop && what == "ZM") {
		x[,1:2]
	} else if (!drop && what == "Z") {
		cbind(unclass(x), 0)
	} else
		stop("this combination of drop and what is not implemented")
}

#' Get precision
#'
#' @param x object of class \code{sfc} or \code{sf}
#' @export
st_precision <- function(x) {
  UseMethod("st_precision")
}

#' @export
st_precision.sf <- function(x) {
  x <- st_geometry(x)
  st_precision(x)
}

#' @export
st_precision.sfc <- function(x) {
  attr(x, "precision")
}

#' Set precision
#'
#' @name st_precision
#' @param precision numeric, or object of class \code{units} with distance units (but see details); see \link{st_as_binary} for how to do this.
#' @details If \code{precision} is a \code{units} object, the object on which we set precision must have a coordinate reference system with compatible distance units.
#'
#' Setting a \code{precision} has no direct effect on coordinates of geometries, but merely set an attribute tag to an \code{sfc} object. The effect takes place in \link{st_as_binary} or, more precise, in the C++ function \code{CPL_write_wkb}, where simple feature geometries are being serialized to well-known-binary (WKB). This happens always when routines are called in GEOS library (geometrical operations or predicates), for writing geometries using \link{st_write} or \link{write_sf}, \code{st_make_valid} in package \code{lwgeom}; also \link{aggregate} and \link{summarise} by default union geometries, which calls a GEOS library function. Routines in these libraries receive rounded coordinates, and possibly return results based on them. \link{st_as_binary} contains an example of a roundtrip of \code{sfc} geometries through WKB, in order to see the rounding happening to R data.
#'
#' The reason to support precision is that geometrical operations in GEOS or liblwgeom may work better at reduced precision. For writing data from R to external resources it is harder to think of a good reason to limiting precision.
#'
#' @seealso \link{st_as_binary} for an explanation of what setting precision does, and the examples therein.
#' @examples
#' x <- st_sfc(st_point(c(pi, pi)))
#' st_precision(x)
#' st_precision(x) <- 0.01
#' st_precision(x)
#' @export
st_set_precision <- function(x, precision) {
    UseMethod("st_set_precision")
}

#' @export
st_set_precision.sfc <- function(x, precision) {
    if (length(precision) != 1) {
        stop("Precision applies to all dimensions and must be of length 1.", call. = FALSE)
    }

	if (inherits(precision, "units")) {
		u = st_crs(x)$ud_unit
		if (is.null(u))
			stop("cannot use precision expressed as units when target object has no units (CRS) set")
		units(precision) = 1/u # convert
		precision = as.numeric(precision)
	}

    if (is.na(precision) || !is.numeric(precision)) {
        stop("Precision must be numeric", call. = FALSE)
    }
    structure(x, precision = precision)
}

#' @export
st_set_precision.sf <- function(x, precision) {
    st_geometry(x) <- st_set_precision(st_geometry(x), precision)
    return(x)
}

#' @name st_precision
#' @param value precision value
#' @export
"st_precision<-" <- function(x, value) {
    st_set_precision(x, value)
}

typed_empty = function(cls, ncol = 2, dim = "XY") {
	switch(cls[1],
		sfc_POINT = st_point(rep(NA_real_, ncol), dim = dim),
		sfc_MULTIPOINT = st_multipoint(matrix(numeric(0), ncol = ncol), dim = dim),
		sfc_LINESTRING = st_linestring(matrix(numeric(0), ncol = ncol), dim = dim),
		sfc_MULTILINESTRING = st_multilinestring(dim = dim),
		sfc_POLYGON = st_polygon(dim = dim),
		sfc_MULTIPOLYGON = st_multipolygon(dim = dim),
		st_geometrycollection(dims = dim))
}

#' retrieve coordinates in matrix form
#'
#' retrieve coordinates in matrix form
#' @param x object of class sf, sfc or sfg
#' @param ... ignored
#' @return matrix with coordinates (X, Y, possibly Z and/or M) in rows, possibly followed by integer indicators \code{L1},...,\code{L3} that point out to which structure the coordinate belongs; for \code{POINT} this is absent (each coordinate is a feature), for \code{LINESTRING} \code{L1} refers to the feature, for \code{MULTILINESTRING} \code{L1} refers to the part and \code{L2} to the simple feature, for \code{POLYGON} \code{L1} refers to the main ring or holes and \code{L2} to the simple feature, for \code{MULTIPOLYGON} \code{L1} refers to the main ring or holes, \code{L2} to the ring id in the \code{MULTIPOLYGON}, and \code{L3} to the simple feature.
#' 
#' For \code{POLYGONS}, \code{L1} can be used to identify exterior rings and inner holes. 
#' The exterior ring is when \code{L1} is equal to 1. Interior rings are identified 
#' when \code{L1} is greater than 1. \code{L2} can be used to differentiate between the 
#' feature. Whereas for \code{MULTIPOLYGON}, \code{L3} refers to the \code{MULTIPOLYGON}
#' feature and \code{L2} refers to the component \code{POLYGON}.
#' 
#' @export
st_coordinates = function(x, ...) UseMethod("st_coordinates")

#' @export
st_coordinates.sf = function(x, ...) st_coordinates(st_geometry(x))

#' @export
st_coordinates.sfg = function(x, ...) st_coordinates(st_geometry(x))

#' @export
st_coordinates.sfc = function(x, ...) {
	if (length(x) == 0)
		return(matrix(nrow = 0, ncol = 2))

	ret = switch(class(x)[1],
		sfc_POINT = if (is.null(attr(x, "points"))) {
					matrix(unlist(x, use.names = FALSE), nrow = length(x), byrow = TRUE, dimnames = NULL)
				} else {
					attr(x, "points")
				},
		sfc_MULTIPOINT = ,
		sfc_LINESTRING = coord_2(x),
		sfc_MULTILINESTRING = ,
		sfc_POLYGON = coord_3(x),
		sfc_MULTIPOLYGON = coord_4(x),
		stop(paste("not implemented for objects of class", class(x)[1]))
	)
	Dims = if (!is.null(attr(x, "points_dim")))
			attr(x, "points_dim")
		else
			class(x[[1]])[1]
	ncd = nchar(Dims)
	colnames(ret)[1:ncd] = vapply(seq_len(ncd), function(i) substr(Dims, i, i), "")
	ret
}

coord_2 = function(x) { # x is a list with matrices
	cbind(do.call(rbind, x), L1 = rep(seq_along(x), times = vapply(x, nrow, 0L)))
}

coord_3 = function(x) { # x is a list of lists with matrices
	x = lapply(x, coord_2)
	cbind(do.call(rbind, x), L2 = rep(seq_along(x), times = vapply(x, nrow, 0L)))
}

coord_4 = function(x) { # x is a list of lists of lists with matrices
	x = lapply(x, coord_3)
	cbind(do.call(rbind, x), L3 = rep(seq_along(x), times = vapply(x, nrow, 0L)))
}

#' @export
rep.sfc = function(x, ...) {
	st_sfc(NextMethod(), crs = st_crs(x))
}

check_ring_dir = function(x) {
	check_polygon = function(pol) {
		sa = sapply(pol, CPL_signed_area)
		revert = if (length(sa))
				c(sa[1] < 0, sa[-1] > 0)
			else
				logical(0)
		pol[revert] = lapply(pol[revert], function(m) m[nrow(m):1,])
		pol
	}
	cls = if (inherits(x, "sfg"))
			class(x)[2]
		else
			class(x)[1]
	ret = switch(cls,
		POLYGON = check_polygon(x),
		MULTIPOLYGON = ,
		sfc_POLYGON = lapply(x, check_polygon),
		sfc_MULTIPOLYGON = lapply(x, function(y) structure(lapply(y, check_polygon), class = class(y))),
		stop(paste("check_ring_dir: not supported for class", class(x)[1]))
	)
	attributes(ret) = attributes(x)
	ret
}

#' @name st_as_sfc
#' @export
st_as_sfc.list = function(x, ..., crs = NA_crs_) {

	if (length(x) == 0)
		return(st_sfc(crs = crs))

	if (is.raw(x[[1]]))
		st_as_sfc.WKB(structure(x, class = "WKB"), ..., crs = crs)
	else if (inherits(x[[1]], "sfg"))
		st_sfc(x, crs = crs)
	else if (is.character(x[[1]])) { # hex wkb or wkt:
		ch12 = substr(x[[1]], 1, 2)
		if (ch12 == "0x" || ch12 == "00" || ch12 == "01") # hex wkb
			st_as_sfc.WKB(structure(x, class = "WKB"), ..., crs = crs)
		else
			st_as_sfc(unlist(x), ..., crs = crs) # wkt
	} else
		stop(paste("st_as_sfc.list: don't know what to do with list with elements of class", class(x[[1]])))
}

#' @name st_as_sfc
#' @export
st_as_sfc.blob = function(x, ...) {
	st_as_sfc.list(x, ...)
}

#' @name st_as_sfc
#' @export
st_as_sfc.bbox = function(x, ...) {
	box = st_polygon(list(matrix(x[c(1, 2, 3, 2, 3, 4, 1, 4, 1, 2)], ncol = 2, byrow = TRUE)))
	st_sfc(box, crs = st_crs(x))
}

#' @export
`[[.sfc` = function(x, i, j, ..., exaxt = TRUE) {
	if (inherits(x, "sfc_POINT") && !is.null(attr(x, "points")))
		restore_point(x, i)
	else
		NextMethod()
}

restore_point = function(x, i = TRUE) {
	restore_points(x, i)[[1]]
}

restore_points = function(x, i = TRUE) {
	a = attributes(x)
	points = a$points[i, , drop=FALSE]
	structure(points_rcpp(points, a$points_dim),
		n_empty = 0L, precision = a$precision, crs = a$crs,
		bbox = bbox.pointmatrix(points), class = a$class, 
		points = NULL, points_dim = NULL)
}
