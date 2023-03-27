#' Cast geometry to another type: either simplify, or cast explicitly
#'
#' Cast geometry to another type: either simplify, or cast explicitly
#'
#' @param x object of class \code{sfg}, \code{sfc} or \code{sf}
#' @param to character; target type, if missing, simplification is tried; when \code{x} is of type \code{sfg} (i.e., a single geometry) then \code{to} needs to be specified.
#' @return object of class \code{to} if successful, or unmodified object if unsuccessful. If information gets lost while type casting, a warning is raised.
#' @examples
#' s = st_multipoint(rbind(c(1,0)))
#' st_cast(s, "POINT")
#' @export
st_cast <- function(x, to, ...) UseMethod("st_cast")

# see this figure:
# https://cloud.githubusercontent.com/assets/520851/21387553/5f1edcaa-c778-11e6-92d0-2d735e4c8e40.png
# columns start counting at 0:

which_sfc_col = function(cls) {
	stopifnot(!missing(cls))
	switch(cls,
		POINT = 0,
		LINESTRING = 1,
		MULTIPOINT = 1,
		MULTILINESTRING = 2,
		POLYGON = 2,
		MULTIPOLYGON = 3,
		MULTICURVE = 3,
		GEOMETRYCOLLECTION = 4,
		COMPOUNDCURVE = 4,
		MULTISURFACE = 4,
		CURVEPOLYGON = 4,
		GEOMETRY = 5,
		stop(paste("st_cast for", cls, "not supported"))
	)
}

# does this geometry type need closed polygons?
need_close = function(cls) {
	switch(cls,
		POLYGON = TRUE,
		MULTIPOLYGON = TRUE,
		FALSE
	)
}

# add y's attributes to x, return x
add_attributes = function(x, y) {
	attributes(x) = attributes(y)
	x
}

close_polygon_or_multipolygon = function(x, to) {
	to_col = which_sfc_col(to)
	close_mat = function(m) {
		if (any(m[1,] != m[nrow(m),]))
			m = rbind(m, m[1,])
		if (nrow(m) < 4)
			stop("polygons require at least 4 points")
		unclass(m)
	}
	add_attributes(
		if (to_col == 2)
			lapply(x, function(y) add_attributes(lapply(y, close_mat), y))
		else if (to_col == 3)
			lapply(x, function(y) add_attributes(lapply(y, function(z) lapply(z, close_mat)), y))
		else
			stop("invalid to_col value")
	, x)
}

# change the class of sfc x, and all its sfg list elements
# (vertical cast)
reclass = function(x, to, must_close) {
	l = if (length(x)) {
		full_cls = c(class(x[[1]])[1], to, "sfg")
		if (must_close)
			x = close_polygon_or_multipolygon(x, to)
		lapply(x, function(g) structure(g, class = full_cls))
	} else
		list()
	attributes(l) = attributes(x)
	structure(l, class = c(paste0("sfc_", to), "sfc"))
}

# how long is each geometry in the sfc?
get_lengths = function(x) {
	switch(class(x)[1],
		sfc_POINT = rep(1, length(x)),
		sfc_MULTIPOINT = vapply(x, nrow, 0L),
		sfc_LINESTRING = vapply(x, nrow, 0L),
		lengths(x) # list
	)
}

#' Coerce geometry to MULTI* geometry
#'
#' Mixes of POINTS and MULTIPOINTS, LINESTRING and MULTILINESTRING,
#' POLYGON and MULTIPOLYGON are returned as MULTIPOINTS, MULTILINESTRING and MULTIPOLYGONS respectively
#' @param x list of geometries or simple features
#' @details Geometries that are already MULTI* are left unchanged.
#' Features that can't be cast to a single  MULTI* geometry are return as a
#' GEOMETRYCOLLECTION
st_cast_sfc_default = function(x) {

	if (length(x) == 0)
		return(x)

	if (!identical(unique(vapply(x, function(w) class(w)[3L], "")), "sfg"))
		stop("list item(s) not of class sfg") # sanity check

	a <- attributes(x)
	ids = NULL
	cls = unique(vapply(x, function(x) class(x)[2L], ""))
	if (length(cls) > 1) {
		if (all(cls %in% c("POINT", "MULTIPOINT"))) {
			x <- lapply(x, function(x) if (inherits(x, "POINT")) POINT2MULTIPOINT(x) else x)
			class(x) <- c("sfc_MULTIPOINT", "sfc")
		} else if (all(cls %in% c("LINESTRING", "MULTILINESTRING"))) {
			x <- lapply(x, function(x) if (inherits(x, "LINESTRING")) LINESTRING2MULTILINESTRING(x) else x)
			class(x) <- c("sfc_MULTILINESTRING", "sfc")
		} else if (all(cls %in% c("POLYGON", "MULTIPOLYGON"))) {
			x <- lapply(x, function(x) if (inherits(x, "POLYGON")) POLYGON2MULTIPOLYGON(x) else x)
			class(x) <- c("sfc_MULTIPOLYGON", "sfc")
		}
	} else if (cls == "GEOMETRYCOLLECTION" && all(lengths(x))) { # https://github.com/r-spatial/sf/issues/1767
		ids = get_lengths(x)
		x <- do.call(st_sfc, unlist(x, recursive = FALSE))
	}
	attributes(x) <- a
	structure(st_sfc(x), ids = ids)
}

copy_sfc_attributes_from = function(x, ret) {
	structure(ret, precision = attr(x, "precision"),
		bbox = attr(x, "bbox"), crs = attr(x, "crs"), n_empty = attr(x, "n_empty"))
}

empty_sfg <- function(to) {
	switch(to,
		   GEOMETRYCOLLECTION = st_geometrycollection(),
		   MULTIPOLYGON = st_multipolygon(),
		   POLYGON = st_polygon(),
		   MULTILINESTRING = st_multilinestring(),
		   LINESTRING = st_linestring(),
		   MULTIPOINT = st_multipoint(),
		   POINT = st_point()
	   )
}


#' @name st_cast
#' @param ids integer vector, denoting how geometries should be grouped (default: no grouping)
#' @param group_or_split logical; if TRUE, group or split geometries; if FALSE, carry out a 1-1 per-geometry conversion.
#' @param ... ignored
#' @export
#' @return In case \code{to} is missing, \code{st_cast.sfc} will coerce combinations of "POINT" and "MULTIPOINT", "LINESTRING" and "MULTILINESTRING", "POLYGON" and "MULTIPOLYGON" into their "MULTI..." form, or in case all geometries are "GEOMETRYCOLLECTION" will return a list of all the contents of the "GEOMETRYCOLLECTION" objects, or else do nothing. In case \code{to} is specified, if \code{to} is "GEOMETRY", geometries are not converted, else, \code{st_cast} will try to coerce all elements into \code{to}; \code{ids} may be specified to group e.g. "POINT" objects into a "MULTIPOINT", if not specified no grouping takes place. If e.g. a "sfc_MULTIPOINT" is cast to a "sfc_POINT", the objects are split, so no information gets lost, unless \code{group_or_split} is \code{FALSE}.
#' @details When converting a GEOMETRYCOLLECTION to COMPOUNDCURVE, MULTISURFACE or CURVEPOLYGON, the user is responsible for the validity of the resulting object: no checks are being carried out by the software.
#' 
#' When converting mixed, GEOMETRY sets, it may help to first convert to the MULTI-type, see examples
#' @examples
#' # https://github.com/r-spatial/sf/issues/1930:
#' pt1 <- st_point(c(0,1))
#' pt23 <- st_multipoint(matrix(c(1,2,3,4), ncol = 2, byrow = TRUE))
#' d <- st_sf(geom = st_sfc(pt1, pt23))
#' st_cast(d, "POINT") # will not convert the entire MULTIPOINT, and warns
#' st_cast(d, "MULTIPOINT") %>% st_cast("POINT")
st_cast.sfc = function(x, to, ..., ids = seq_along(x), group_or_split = TRUE) {
	if (missing(to))
		return(st_cast_sfc_default(x))

	e = rep(FALSE, length(x))
	if (!inherits(x, c("sfc_MULTICURVE", "sfc_COMPOUNDCURVE", "sfc_CURVEPOLYGON"))) { # for which GEOS has no st_is_empty()
		e = st_is_empty(x)
		if (all(e)) {
			x[e] = empty_sfg(to)
			return(x) # RETURNS
		}
	}
	if (any(e))
		x = x[!e]
	from_cls = substr(class(x)[1], 5, 100)
	from_col = which_sfc_col(from_cls)
	to_col = which_sfc_col(to)
	ret = if (from_cls == to)
		x # returns x: do nothing
	else if (to == "GEOMETRY") # we can always do that:
		structure(x, class = c("sfc_GEOMETRY", "sfc"))
	else if (from_cls == "GEOMETRY" || !group_or_split)
		st_sfc(lapply(x, st_cast, to = to), crs = st_crs(x), precision = st_precision(x))
	else if (from_col == to_col) # "vertical" conversion: only reclass, possibly close polygons
		reclass(x, to, need_close(to))
	else if (abs(from_col - to_col) > 1) {
		if (to == "POINT")
			st_cast(st_cast(x, "MULTIPOINT"), "POINT")
		else if (to == "MULTIPOINT") {
			ret = lapply(x, function(y) structure(as.matrix(y), class = c(class(y)[1], to, "sfg")))
			ret = copy_sfc_attributes_from(x, ret)
			reclass(ret, to, FALSE)
		} else
			#st_cast(st_cast(x, "MULTILINESTRING"), to)
			stop("use smaller steps for st_cast; first cast to MULTILINESTRING or POLYGON?")
	} else if (from_col < to_col) { # "horizontal", to the right: group
		ret = if (from_col == 0)
				lapply(unname(split(x, ids)), function(y) structure(do.call(rbind, y), 
					class = class(x[[1]])))
			else
				lapply(unname(split(x, ids)), function(y) structure(y, class = class(x[[1]])))
		ret = copy_sfc_attributes_from(x, ret)
		reclass(ret, to, need_close(to))
	} else if (from_col == 3 && to == "MULTILINESTRING") {
		if (from_cls == "MULTICURVE") {
			ret = lapply(x, st_cast, to = "MULTILINESTRING")
		} else {
			ret = lapply(x, unlist, recursive = FALSE) # unlist one level deeper; one MULTIPOLYGON -> one MULTILINESTRING
			if (length(ret))
				class(ret[[1]]) = class(x[[1]]) # got dropped
		}
		ret = copy_sfc_attributes_from(x, ret)
		structure(reclass(ret, to, FALSE))
	} else { # "horizontal", to the left: split
		ret = if (from_col == 1) # LINESTRING or MULTIPOINT to POINT
				unlist(lapply(x, function(m) lapply(seq_len(nrow(m)), function(i) m[i,])), recursive = FALSE)
			else {
				if (to_col == 0 && from_cls == "POLYGON") # POLYGON -> POINT
					lapply(x, function(y) do.call(rbind, y))
				else
					unlist(x, recursive = FALSE)
			}
		ret = lapply(ret, function(y) structure(y, class = class(x[[1]]))) # will be reset by reclass()
		ret = copy_sfc_attributes_from(x, ret)
		# EJP: FIXME:
		structure(reclass(ret, to, need_close(to)), ids = get_lengths(x))
	}
	if (any(e)) {
		crs = st_crs(x)
		x = vector("list", length = length(e))
		x[e] = list(empty_sfg(to))
		x[!e] = ret
		st_set_crs(do.call(st_sfc, x), crs)
	} else
		ret
}

#' @name st_cast
#' @param warn logical; if \code{TRUE}, warn if attributes are assigned to sub-geometries
#' @param do_split logical; if \code{TRUE}, allow splitting of geometries in sub-geometries
#' @export
#' @details the \code{st_cast} method for \code{sf} objects can only split geometries, e.g. cast \code{MULTIPOINT} into multiple \code{POINT} features.  In case of splitting, attributes are repeated and a warning is issued when non-constant attributes are assigned to sub-geometries. To merge feature geometries and attribute values, use \link[sf:aggregate.sf]{aggregate} or \link[sf:tidyverse]{summarise}.
st_cast.sf = function(x, to, ..., warn = TRUE, do_split = TRUE) {
	geom = st_cast(st_geometry(x), to, group_or_split = do_split)
	crs = st_crs(x)
	agr = st_agr(x)
	all_const = all_constant(x)
	sf_column = attr(x, "sf_column") # keep name
	st_geometry(x) = NULL
	# class(x) = setdiff(class(x), "sf")
	ids = attr(geom, "ids")          # e.g. 3 2 4
	if (!is.null(ids)) { # split:
		if (warn && ! all_const)
			warning("repeating attributes for all sub-geometries for which they may not be constant")
		reps = rep(seq_len(length(ids)), ids) # 1 1 1 2 2 3 3 3 3 etc
		agr[agr == "identity"] = "constant" # since we splitted
		x = x[reps,, drop = FALSE]
		stopifnot(nrow(x) == length(geom))
	}
	attr(geom, "ids") = NULL # remove
	x[[sf_column]] = geom
	st_geometry(x) = sf_column
	st_agr(x) = agr
	x
}

#' @name st_cast
#' @export
st_cast.sfc_CIRCULARSTRING <- function(x, to, ...) {
	if (isTRUE(st_is_longlat(x)))
		message_longlat("st_cast")
	stopifnot(to == "LINESTRING")
	st_sfc(CPL_circularstring_to_linestring(st_sfc(x)), crs = st_crs(x)) # should add attributes?
}

#' test equality between the geometry type and a class or set of classes
#'
#' test equality between the geometry type and a class or set of classes
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param type character; class, or set of classes, to test against
#' @examples
#' st_is(st_point(0:1), "POINT")
#' sfc = st_sfc(st_point(0:1), st_linestring(matrix(1:6,,2)))
#' st_is(sfc, "POINT")
#' st_is(sfc, "POLYGON")
#' st_is(sfc, "LINESTRING")
#' st_is(st_sf(a = 1:2, sfc), "LINESTRING")
#' st_is(sfc, c("POINT", "LINESTRING"))
#' @export
st_is = function(x, type) UseMethod("st_is")

#' @export
st_is.sf = function(x, type)
	st_is(st_geometry(x), type)

#' @export
st_is.sfc = function(x, type)
	vapply(x, inherits, type, FUN.VALUE = logical(1))

#' @export
st_is.sfg = function(x, type)
	inherits(x, type)
