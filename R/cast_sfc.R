#' Cast geometry to another type: either simplify, or cast explicitly
#'
#' Cast geometry to another type: either simplify, or cast explicitly
#'
#' @param x object of class \code{sfg}, \code{sfc} or \code{sf}
#' @param to character; target type, if missing, simplification is tried; when \code{x} is of type \code{sfg} (i.e., a single geometry) then \code{to} needs to be specified.
#' @param ... ignored
#' @return object of class \code{to} if successful, or unmodified object if unsuccesful. If information gets lost while type casting, a warning is raised.
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
		GEOMETRYCOLLECTION = 4,
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
			rbind(m, m[1,])
		else
			m
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
		sfc_MULTIPOINT = sapply(x, nrow),
		sfc_LINESTRING = sapply(x, nrow),
		sapply(x, length) # list
	)
}

#' Coerce geometry to MULTI* geometry
#' 
#' mixes of POINTS and MULTIPOINTS, LINESTRING and MULTILINESTRING, 
#' POLYGON and MULTIPOLYGON are returned as MULTIPOINTS, MULTILINESTRING and MULTIPOLYGONS respectively
#' @param x list of geometries or simple features
#' @details Geometries that are already MULTI* are left unchanged. 
#' Features that can't be cast to a single  MULTI* geometry are return as a 
#' GEOMETRYCOLLECTION
st_cast_sfc_default = function(x) {
  if (!identical(unique(sapply(x, function(w) class(w)[3L])), "sfg"))
    stop("list item(s) not of class sfg") # sanity check
  
  a <- attributes(x)
  cls = unique(sapply(x, function(x) class(x)[2L]))
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
  } else if (cls == "GEOMETRYCOLLECTION") {
    x <- structure(do.call(st_sfc, unlist(x, recursive = FALSE)), ids = get_lengths(x))
    class(x) <- c("sfc_GEOMETRY", "sfc") 
  }
  attributes(x) <- a
  st_sfc(x)
}

#' @name st_cast
#' @param ids integer vector, denoting how geometries should be grouped (default: no grouping)
#' @export
st_cast.sfc = function(x, to, ..., ids = seq_along(x)) {
	if (missing(to)) {
		stopifnot(missing(ids))
		return(st_cast_sfc_default(x))
	}

	from_cls = substr(class(x)[1], 5, 100)
	from_col = which_sfc_col(from_cls)
	to_col = which_sfc_col(to)
	if (from_cls == to)
		x # returns x: do nothing
	else if (from_cls == "GEOMETRY")
		st_sfc(lapply(x, st_cast, to = to), crs = st_crs(x))
	else if (from_col == to_col) # "vertical" conversion: only reclass, possibly close polygons
		reclass(x, to, need_close(to))
	else if (abs(from_col - to_col) > 1) {
		if (to == "POINT")
			st_cast(st_cast(x, "MULTIPOINT"), "POINT")
		else if (to %in% c("MULTIPOINT", "LINESTRING")) {
			ret = lapply(x, function(y) structure(unlist(y), class = c(class(y)[1], to, "sfg")))
			attributes(ret) = attributes(x)
			reclass(ret, to, FALSE)
		} else
			stop("use smaller steps for st_cast, or first convert to MULTIPOINT or LINESTRING")
	} else if (from_col < to_col) { # "horizontal", to the right: group
		ret = if (from_col == 0)
			lapply(unname(split(x, ids)), function(y) structure(do.call(rbind, y), class = class(x[[1]])))
		else
			lapply(unname(split(x, ids)), function(y) structure(y, class = class(x[[1]])))
		attributes(ret) = attributes(x)
		reclass(ret, to, need_close(to))
	} else { # "horizontal", to the left: split
		ret = if (from_col == 1)
				unlist(lapply(x, function(m) lapply(seq_len(nrow(m)), 
						function(i) structure(m[i,], class = class(m)))),
					recursive = FALSE)
			else 
				lapply(do.call(c, x), function(y) structure(y, class = class(x[[1]])))
		attributes(ret) = attributes(x)
		structure(reclass(ret, to, need_close(to)), ids = get_lengths(x))
	}
}

#' @name st_cast
#' @param FUN function passed on to \link[stats]{aggregate}, in case \code{ids} was specified and attributes need to be grouped
#' @param warn logical; if \code{TRUE}, warn if attributes are assigned to sub-geometries
#' @export
st_cast.sf = function(x, to, ..., ids = seq_len(nrow(x)), FUN, warn = TRUE) {
	geom = st_cast(st_geometry(x), to, ids = ids)
	crs = st_crs(x)
	st_geometry(x) = NULL
	#x = as.data.frame(x)
	if (!is.null(attr(geom, "ids"))) {
		if (!missing(ids))
			warning("argument ids is ignored, and taken from the geometry splitting")
		if (warn && !all_constant(x))
			warning("repeating attributes for all sub-geometries for which they may not be constant")
		ids = attr(geom, "ids")          # e.g. 3 2 4
		reps = rep(seq_len(length(ids)), ids) # 1 1 1 2 2 3 3 3 3 etc
		# FIXME: deal with identity -> constant
		st_sf(x[reps, ], geom, crs = crs)
	} else { 
		# FIXME: warn on const -> aggregation; carry out area-weighted aggregation?
		if (length(unique(ids)) < nrow(x)) {
			if (missing(FUN))
				stop("aggregation function missing; pls specify argument FUN")
			x = aggregate(x, list(ids.group = ids), FUN, simplify = FALSE)
		}
		st_sf(x, geom, crs = crs)
	}
}
