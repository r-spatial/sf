

#' Cast geometry to another type: either simplify, or cast explicitly
#'
#' Cast geometry to another type: either simplify, or cast explicitly
#'
#' @param x object of class \code{sfg}, \code{sfc} or \code{sf}
#' @param to character; target type, if missing, simplification is tried; when \code{x} is of type \code{sfg} (i.e., a single geometry) then \code{to} needs to be specified.
#' @param ... ignored
#' @returns object of class \code{to} if successful, or unmodified object if unsuccesful. If information gets lost while type casting, a warning is raised.
#' @details 
#' @examples
#' s = st_multipoint(rbind(c(1,0)))
#' st_cast(s, "POINT")
#' @export
st_cast <- function(x, to, ...) UseMethod("st_cast")

#' @export
st_cast.sfc <- function(x, to, ...) {
	g = x
	gtp = substr(class(g)[1], 5, 100)
	tp = st_geometry_type(g)
	utp = unique(tp)
	if (missing(to)) {
		if (length(utp) == 1) {
			if (utp != gtp)
				structure(g, class = c(paste0("sfc_", utp), "sfc"))
			else if (utp == "GEOMETRYCOLLECTION")
				un_gc(g)
			else 
				g
		} else 
			g
	} else {
		sel = which(tp != to)
		if (length(sel) == 0)
			g
		else {
			g[sel] = lapply(g[sel], function(y) st_cast(y, to, ...))
			structure(g, class = c(paste0("sfc_", to), "sfc"))
		}
	}
}

#' @export
st_cast.sf <- function(x, to, ...) {
	g = st_cast(st_geometry(x), to, ...)
	x[[attr(x, "sf_column")]] = g
	x
}

#' @export
st_cast.sfg <- function(x, to, ...) {
	if (missing(to))
		stop("to argument is needed to cast single geometry")
	lengthOne = function(x) { 
		if (is.list(x))
			length(x) == 1
		else if (is.matrix(x))
			nrow(x) == 1
		else # POINT
			TRUE
	}
	chkOne = function(x)
		if (!lengthOne(x))
			warning("object does not have length one: casting causes information loss")
	if (class(x)[2] == "GEOMETRYCOLLECTION") {
		chkOne(x)
		st_cast(x[[1]], to, ...)
	} else { 
		switch(to,
		POINT = { chkOne(x); st_point(x[1,]) },
		LINESTRING = { chkOne(x); st_linestring(x[[1]]) },
		POLYGON = { chkOne(x); st_polygon(x[[1]]) },
		MULTIPOINT = st_multipoint(rbind(unclass(x))),
		MULTILINESTRING = st_multilinestring(list(unclass(x))),
		MULTIPOLYGON = st_multipolygon(list(unclass(x))),
		GEOMETRYCOLLECTION = st_geometrycollection(list(x)),
		stop(paste("cannot cast object of class", class(x)[2], "into", to)))
	}
}

un_gc <- function(x) {
	l = sapply(x, length)
	if (all(l == 1))
		do.call(st_sfc, lapply(x, function(y) y[[1]]))
	else
		x
}
