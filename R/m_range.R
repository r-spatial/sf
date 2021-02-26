
#' @name st_m_range
#' @param x object of class \code{m_range}
#' @export
is.na.m_range = function(x) identical(x, NA_m_range_)

mb_wrap = function(mb) {
	stopifnot(is.numeric(mb) && length(mb) == 2)
	structure(mb, names = c("mmin", "mmax"), class = "m_range")
}

m_range.Set = function(obj, ...) {
	sel = vapply(obj, function(x) { length(x) && !all(is.na(x)) }, TRUE)
	if (! any(sel))
		NA_m_range_
	else
		mb_wrap(CPL_get_m_range(unclass(obj)[sel], 0))
}
m_range.Mtrx = function(obj, ...) {
	if (length(obj) == 0)
		NA_m_range_
	else
		mb_wrap(CPL_get_m_range(list(obj), 1)) # note the list()
}
m_range.MtrxSet = function(obj, ...) {
	if (length(obj) == 0)
		NA_m_range_
	else
		mb_wrap(CPL_get_m_range(obj, 1))
}
m_range.MtrxSetSet = function(obj, ...) {
	if (length(obj) == 0)
		NA_m_range_
	else
		mb_wrap(CPL_get_m_range(obj, 2))
}
m_range.MtrxSetSetSet = function(obj, ...) {
	if (length(obj) == 0)
		NA_m_range_
	else
		mb_wrap(CPL_get_m_range(obj, 3))
}

#' Return 'm' range of a simple feature or simple feature set
#'
#' Return 'm' range of a simple feature or simple feature set
#' @param obj object to compute the m range from
#' @param ... ignored
#' @export
#' @return a numeric vector of length two, with \code{mmin} and \code{mmax} values;
#' if \code{obj} is of class \code{sf} or \code{sfc} the object
#' if \code{obj} is of class \code{sf} or \code{sfc} the object
#' returned has a class \code{m_range}
#' @name st_m_range
#' @examples
#' a = st_sf(a = 1:2, geom = st_sfc(st_point(0:3), st_point(1:4)), crs = 4326)
#' st_m_range(a)
st_m_range = function(obj, ...) UseMethod("st_m_range")

#' @export
#' @name st_m_range
st_m_range.POINT = function(obj, ...) mb_wrap(if (inherits(obj, "XYM")) c(obj[3L], obj[3L]) else c(obj[4L],obj[4L]))
#' @export
#' @name st_m_range
st_m_range.MULTIPOINT = m_range.Mtrx
#' @export
#' @name st_m_range
st_m_range.LINESTRING = m_range.Mtrx
#' @export
#' @name st_m_range
st_m_range.POLYGON = m_range.MtrxSet
#' @export
#' @name st_m_range
st_m_range.MULTILINESTRING = m_range.MtrxSet
#' @export
#' @name st_m_range
st_m_range.MULTIPOLYGON = m_range.MtrxSetSet

m_range_list = function(obj, ...) {
	s = vapply(obj, st_m_range, c(0.,0.)) # dispatch on class
	if (length(s) == 0 || all(is.na(s[1L,])))
		NA_m_range_
	else
		mb_wrap(c(min(s[1L,], na.rm = TRUE), max(s[2L,], na.rm = TRUE)))
}

#' @name st_m_range
#' @export
st_m_range.GEOMETRYCOLLECTION = m_range_list

#' @name st_m_range
#' @export
st_m_range.MULTISURFACE = m_range_list

#' @name st_m_range
#' @export
st_m_range.MULTICURVE = m_range_list

#' @name st_m_range
#' @export
st_m_range.CURVEPOLYGON = m_range_list

#' @name st_m_range
#' @export
st_m_range.COMPOUNDCURVE = m_range_list

#' @name st_m_range
#' @export
st_m_range.POLYHEDRALSURFACE = m_range.MtrxSetSet

#' @name st_m_range
#' @export
st_m_range.TIN = m_range.MtrxSetSet

#' @name st_m_range
#' @export
st_m_range.TRIANGLE = m_range.MtrxSet

#' @name st_m_range
#' @export
st_m_range.CIRCULARSTRING = function(obj, ...) {
	# this is of course wrong:
	st_m_range(st_cast(obj, "LINESTRING")) # nocov
}

#' @export
print.m_range = function(x, ...) {
	x = structure(x, crs = NULL, class = NULL) # nocov
	print(set_units(x, attr(x, "units"), mode = "standard")) # nocov
}

compute_m_range = function(obj) {
	switch(class(obj)[1],
		   sfc_POINT = mb_wrap(m_range.Set(obj)),
		   sfc_MULTIPOINT = mb_wrap(m_range.MtrxSet(obj)),
		   sfc_LINESTRING = mb_wrap(m_range.MtrxSet(obj)),
		   sfc_POLYGON = mb_wrap(m_range.MtrxSetSet(obj)),
		   sfc_MULTILINESTRING = mb_wrap(m_range.MtrxSetSet(obj)),
		   sfc_MULTIPOLYGON = mb_wrap(m_range.MtrxSetSetSet(obj)),
		   m_range_list(obj)
	)
}

#' @name st_m_range
#' @export
st_m_range.sfc = function(obj, ...) {
	a = attr(obj, "m_range")
	if(is.null(a)) return( NULL ) ## TODO return null?
	structure(a, crs = st_crs(obj))
}
#' @name st_m_range
#' @export
st_m_range.sf = function(obj, ...) st_m_range(st_geometry(obj))

#' @name st_m_range
#' @param crs object of class \code{crs}, or argument to \link{st_crs}, specifying the CRS of this bounding box.
#' @examples
#' st_m_range(c(mmin = 16.1, mmax = 16.6), crs = st_crs(4326))
#' @export
st_m_range.numeric = function(obj, ..., crs = NA_crs_) {
	structure(mb_wrap(obj[c("mmin", "mmax")]), crs = st_crs(crs)) # nocov
}

#' @export
st_m_range.m_range = function(obj, ...) obj # nocov


#' @export
"$.m_range" = function(x, name) { # nocov start
	switch(name,
		   mmin = x["mmin"],
		   mmax = x["mmax"],
		   stop("unsupported name")
	)
} # nocov end

#' @name st_m_range
#' @details \code{NA_m_range_} represents the missing value for a \code{m_range} object
#' @export
NA_m_range_ = structure(rep(NA_real_, 2),
					 names = c("mmin", "mmax"),
					 crs = NA_crs_,
					 class = "m_range")
