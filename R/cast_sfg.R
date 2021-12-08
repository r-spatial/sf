## utility functions, patterns that are either used elsewhere or can be 
##  replaced by other changes 
## 
## worker functions from the internals of c.sfg
##  to unclass the underlying coordinates
Paste0 <- function(lst) lapply(lst, unclass)
##
## drop the tail coordinate of a polygon ring
Tail1 <- function(lst) lapply(lst, head, -1)

ClosePol <- function(mtrx) { 
	stopifnot(is.matrix(mtrx))
	if (!all(mtrx[1,] == mtrx[nrow(mtrx),])) 
		mtrx = rbind(mtrx, mtrx[1,]) 
	if (nrow(mtrx) < 4)
		stop("polygons require at least 4 points")
	mtrx
}

## multi-polygon and polygon constructor, allow unclosed (but don't apply auto-closing)
## note use of local constructor below, not the sf-API one
#st_multipolygon_close <- function(x = list(), dim = "XYZ") {
#	MtrxSetSet(x, dim, type = "MULTIPOLYGON", needClosed = FALSE)
#}

#st_polygon_close <- function(x = list(), dim = "XYZ") {
#	MtrxSet(x, dim, type = "POLYGON", needClosed = FALSE)
#}

# TODO
# FIXME: warn on multi-part loss only if there are multiple parts
# disallow auto-polygon-closure for two-point inputs:  st_cast(st_linestring(cbind(0, 1:2)), "POLYGON")
# -> that should give an error
# check discussions, holes become lines, those lines become overlapping islands, or does polygonize auto-detect nesting
##  and assign holes to islands as sp-comments did?
# test on holes
# warnings on these individual tests as here, or on detection of loss higher up?
# geometrycollection conversions?
# check comments and warnings are consistent in each case below

#' @name st_cast
#' @export
#' @examples 
#' # example(st_read)
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' mpl <- nc$geometry[[4]]
#' #st_cast(x) ## error 'argument "to" is missing, with no default'
#' cast_all <- function(xg) {
#'   lapply(c("MULTIPOLYGON", "MULTILINESTRING", "MULTIPOINT", "POLYGON", "LINESTRING", "POINT"), 
#'       function(x) st_cast(xg, x))
#' }
#' st_sfc(cast_all(mpl))
#' ## no closing coordinates should remain for multipoint
#' any(duplicated(unclass(st_cast(mpl, "MULTIPOINT"))))  ## should be FALSE
#' ## number of duplicated coordinates in the linestrings should equal the number of polygon rings 
#' ## (... in this case, won't always be true)
#' sum(duplicated(do.call(rbind, unclass(st_cast(mpl, "MULTILINESTRING"))))
#'      ) == sum(unlist(lapply(mpl, length)))  ## should be TRUE
#' 
#' p1 <- structure(c(0, 1, 3, 2, 1, 0, 0, 0, 2, 4, 4, 0), .Dim = c(6L, 2L))
#' p2 <- structure(c(1, 1, 2, 1, 1, 2, 2, 1), .Dim = c(4L, 2L))
#' st_polygon(list(p1, p2))
st_cast.MULTIPOLYGON <- function(x, to, ...) {
	switch(to, 
		MULTIPOLYGON = x, 
		MULTILINESTRING = st_multilinestring(     unlist(Paste0(x), recursive = FALSE, use.names = FALSE)), 
		MULTIPOINT = st_multipoint(do.call(rbind, Tail1(unlist(Paste0(x), recursive = FALSE, use.names = FALSE)))), 
         ## loss, drop to first part
		POLYGON = {
		 	if (length(x) > 1)
				warning("polygon from first part only")
			st_polygon(x[[1L]])
		}, 
		LINESTRING = {warning("line from first ring only"); st_linestring(x[[1L]][[1L]])}, 
		## loss, drop to first coordinate of first ring of first part
		POINT = {warning("point from first coordinate only"); st_point(x[[1L]][[1L]][1L, , drop = TRUE])},
		GEOMETRYCOLLECTION = st_geometrycollection(list(x))
	)
}

#' @name st_cast
#' @export
#' @examples 
#' mls <- st_cast(nc$geometry[[4]], "MULTILINESTRING")
#' st_sfc(cast_all(mls))
st_cast.MULTILINESTRING <- function(x, to, ...) {
	switch(to, 
		MULTIPOLYGON = st_multipolygon(list(lapply(x, ClosePol))), 
		MULTILINESTRING = x, 
		MULTIPOINT = st_multipoint(do.call(rbind, Paste0(x))), 
		## loss, drop to first line
		#POLYGON = {warning("keeping first linestring only"); st_polygon(x[1L])}, 
		POLYGON = st_polygon(lapply(x, ClosePol)),
		LINESTRING = {
			if (length(x) > 1)
				warning("keeping first linestring only")
			st_linestring(x[[1L]])
		},
		## loss, drop to first coordinate of first line 
		POINT = {
			warning("keeping first coordinate only")
			st_point(x[[1L]][1L, , drop = TRUE])
		},
		GEOMETRYCOLLECTION = st_geometrycollection(list(x))
	)
}

#' @name st_cast
#' @export
#' @examples
#' mpt <- st_cast(nc$geometry[[4]], "MULTIPOINT")
#' st_sfc(cast_all(mpt))
st_cast.MULTIPOINT <- function(x, to, ...) {
  switch(to, 
         ## DANGER: polygon, linestring forms unlikely to be valid
         MULTIPOLYGON = st_multipolygon(list(list(ClosePol(unclass(x))))), 
         MULTILINESTRING = st_multilinestring(list(unclass(x))), 
         MULTIPOINT = x, 
         POLYGON = st_polygon(list(unclass(ClosePol(x)))), 
         LINESTRING = st_linestring(unclass(x)),
         ## loss, drop to first coordinate
         POINT = {
             if (st_is_empty(x)) {
                 row <- NA_integer_
             } else {
	             warning("point from first coordinate only")
                 row <- 1L
             }
             st_point(unclass(x)[row, , drop = TRUE])
         },
		 GEOMETRYCOLLECTION = st_geometrycollection(list(x))
  )
}

#' @name st_cast
#' @export
#' @examples
#' pl <- st_cast(nc$geometry[[4]], "POLYGON")
#' st_sfc(cast_all(pl))
st_cast.POLYGON <- function(x, to, ...) {
  switch(to, 
         MULTIPOLYGON = {
			 if (length(x))
				x = list(lapply(Paste0(x), ClosePol))
			 st_multipolygon(x)
		 },
         MULTILINESTRING = st_multilinestring(unclass(x)), 
         MULTIPOINT = st_multipoint(Tail1(unclass(x))[[1L]]), 
         POLYGON = x, 
         LINESTRING = st_linestring(unclass(x)[[1L]]),
         POINT = {warning("point from first coordinate only"); st_point(unclass(x)[[1L]][1L, , drop = TRUE])},
		 GEOMETRYCOLLECTION = st_geometrycollection(list(x))
  )
}

#' @name st_cast
#' @export
#' @examples
#' ls <- st_cast(nc$geometry[[4]], "LINESTRING")
#' st_sfc(cast_all(ls))
st_cast.LINESTRING <- function(x, to, ...) {
  switch(to, 
         MULTIPOLYGON = st_multipolygon(list(list(ClosePol(unclass(x))))), 
         MULTILINESTRING = st_multilinestring(list(unclass(x))), 
         MULTIPOINT = st_multipoint(unclass(x)), 
         POLYGON = st_polygon(list(unclass(ClosePol(x)))), 
         LINESTRING = x,
         POINT = {warning("point from first coordinate only"); st_point(unclass(x)[1L, , drop = TRUE])},
		 GEOMETRYCOLLECTION = st_geometrycollection(list(x))
  )
}

#' @name st_cast
#' @export
#' @examples
#' pt <- st_cast(nc$geometry[[4]], "POINT")
#' ## st_sfc(cast_all(pt))  ## Error: cannot create MULTIPOLYGON from POINT 
#' st_sfc(lapply(c("POINT", "MULTIPOINT"), function(x) st_cast(pt, x)))
st_cast.POINT <- function(x, to, ...) {
  switch(to, 
         MULTIPOLYGON = stop("cannot create MULTIPOLYGON from POINT"), 
         MULTILINESTRING = stop("cannot create MULTILINESTRING from POINT"), 
         MULTIPOINT = st_multipoint(matrix(unclass(x), nrow = 1L)), 
         POLYGON = stop("cannot create POLYGON from POINT"), 
         LINESTRING = stop("cannot create LINESTRING from POINT"),
         POINT = x,
		 GEOMETRYCOLLECTION = st_geometrycollection(list(x))
  )
}

#' @name st_cast
#' @export
st_cast.GEOMETRYCOLLECTION <- function(x, to, ...) {
  switch(to, 
  	GEOMETRYCOLLECTION = x,
  	{
		if (length(x) > 1)
			warning("only first part of geometrycollection is retained")
		st_cast(x[[1]], to, ...)
	}
  )
}

#' @name st_cast
#' @export
st_cast.CIRCULARSTRING <- function(x, to, ...) {
	if (to != "LINESTRING")
		stop("CIRCULARSTRING can only be converted into LINESTRING")
	CPL_circularstring_to_linestring(structure(list(x), crs = NA_crs_, precision = 0.0, 
		class = c("sfc_CIRCULARSTRING", "sfc")))[[1]]
}

#' @name st_cast
#' @export
st_cast.MULTISURFACE <- function(x, to, ...) {
	if (! missing(to) && to != "MULTIPOLYGON")
		stop("MULTISURFACE can only be converted into MULTIPOLYGON")
	CPL_multisurface_to_multipolygon(structure(list(x), crs = NA_crs_, precision = 0.0, 
		class = c("sfc_MULTISURFACE", "sfc")))[[1]]
}

#' @name st_cast
#' @export
st_cast.COMPOUNDCURVE <- function(x, to, ...) {
	if (! missing(to) && to != "LINESTRING")
		stop("to should be missing or LINESTRING")
	CPL_compoundcurve_to_linear(structure(list(x), crs = NA_crs_, precision = 0.0, 
		class = c("sfc_COMPOUNDCURVE", "sfc")))[[1]]
}

#' @name st_cast
#' @export
st_cast.MULTICURVE <- function(x, to, ...) {
	if (! missing(to) && to != "MULTILINESTRING")
		stop("to should be missing or MULTILINESTRING")
	st_multilinestring(lapply(x, st_cast, to = "LINESTRING"))
}


#' @name st_cast
#' @export
st_cast.CURVE <- function(x, to, ...) { # nocov start
	if (! missing(to) && to != "LINESTRING")
		stop("CURVE can only be converted into LINESTRING")
	CPL_curve_to_linestring(structure(list(x), crs = NA_crs_, precision = 0.0, 
		class = c("sfc_CURVE", "sfc")))[[1]]
} # nocov end

# st_cast.class <- function(x, to) {
#   switch(to, 
#          MULTIPOLYGON = x, 
#          MULTILINESTRING = x, 
#          MULTIPOINT = x, 
#          POLYGON = x, 
#          LINESTRING = x,
#          POINT = x
#   )
# }

