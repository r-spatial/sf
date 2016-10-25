#' transform or convert coordinates of simple feature
#' 
#' transform or convert coordinates of simple feature
#' 
#' @param x object of class sf, sfc or sfg
#' @param crs coordinate reference system: integer with the epsg code, or character with proj4string
#' @param ... ignored
#' 
#' @details transforms coordinates of object to new projection
#' @examples
#' p1 = st_point(c(7,52))
#' p2 = st_point(c(-30,20))
#' sfc = st_sfc(p1, p2, crs = "+init=epsg:4326")
#' st_transform(sfc, "+init=epsg:3857")
#' @export
st_transform = function(x, crs) UseMethod("st_transform")

#' @name st_transform
#' @export
#' @examples
#' st_transform(st_sf(a=2:1, geom=sfc), "+init=epsg:3857")
st_transform.sfc = function(x, crs, ...) {
	if (is.na(st_crs(x)))
		stop("sfc object should have crs set")
	if (missing(crs))
		stop("argument crs cannot be missing")
	if (is.numeric(crs)) { # keep epsg:
		proj4string = CPL_proj4string_from_epsg(crs)
		suppressWarnings(st_sfc(CPL_transform(x, proj4string), crs = crs))
	} else
		suppressWarnings(st_sfc(CPL_transform(x, crs), crs = crs))
}

#' @name st_transform
#' @export
st_transform.sf = function(x, crs, ...) {
	x[[ attr(x, "sf_column") ]] = st_transform(st_geometry(x), crs)
	x
}

#' @name st_transform
#' @export
#' @details the st_transform method for sfg objects assumes that the crs of the object is available as an attribute of that name.
#' @examples 
#' st_transform(structure(p1, proj4string = "+init=epsg:4326"), "+init=epsg:3857")
st_transform.sfg = function(x, crs , ...) {
	x = st_sfc(x, crs = attr(x, "proj4string"))
	if (missing(crs))
		stop("argument crs cannot be missing")
	if (is.numeric(crs)) # epsg
		crs = CPL_proj4string_from_epsg(crs)
	CPL_transform(x, crs)[[1]]
}
