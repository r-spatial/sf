#' Transform or convert coordinates of simple feature
#' 
#' Transform or convert coordinates of simple feature
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
	#suppressWarnings(st_sfc(CPL_transform(x, crs), crs = crs))
	crs = make_crs(crs)
	if (crs != st_crs(x))
		st_sfc(CPL_transform(x, crs$proj4string), crs = crs)
	else
		x
}

#' @name st_transform
#' @export
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' st_area(nc[1,]) # area, using geosphere::areaPolygon
#' st_area(st_transform(nc[1,], 32119)) # NC state plane, m
#' st_area(st_transform(nc[1,], 2264)) # NC state plane, US foot
#' library(units)
#' as.units(st_area(st_transform(nc[1,], 2264)), make_unit("m")^2)
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
	CPL_transform(x, make_crs(crs)$proj4string)[[1]]
}
