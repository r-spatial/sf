#' coordinate transformation or conversion
#' 
#' coordinate transformation or conversion
#' 
#' @param x object of class sf, sfc or sfi
#' @param proj4string character; describing the coordinate reference systems in PROJ.4 syntax
#' @param ... ignored
#' 
#' @details transforms coordinates of object to new projection
#' @examples
#' p1 = st_point(c(7,52))
#' p2 = st_point(c(-30,20))
#' sfc = st_sfc(p1, p2, proj4string = "+init=epsg:4326")
#' st_transform(sfc, "+init=epsg:3857")
#' @export
st_transform = function(x, proj4string) UseMethod("st_transform")

#' @name st_transform
#' @export
#' @examples
#' st_transform(st_sf(a=2:1, geom=sfc), "+init=epsg:3857")
st_transform.sfc = function(x, proj4string = NA_character_, ...) {
	if (is.na(attr(x, "proj4string")))
		stop("sfc object should have proj4string set")
	if (is.na(proj4string))
		stop("argument proj4string cannot be missing")
	out = OGR_Transform(x, proj4string)
	do.call(st_sfc, c(out, proj4string = attr(out, "proj4string")))
}

#' @name st_transform
#' @export
st_transform.sf = function(x, proj4string = NA_character_, ...) {
	geom = st_geometry(x)
	if (is.na(attr(geom, "proj4string")))
		stop("sfc object should have proj4string set")
	if (is.na(proj4string))
		stop("argument proj4string cannot be missing")
	geom = st_transform(geom, proj4string)
	x[[attr(x, "sf_column")]] = geom # replace 
	x
}

#' @name st_transform
#' @export
#' @examples 
#' st_transform(structure(p1, proj4string = "+init=epsg:4326"), "+init=epsg:3857")
st_transform.sfi = function(x, proj4string = NA_character_, ...) {
	x = st_sfc(x, proj4string = attr(x, "proj4string"))
	if (is.na(proj4string))
		stop("argument proj4string cannot be missing")
	OGR_Transform(x, proj4string)[[1]]
}
