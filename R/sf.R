# convert to, and from, sp::Spatial objects

#' convert foreign object to an sf object
#'
#' convert foreign object to an sf object
#' @param x object to be converted into an object class \code{sf}
#' @param ... further arguments
#' @export
ST_as.sf = function(x, ...) UseMethod("ST_as.sf")

#' @name ST_as.sf
#'
#' @param relation_to_geometry character vector; see details section of \link{ST_sf}
#' 
#' @examples
#' pt1 = ST_Point(c(0,1))
#' pt2 = ST_Point(c(1,1))
#' ST_sfc(list(pt1, pt2))
#' d = data.frame(a = 1:2)
#' d$geom = ST_sfc(list(pt1, pt2))
#' df = ST_as.sf(d)
#' d$geom2 = ST_sfc(list(pt1, pt2))
#' ST_as.sf(df) # should warn
#' @export
ST_as.sf.data.frame = function(x, ..., relation_to_geometry = NA_character_)
	do.call(ST_sf, c(as.list(x), relation_to_geometry = relation_to_geometry))

#' get geometry from sf object
#' 
#' get geometry from sf object
#' @param obj object of class \code{sf}
#' @export
setMethod("geometry", "sf", function(obj) obj[[attr(obj, "sf_column")]])

#' create sf object
#' 
#' create sf, which extends data.frame-like objects with a simple feature list column
#' @param ... column elements to be binded into an \code{sf} object, one of them being of class \code{sfc}
#' @param relation_to_geometry character vector; see details below.
#' @param row.names row.names for the created \code{sf} object
#' @details \code{relation_to_geometry} specified for each non-geometry column how it relates to the geometry, and can have one of following values: "field", "lattice", "entity". "field" is used for attributes that are constant throughout the geometry (e.g. land use), "lattice" where the attribute is an aggregate value over the geometry (e.g. population density), "entity" when the attributes identifies the geometry of particular "thing", such as a building or a city. The default value, \code{NA_character_}, implies we don't know.  #' @examples
#' g = ST_sfc(list(ST_Point(1:2)))
#' ST_sf2(a=3,g)
#' ST_sf2(g, a=3)
#' ST_sf2(a=3, ST_sfc(list(ST_Point(1:2)))) # better to name it!
#' @export
ST_sf = function(..., relation_to_geometry = NA_character_, row.names) {
	x = list(...)
	# find & remove the sfc column:
	sf = sapply(x, function(x) inherits(x, "sfc"))
	if (!any(sf))
		stop("no simple features geometry column present")
	sf_column = which(sf)
	if (length(sf_column) > 1) {
		warning("more than one geometry column: ignoring all but first")
		sf_column = sf_column[1]
	}
	if (missing(row.names))
		row.names = seq(length(x[[sf_column]]))
	df = if (length(x) == 1) # ONLY sfc
			data.frame(row.names = row.names)
		else 
			data.frame(x[-sf_column], row.names = row.names)
	# add sfc column, with right name:
	object = as.list(substitute(list(...)))[-1L] 
	arg_nm = sapply(object, function(x) deparse(x))
	sfc_name = if (nzchar(names(x)[sf_column]))
		names(x)[sf_column]
	else 
		arg_nm[sf_column]
	df[[sfc_name]] = x[[sf_column]]
	# add attributes:
	attr(df, "sf_column") = sf_column[1]
	f = factor(rep(relation_to_geometry, length.out = ncol(df) - 1), 
		levels = c("field", "lattice", "entity"))
	names(f) = names(df)[-sf_column[1]]
	attr(df, "relation_to_geometry") = f
	# TODO: check that lattice has to be anything but POINT
	class(df) = c("sf", class(df))
	df
}
