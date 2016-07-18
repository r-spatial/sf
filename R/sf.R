#' create sf object
#' 
#' create sf, which extends data.frame-like objects with a simple feature list column
#'
#' @param df object of class \code{data.frame}
#' @param relation_to_geometry character vector; indicates for each attribute column how it relates to the geometry; see Details
#'
#' @details \code{relation_to_geometry} specified for each attribute column how it relates to the geometry, and can have one of following values: "field", "lattice", "entity". "field" is used for attributes that are constant throughout the geometry (e.g. land use), "lattice" where the attribute is an aggregate value over the geometry (e.g. population density), "entity" when the attributes identifies the geometry of particular "thing", such as a building or a city.
#' 
#' @examples
#' pt1 = POINT(c(0,1))
#' pt2 = POINT(c(1,1))
#' sfc(list(pt1, pt2))
#' d = data.frame(a = 1:2)
#' d$geom = sfc(list(pt1, pt2))
#' df = sf(d)
#' d$geom2 = sfc(list(pt1, pt2))
#' sf(df) # warns
#' @export
sf = function(df, relation_to_geometry = rep(as.character(NA), ncol(df) - 1)) {
	sf = sapply(df, function(x) inherits(x, "sfc"))
	if (!any(sf))
		stop("no simple features geometry column present")
	sf_column = which(sf)
	if (length(sf_column) > 1) {
		warning("more than one geometry column: ignoring all but first")
		df = df[,-sf_column[-1]]
	}
	attr(df, "sf_column") = sf_column[1]
	f = factor(rep(relation_to_geometry, length.out = ncol(df) - 1), 
		levels = c("field", "lattice", "entity"))
	names(f) = names(df)[-sf_column[1]]
	attr(df, "relation_to_geometry") = f
	# TODO: check that lattice has to anything but POINT
	class(df) = c("sf", class(df))
	df
}

#' get geometry from sf object
#' 
#' get geometry from sf object
#' @param obj object of class \link{sf}
#' @export
setMethod("geometry", "sf", function(obj) obj[[attr(obj, "sf_column")]])
