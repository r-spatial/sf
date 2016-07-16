#' create sf object
#' 
#' create sf, which extends data.frame-like objects with a simple feature list column
#'
#' @param df object of class \code{data.frame}
#' @param is_what character vector; indicates for each attribute column how it relates to the geometry; see Details
#'
#' @details is_what specified for each attribute column how it relates to the geometry, and can have one of following values: constant, aggregation, identifier.
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
sf = function(df, is_what = rep(as.character(NA), ncol(df) - 1)) {
	sf = sapply(df, function(x) inherits(x, "sfc"))
	if (!any(sf))
		stop("no simple features geometry column present")
	sf_column = which(sf)
	if (length(sf_column) > 1) {
		warning("more than one geometry column: ignoring all but first")
		df = df[,-sf_column[-1]]
	}
	attr(df, "sf_column") = sf_column[1]
 	is_what = rep(is_what, length.out = ncol(df) - 1)
 	if (any(!is.na(is_what)) && !all(na.omit(is_what) %in% c("field", "lattice", "entity")))
	 		stop("unknown value for is_what; allowed values: field, lattice, entity")
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
