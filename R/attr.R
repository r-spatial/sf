#' Return non-spatial attributes of a sf object
#'
#' Convenience function to return non-spatial feature attributes of a \code{sf} object as a \code{data.frame}.
#' @param x object of class \code{sf}.
#' @return a \code{data.frame} with the non-spatial attributes of \code{x}.
#' @details A convenience function equivalent to the assignment method \code{st_geometry(x) <- NULL} without
#' modifying the input object.  Currently drops only a single geometry list column from the spatial object
#' and coerces the remaining columns to a \code{data.frame}. See \url{https://github.com/edzer/sfr/issues/229}
#' for discussion.
#' @examples
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#' str(nc, max.level = 2)
#' x <- st_attr(nc)
#' str(nc, max.level = 2)
#'
#' @export
st_attr = function(x) {
	stopifnot(inherits(x, "sf"))
	as.data.frame(x)[setdiff(names(x), attr(x, "sf_column"))]
}

