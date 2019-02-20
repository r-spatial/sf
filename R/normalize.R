#' Normalize simple features
#'
#' \code{st_normalize} transforms the coordinates in the input feature to fall
#' between 0 and 1. By default the current domain is set to the bounding box of
#' the input, but other domains can be used as well
#'
#' @param x object of class sf, sfc or sfg
#' @param domain The domain \code{x} should be normalized from as a length 4
#' vector of the form \code{c(xmin, ymin, xmax, ymax)}. Defaults to the
#' bounding box of \code{x}
#' @param ... ignored
#'
#' @export
#'
#' @examples
#' p1 = st_point(c(7,52))
#' st_normalize(p1, domain = c(0, 0, 10, 100))
#'
#' p2 = st_point(c(-30,20))
#' sfc = st_sfc(p1, p2, crs = 4326)
#' sfc
#' sfc_norm <- st_normalize(sfc)
#' st_bbox(sfc_norm)
#'
st_normalize <- function(x, domain = st_bbox(x), ...) {
	UseMethod("st_normalize")
}

#' @export
st_normalize.sfg <- function(x, domain = st_bbox(x), ...) {
	(x - domain[c(1, 2)]) / diag(c(domain[3] - domain[1], domain[4] - domain[2]))
}

#' @export
st_normalize.sfc <- function(x, domain = st_bbox(x), ...) {
	domain <- as.numeric(domain)
	if (length(x) == 0) return(x)
	min <- -domain[c(1, 2)]
	range <- 1 / c(domain[3] - domain[1], domain[4] - domain[2])
	if (any(is.infinite(range)) || any(range < 0)) {
		stop("domain must have a positive range") # nocov
	}
	normalize_sfc(x, min, range, NA_crs_)
}

#' @export
st_normalize.sf <- function(x, domain = st_bbox(x), ...) {
	x[[ attr(x, "sf_column") ]] = st_normalize(st_geometry(x), domain, ...)
	x
}
