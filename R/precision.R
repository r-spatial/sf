#' Get precision
#'
#' @param x object of class \code{sfc} or \code{sf}
#' @export
st_precision <- function(x) {
  UseMethod("st_precision")
}

#' @export
st_precision.default <- function(x) {
	get(".sf.precision", envir = .sf_cache)
}

#' @export
st_precision.sf <- function(x) {
  x <- st_geometry(x)
  st_precision(x)
}

#' @export
st_precision.sfc <- function(x) {
  attr(x, "precision")
}

#' Set precision
#'
#' @name st_precision
#' @param precision numeric, or object of class \code{units} with distance units (but see details); see \link{st_as_binary} for how to do this.
#' @details If \code{precision} is a \code{units} object, the object on which we set precision must have a coordinate reference system with compatible distance units.
#'
#' Setting a \code{precision} has no direct effect on coordinates of geometries, but merely set an attribute tag to an \code{sfc} object. The effect takes place in \link{st_as_binary} or, more precise, in the C++ function \code{CPL_write_wkb}, where simple feature geometries are being serialized to well-known-binary (WKB). This happens always when routines are called in GEOS library (geometrical operations or predicates), for writing geometries using \link{st_write} or \link{write_sf}, \code{st_make_valid} in package \code{lwgeom}; also \link{aggregate} and \link{summarise} by default union geometries, which calls a GEOS library function. Routines in these libraries receive rounded coordinates, and possibly return results based on them. \link{st_as_binary} contains an example of a roundtrip of \code{sfc} geometries through WKB, in order to see the rounding happening to R data.
#'
#' The reason to support precision is that geometrical operations in GEOS or liblwgeom may work better at reduced precision. For writing data from R to external resources it is harder to think of a good reason to limiting precision.
#'
#' @seealso \link{st_as_binary} for an explanation of what setting precision does, and the examples therein.
#' @examples
#' x <- st_sfc(st_point(c(pi, pi)))
#' st_precision(x)
#' st_precision(x) <- 0.01
#' st_precision(x)
#' @export
st_set_precision <- function(x, precision) {
    UseMethod("st_set_precision")
}

#' @export
st_set_precision.numeric <- function(x, precision) {
	stopifnot(is.numeric(x), !is.na(x), is.finite(x), !missing(precision))
	assign(".sf.precision", x, envir=.sf_cache)
	st_precision()
}

#' @export
st_set_precision.sfc <- function(x, precision) {
    if (length(precision) != 1) {
        stop("Precision applies to all dimensions and must be of length 1.", call. = FALSE)
    }

	if (inherits(precision, "units")) {
		u = st_crs(x, parameters=TRUE)$ud_unit
		if (is.null(u) || !inherits(u, "units"))
			stop("cannot use precision expressed as units when target object has no units (CRS) set")
		units(precision) = 1/u # convert
		precision = as.numeric(precision)
	}

    if (is.na(precision) || !is.numeric(precision)) {
        stop("Precision must be numeric", call. = FALSE)
    }
    structure(x, precision = precision)
}

#' @export
st_set_precision.sf <- function(x, precision) {
    st_set_geometry(x, st_set_precision(st_geometry(x), precision))
}

#' @name st_precision
#' @param value precision value
#' @export
"st_precision<-" <- function(x, value) {
    st_set_precision(x, value)
}
