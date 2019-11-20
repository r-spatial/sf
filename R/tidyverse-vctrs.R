
# This returns the memory representation of `sfc` vectors. At the same
# time, this declares `sfc` lists as vectors which is necessary
# because vctrs generally treats S3 lists as scalars.
<<<<<<< HEAD
vec_proxy.sfc <- function(x, ...) {
=======
vec_proxy.sfc = function(x, ...) {
>>>>>>> 6946fd901beb39cfa604c8383592967a847ca9bd
	x
}
# This restores `sfc` attributes after manipulation of the proxy
# (e.g. slicing or combination)
<<<<<<< HEAD
vec_restore.sfc <- function(x, to, ...) {
=======
vec_restore.sfc = function(x, to, ...) {
>>>>>>> 6946fd901beb39cfa604c8383592967a847ca9bd
	# Ensure restoration of `n_empty` by `st_sfc()`
	attr(x, "n_empty") = NULL

	st_sfc(x, crs = st_crs(to), precision = st_precision(to))
}

#' vctrs methods for sf objects
#' @name vctrs
#' @export
#' @inheritParams vctrs::vec_ptype2
<<<<<<< HEAD
vec_ptype2.sfc <- function(x, y, ...) {
=======
vec_ptype2.sfc = function(x, y, ...) {
>>>>>>> 6946fd901beb39cfa604c8383592967a847ca9bd
	UseMethod("vec_ptype2.sfc", y)
}
#' @name vctrs
#' @export
<<<<<<< HEAD
vec_ptype2.sfc.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
=======
vec_ptype2.sfc.default = function(x, y, ..., x_arg = "x", y_arg = "y") {
>>>>>>> 6946fd901beb39cfa604c8383592967a847ca9bd
	vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @name vctrs
#' @export
<<<<<<< HEAD
vec_ptype2.sfc.sfc <- function(x, y, ...) {
	ucls = unique(c(class(x)[1], class(y)[1]))
	cls =
=======
vec_ptype2.sfc.sfc = function(x, y, ...) {
	crs = common_crs(x, y)
	prec = common_prec(x, y)
	ret = st_sfc(crs = crs, precision = prec)

	ucls = unique(c(class(x)[1], class(y)[1]))
	class(ret) =
>>>>>>> 6946fd901beb39cfa604c8383592967a847ca9bd
		if (length(ucls) > 1) # a mix:
			c("sfc_GEOMETRY", "sfc")
		else
			c(ucls, "sfc")
<<<<<<< HEAD
	structure(st_sfc(), class = cls)
=======

	ret
}

# take conservative approach of requiring equal CRS and precision
common_crs = function(x, y) {
	lhs = st_crs(x)
	rhs = st_crs(y)

	if (lhs != rhs)
		stop("coordinate reference systems not equal: use st_transform() first?")

	lhs
}
common_prec = function(x, y) {
	lhs = st_precision(x)
	rhs = st_precision(y)

	if (lhs != rhs)
		stop("precisions not equal")

	lhs
>>>>>>> 6946fd901beb39cfa604c8383592967a847ca9bd
}


# minimal vec_cast implementation: https://github.com/r-spatial/sf/issues/1068
#' @name vctrs
#' @export
#' @inheritParams vctrs::vec_cast
#' @param x_arg,y_arg Argument names for \code{x} and \code{y}.
<<<<<<< HEAD
vec_cast.sfc <- function(x, to, ...) UseMethod("vec_cast.sfc") # nocov

#' @name vctrs
#' @export
vec_cast.sfc.sfc <- function(x, to, ...) {
=======
vec_cast.sfc = function(x, to, ...) UseMethod("vec_cast.sfc") # nocov

#' @name vctrs
#' @export
vec_cast.sfc.sfc = function(x, to, ...) {
>>>>>>> 6946fd901beb39cfa604c8383592967a847ca9bd
	st_cast(x, gsub("sfc_", "", class(to)[1])) # nocov
}

#' @name vctrs
#' @export
<<<<<<< HEAD
vec_cast.sfc.default <- function(x, to, ...) {
=======
vec_cast.sfc.default = function(x, to, ...) {
>>>>>>> 6946fd901beb39cfa604c8383592967a847ca9bd
	if (!requireNamespace("vctrs", quietly = TRUE)) # nocov start
		stop("vctrs not available: install first?")
	vctrs::vec_default_cast(x, to) # nocov end
}


<<<<<<< HEAD
register_vctrs_methods <- function() {
=======
register_vctrs_methods = function() {
>>>>>>> 6946fd901beb39cfa604c8383592967a847ca9bd
	register_s3_method("vctrs", "vec_proxy", "sfc")
	register_s3_method("vctrs", "vec_restore", "sfc")
	register_s3_method("vctrs", "vec_ptype2", "sfc")
	register_s3_method("vctrs", "vec_cast", "sfc")
}
