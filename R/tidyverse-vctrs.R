
# This returns the memory representation of `sfc` vectors. At the same
# time, this declares `sfc` lists as vectors which is necessary
# because vctrs generally treats S3 lists as scalars.
vec_proxy.sfc = function(x, ...) {
	x
}
# This restores `sfc` attributes after manipulation of the proxy
# (e.g. slicing or combination)
vec_restore.sfc = function(x, to, ...) {
	# Ensure restoration of `n_empty` by `st_sfc()`
	attr(x, "n_empty") = NULL

	st_sfc(x, crs = st_crs(to), precision = st_precision(to))
}

#' vctrs methods for sf objects
#' @name vctrs
#' @export
#' @export vec_ptype2.sfc
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.sfc = function(x, y, ...) {
	UseMethod("vec_ptype2.sfc", y)
}
#' @name vctrs
#' @method vec_ptype2.sfc default
#' @export
vec_ptype2.sfc.default = function(x, y, ..., x_arg = "x", y_arg = "y") {
	vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}
#' @name vctrs
#' @method vec_ptype2.sfc sfc
#' @export
vec_ptype2.sfc.sfc = function(x, y, ...) {
	crs = common_crs(x, y)
	prec = common_prec(x, y)
	ret = st_sfc(crs = crs, precision = prec)

	ucls = unique(c(class(x)[1], class(y)[1]))
	class(ret) =
		if (length(ucls) > 1) # a mix:
			c("sfc_GEOMETRY", "sfc")
		else
			c(ucls, "sfc")

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
}


# minimal vec_cast implementation: https://github.com/r-spatial/sf/issues/1068
#' @name vctrs
#' @export
#' @inheritParams vctrs::vec_cast
#' @param x_arg,y_arg Argument names for \code{x} and \code{y}.
vec_cast.sfc = function(x, to, ...) UseMethod("vec_cast.sfc") # nocov

#' @name vctrs
#' @export
vec_cast.sfc.sfc = function(x, to, ...) {
	st_cast(x, gsub("sfc_", "", class(to)[1])) # nocov
}

#' @name vctrs
#' @export
vec_cast.sfc.default = function(x, to, ...) {
	if (!requireNamespace("vctrs", quietly = TRUE)) # nocov start
		stop("vctrs not available: install first?")
	vctrs::vec_default_cast(x, to) # nocov end
}

#' @name vctrs
#' @export
vec_ptype.sfc = function(x, ...) {
	x[0]
}

#' @name vctrs
#' @export
vec_ptype.sfc_POINT = function(x, ...) {
	x[0]
}

#' @name vctrs
#' @method vec_ptype2.sfc sfc
#' @export
vec_ptype2.sfc_POINT.sfc_POINT = function(x, y, ...) {
	crs = common_crs(x, y)
	prec = common_prec(x, y)
	ret = st_sfc(crs = crs, precision = prec)

	ucls = unique(c(class(x)[1], class(y)[1]))
	class(ret) =
		if (length(ucls) > 1) # a mix:
			c("sfc_GEOMETRY", "sfc")
		else
			c(ucls, "sfc")

	ret
}

#nocov start
register_vctrs_methods = function() {
	s3_register("vctrs::vec_proxy", "sfc")
	s3_register("vctrs::vec_restore", "sfc")
	s3_register("vctrs::vec_ptype2", "sfc")
	s3_register("vctrs::vec_cast", "sfc")
	s3_register("vctrs::vec_ptype", "sfc")
	s3_register("vctrs::vec_ptype", "sfc_POINT")
}
#nocov end
