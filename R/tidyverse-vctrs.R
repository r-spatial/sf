# All S3 methods in this file are registered lazily when vctrs is loaded

#nocov start
register_vctrs_methods = function() {
	s3_register("vctrs::vec_proxy", "sfc_POINT")
	s3_register("vctrs::vec_proxy", "sfc_LINESTRING")
	s3_register("vctrs::vec_proxy", "sfc_GEOMETRY")

	s3_register("vctrs::vec_restore", "sfc_POINT")
	s3_register("vctrs::vec_restore", "sfc_LINESTRING")
	s3_register("vctrs::vec_restore", "sfc_GEOMETRY")

	s3_register("vctrs::vec_ptype", "sfc_POINT")
	s3_register("vctrs::vec_ptype", "sfc_LINESTRING")
	s3_register("vctrs::vec_ptype", "sfc_GEOMETRY")

	s3_register("vctrs::vec_ptype2", "sfc_POINT.sfc_POINT")
	s3_register("vctrs::vec_ptype2", "sfc_LINESTRING.sfc_LINESTRING")
	s3_register("vctrs::vec_ptype2", "sfc_GEOMETRY.sfc_GEOMETRY")

	s3_register("vctrs::vec_cast", "sfc_POINT.sfc_POINT")
	s3_register("vctrs::vec_cast", "sfc_LINESTRING.sfc_LINESTRING")
	s3_register("vctrs::vec_cast", "sfc_GEOMETRY.sfc_GEOMETRY")
}
#nocov end

vec_proxy.sfc_POINT = function(x, ...) {
	vec_proxy_sfc(x)
}
vec_proxy.sfc_LINESTRING = function(x, ...) {
	vec_proxy_sfc(x)
}
vec_proxy.sfc_GEOMETRY = function(x, ...) {
	vec_proxy_sfc(x)
}
vec_proxy_sfc = function(x) {
	sf_unstructure(x)
}

vec_restore.sfc_POINT = function(x, to, ...) {
	vec_restore_sfc(x, to)
}
vec_restore.sfc_LINESTRING = function(x, to, ...) {
	vec_restore_sfc(x, to)
}
vec_restore.sfc_GEOMETRY = function(x, to, ...) {
	vec_restore_sfc(x, to)
}
vec_restore_sfc = function(x, to) {
	st_sfc(
		x,
		crs = st_crs(to),
		precision = st_precision(to),
		fall_back_class = class(to)
	)
}

vec_ptype.sfc_POINT = function(x, ...) {
	vec_ptype_sfc(x)
}
vec_ptype.sfc_LINESTRING = function(x, ...) {
	vec_ptype_sfc(x)
}
vec_ptype.sfc_GEOMETRY = function(x, ...) {
	vec_ptype_sfc(x)
}
vec_ptype_sfc = function(x) {
	st_sfc(
		crs = st_crs(x),
		precision = st_precision(x),
		fall_back_class = class(x)
	)
}

vec_ptype2.sfc_POINT.sfc_POINT = function(x, y, ...) {
	vec_ptype2_sfc_sfc(x, y)
}
vec_ptype2.sfc_LINESTRING.sfc_LINESTRING = function(x, y, ...) {
	vec_ptype2_sfc_sfc(x, y)
}
vec_ptype2.sfc_GEOMETRY.sfc_GEOMETRY = function(x, y, ...) {
	vec_ptype2_sfc_sfc(x, y)
}
vec_ptype2_sfc_sfc = function(x, y) {
	check_same_crs(x, y)
	check_same_precision(x, y)
	x
}

vec_cast.sfc_POINT.sfc_POINT = function(x, to, ...) {
	vec_cast_sfc_sfc(x, to)
}
vec_cast.sfc_LINESTRING.sfc_LINESTRING = function(x, to, ...) {
	vec_cast_sfc_sfc(x, to)
}
vec_cast.sfc_GEOMETRY.sfc_GEOMETRY = function(x, to, ...) {
	vec_cast_sfc_sfc(x, to)
}
vec_cast_sfc_sfc = function(x, to) {
	check_same_crs(x, to)
	check_same_precision(x, to)
	x
}

sf_unstructure = function(x) {
	if (is.data.frame(x)) {
		x = vctrs::new_data_frame(x, row.names = .row_names_info(x, 0L))
	} else if (!is.null(dim(x))) {
		attributes(x) = list(dim = dim(x), dimnames = dimnames(x))
	} else {
		attributes(x) = list(names = names(x))
	}
	x
}

# Take conservative approach of requiring equal CRS
check_same_crs = function(x, y) {
	lhs = st_crs(x)
	rhs = st_crs(y)

	if (lhs != rhs) {
		stop("arguments have different crs")
	}

	invisible()
}

# Take conservative approach of requiring equal precision
check_same_precision = function(x, y) {
	lhs = st_precision(x)
	rhs = st_precision(y)

	if (lhs != rhs) {
		stop("precisions not equal")
	}

	invisible()
}
