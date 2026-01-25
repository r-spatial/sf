types = c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", 
            "POLYGON", "MULTIPOLYGON", "GEOMETRY")
sfc_types = paste0("sfc_", types)

# All S3 methods in this file are registered lazily when vctrs is loaded
register_vctrs_methods = function() {
  # Register vec_proxy, vec_restore, vec_ptype for all types
  for (type in sfc_types) {
    s3_register("vctrs::vec_proxy", type)
    s3_register("vctrs::vec_restore", type)
    s3_register("vctrs::vec_ptype", type)
  }
  
  # Register vec_ptype2 for all pairs
  for (i in seq_along(sfc_types)) {
    for (j in seq_along(sfc_types)) {
      s3_register("vctrs::vec_ptype2", paste0(sfc_types[i], ".", sfc_types[j]))
    }
  }
  
  # Register vec_cast for all pairs
  for (i in seq_along(sfc_types)) {
    for (j in seq_along(sfc_types)) {
      s3_register("vctrs::vec_cast", paste0(sfc_types[i], ".", sfc_types[j]))
    }
  }
}

# vec_proxy implementations:
vec_proxy.sfc_POINT = function(x, ...)  vec_proxy_sfc(x)
vec_proxy.sfc_MULTIPOINT = function(x, ...) vec_proxy_sfc(x)
vec_proxy.sfc_LINESTRING = function(x, ...) vec_proxy_sfc(x)
vec_proxy.sfc_MULTILINESTRING = function(x, ...) vec_proxy_sfc(x)
vec_proxy.sfc_POLYGON = function(x, ...) vec_proxy_sfc(x)
vec_proxy.sfc_MULTIPOLYGON = function(x, ...) vec_proxy_sfc(x)
vec_proxy.sfc_GEOMETRY = function(x, ...) vec_proxy_sfc(x)
vec_proxy_sfc = function(x) sf_unstructure(x)

vec_restore.sfc_POINT = function(x, to, ...) vec_restore_sfc(x, to)
vec_restore.sfc_MULTIPOINT = function(x, to, ...) vec_restore_sfc(x, to)
vec_restore.sfc_LINESTRING = function(x, to, ...) vec_restore_sfc(x, to)
vec_restore.sfc_MULTILINESTRING = function(x, to, ...) vec_restore_sfc(x, to)
vec_restore.sfc_POLYGON = function(x, to, ...) vec_restore_sfc(x, to)
vec_restore.sfc_MULTIPOLYGON = function(x, to, ...) vec_restore_sfc(x, to)
vec_restore.sfc_GEOMETRY = function(x, to, ...) vec_restore_sfc(x, to)

vec_restore_sfc = function(x, to) {
	st_sfc(
		x,
		crs = st_crs(to),
		precision = st_precision(to),
		fall_back_class = class(to)
	)
}

# vec_type methods:
vec_ptype.sfc_POINT = function(x, ...) vec_ptype_sfc(x)
vec_ptype.sfc_MULTIPOINT = function(x, ...) vec_ptype_sfc(x)
vec_ptype.sfc_LINESTRING = function(x, ...) vec_ptype_sfc(x)
vec_ptype.sfc_MULTILINESTRING = function(x, ...) vec_ptype_sfc(x)
vec_ptype.sfc_POLYGON = function(x, ...) vec_ptype_sfc(x)
vec_ptype.sfc_MULTIPOLYGON = function(x, ...) vec_ptype_sfc(x)
vec_ptype.sfc_GEOMETRY = function(x, ...) vec_ptype_sfc(x)

vec_ptype_sfc = function(x) {
	st_sfc(
		crs = st_crs(x),
		precision = st_precision(x),
		fall_back_class = class(x)
	)
}

# Single implementation that works for all type pairs
vec_ptype2_impl = function(x, y) {
  check_same_crs(x, y)
  check_same_precision(x, y)
  if (identical(class(x), class(y)))
    x
  else # return empty sfc_GEOMETRY for mixed types
    st_sfc(crs = st_crs(x), precision = st_precision(x))
}

# Then assign to all pairs
for (type1 in sfc_types) {
  for (type2 in sfc_types) {
    assign(paste0("vec_ptype2.", type1, ".", type2), 
           function(x, y, ...) vec_ptype2_impl(x, y))
  }
}

for (type1 in sfc_types) {
  for (type2 in sfc_types) {
	if (type1 == type2)
      assign(paste0("vec_cast", type1, ".", type2), 
             function(x, y, ...) vec_cast_sfc_sfc(x, y))
	else 
      assign(paste0("vec_cast", type1, ".", type2), 
             function(x, y, ...) vec_cast_to_geometry(x, y))
  }
}

vec_cast_sfc_sfc = function(x, to) {
	check_same_crs(x, to)
	check_same_precision(x, to)
	x
}

vec_cast_to_geometry = function(x, to) {
	check_same_crs(x, to)
	check_same_precision(x, to)
	# print(paste("cast from", class(x), "to sfc_GEOMETRY"))
	if (length(x))
		st_cast(x, "GEOMETRY")
	else
		st_sfc(crs = st_crs(x), precision = st_precision(x))
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
  
  if (lhs != rhs)
    stop(paste("CRS mismatch:",  lhs$input %||% "NA", "vs", rhs$input %||% "NA"), 
         call. = FALSE)
  invisible()
}

check_same_precision = function(x, y) {
  lhs = st_precision(x)
  rhs = st_precision(y)
  
  if (lhs != rhs) 
    stop(paste("Precision mismatch:", lhs, "vs", rhs), call. = FALSE)
  invisible()
}