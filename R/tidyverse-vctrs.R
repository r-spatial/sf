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
  s3_register("vctrs::vec_proxy", "sf")
  s3_register("vctrs::vec_restore", "sf")
  s3_register("vctrs::vec_ptype2", "sf.sf")
  s3_register("vctrs::vec_ptype2", "sf.data.frame")
  s3_register("vctrs::vec_ptype2", "data.frame.sf")
  s3_register("vctrs::vec_ptype2", "sf.tbl_df")
  s3_register("vctrs::vec_ptype2", "tbl_df.sf")
  s3_register("vctrs::vec_cast", "data.frame.sf")
  s3_register("vctrs::vec_cast", "sf.data.frame")
  s3_register("vctrs::vec_cast", "sf.tbl_df")
  s3_register("vctrs::vec_cast", "tbl_df.sf")
}

vec_proxy_sfc = function(x) sf_unstructure(x)

vec_restore_sfc = function(x, to) {
	st_sfc(
		x,
		crs = st_crs(to),
		precision = st_precision(to),
		fall_back_class = class(to)
	)
}

vec_ptype_sfc = function(x) {
	st_sfc(
		crs = st_crs(x),
		precision = st_precision(x),
		fall_back_class = class(x)
	)
}

# vec_proxy:
for (type in sfc_types) {
  assign(paste0("vec_proxy.", type), function(x, y, ...) vec_proxy_sfc(x))
  assign(paste0("vec_restore.", type), function(x, to, ...) vec_restore_sfc(x, to))
  assign(paste0("vec_ptype.", type), function(x, ...) vec_ptype_sfc(x))
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
      assign(paste0("vec_cast.", type1, ".", type2), 
             function(x, to, ...) vec_cast_sfc_sfc(x, to))
	else 
      assign(paste0("vec_cast.", type1, ".", type2), 
             function(x, to, ...) vec_cast_to_geometry(x, to))
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

## sf methods:
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

vec_proxy.sf = function(x, ...) {
  # Strip attributes to ensure `vec_restore()`'s call to `st_as_sf()` uses the
  # data frame S3 method, and can't use any information from `x`'s original
  # `sf` state
  sf_unstructure(x)
}

vec_restore.sf = function(x, to, ...) {
  # Due to the way `vec_ptype()` works, `vec_df_restore()` will preemptively
  # restore the `to` attributes by straight up copying them over. We really
  # don't want that! `sf::st_as_sf()` needs to S3 dispatch to the data frame
  # method. If `to` attributes are preemptively restored (including the class)
  # then it will instead dispatch on the sf method, and will "reuse"
  # attributes from `x`, which is incorrect. It should only use `to`
  # attributes when restoring. See TODO in `vec_df_restore()`.
  x = sf_unstructure(x)

  sf_column_name = attr(to, "sf_column")
  crs = st_crs(to)
  prec = st_precision(to)

  st_as_sf(
    x,
    sf_column_name = sf_column_name,
    crs = crs,
    precision = prec,
    stringsAsFactors = FALSE
  )
}

sf_ptype2 = function(x, y, ...) {
  data = vctrs::df_ptype2(x, y, ...)

  # Take active geometry from left-hand side
  sf_column_name = attr(x, "sf_column")

  # CRS and precision must match
  check_same_crs(x, y)
  check_same_precision(x, y)

  st_sf(data, sf_column_name = sf_column_name)
}

vec_ptype2.sf.sf = function(x, y, ...) {
  sf_ptype2(x, y, ...)
}

vec_ptype2.sf.data.frame = function(x, y, ...) {
  vctrs::df_ptype2(x, y, ...)
}
vec_ptype2.data.frame.sf = function(x, y, ...) {
  vctrs::df_ptype2(x, y, ...)
}

vec_ptype2.sf.tbl_df = function(x, y, ...) {
  vctrs::tib_ptype2(x, y, ...)
}
vec_ptype2.tbl_df.sf = function(x, y, ...) {
  vctrs::tib_ptype2(x, y, ...)
}

sf_cast = function(x, to, ...) {
  data = vctrs::df_cast(x, to, ...)

  sf_column_name = attr(to, "sf_column")
  crs = st_crs(to)
  prec = st_precision(to)

  st_as_sf(
    data,
    sf_column_name = sf_column_name,
    crs = crs,
    precision = prec,
    stringsAsFactors = FALSE
  )
}

# Because `vec_ptype2.sf.sf()` returns a sf
vec_cast.sf.sf = function(x, to, ...) {
  # sf_cast(x, to, ...)
  x
}

# Because `vec_ptype2.sf.data.frame()` returns a data frame
vec_cast.data.frame.sf = function(x, to, ...) {
  vctrs::df_cast(x, to, ...)
}
# Opt out of `vec_default_cast()` support for data.frame -> sf.
# Would never be called automatically, and likely not meaningful.
vec_cast.sf.data.frame = function(x, to, ..., x_arg = "", to_arg = "") {
  vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

# Because `vec_ptype2.sf.tbl_df()` returns a tibble
vec_cast.tbl_df.sf = function(x, to, ...) {
  vctrs::tib_cast(x, to, ...)
}
# Opt out of `vec_default_cast()` support for tibble -> sf.
# Would never be called automatically, and likely not meaningful.
vec_cast.sf.tbl_df = function(x, to, ..., x_arg = "", to_arg = "") {
  vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
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
