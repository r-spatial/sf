test_that("`sfc` vectors are treated as vectors", {
	expect_true(vctrs::vec_is(st_sfc(st_point())))
	expect_true(vctrs::vec_is(st_sfc(st_linestring())))
	expect_true(vctrs::vec_is(st_sfc(st_point(), st_linestring())))
})

test_that("`sfc` vector proxy is correct", {
	x = st_sfc(st_point())
	proxy = x
	attributes(proxy) = NULL
	expect_identical(vctrs::vec_proxy(x), proxy)

	x = st_sfc(st_linestring())
	proxy = x
	attributes(proxy) = NULL
	expect_identical(vctrs::vec_proxy(x), proxy)

	x = st_sfc(st_point(), st_linestring())
	proxy = x
	attributes(proxy) = NULL
	expect_identical(vctrs::vec_proxy(x), proxy)
})

test_that("`sfc` restore proxy is correct", {
	x = st_sfc(st_point())
	proxy = vctrs::vec_proxy(x)
	expect_identical(vctrs::vec_restore(proxy, x), x)

	x = st_sfc(st_linestring())
	proxy = vctrs::vec_proxy(x)
	expect_identical(vctrs::vec_restore(proxy, x), x)

	x = st_sfc(st_point(), st_linestring())
	proxy = vctrs::vec_proxy(x)
	expect_identical(vctrs::vec_restore(proxy, x), x)
})

test_that("`sfc` vector ptype is correct", {
	x = st_sfc(st_point())
	expect_identical(vctrs::vec_ptype(x), x[0])
	expect_s3_class(vctrs::vec_ptype(x), "sfc_POINT")

	x = st_sfc(st_linestring())
	expect_identical(vctrs::vec_ptype(x), x[0])
	expect_s3_class(vctrs::vec_ptype(x), "sfc_LINESTRING")

	x = st_sfc(st_point(), st_linestring())
	expect_identical(vctrs::vec_ptype(x), x[0])
	expect_s3_class(vctrs::vec_ptype(x), "sfc_GEOMETRY")
})

test_that("`sfc` vector ptype2 is correct", {
	x = st_sfc(st_point())
	y = st_sfc(st_point(), crs = 3857)
	z = st_sfc(st_point(), precision = 1e-4)
	expect_identical(vctrs::vec_ptype2(x, x), x[0])
	expect_error(vctrs::vec_ptype2(x, y))
	expect_error(vctrs::vec_ptype2(x, z))
	expect_error(vctrs::vec_ptype2(x, st_sfc(st_linestring())))

	x = st_sfc(st_linestring())
	y = st_sfc(st_linestring(), crs = 3857)
	z = st_sfc(st_linestring(), precision = 1e-4)
	expect_identical(vctrs::vec_ptype2(x, x), x[0])
	expect_error(vctrs::vec_ptype2(x, y))
	expect_error(vctrs::vec_ptype2(x, z))
	expect_error(vctrs::vec_ptype2(x, st_sfc(st_point())))

	x = st_sfc(st_point(), st_linestring())
	y = st_sfc(st_point(), st_linestring(), crs = 3857)
	z = st_sfc(st_point(), st_linestring(), precision = 1e-4)
	expect_identical(vctrs::vec_ptype2(x, x), x[0])
	expect_error(vctrs::vec_ptype2(x, y))
	expect_error(vctrs::vec_ptype2(x, z))
	expect_error(vctrs::vec_ptype2(x, st_sfc(st_point())))
})

test_that("`sfc` vector cast is correct", {
	x = st_sfc(st_point())
	expect_identical(vctrs::vec_cast(x, x), x)
	expect_error(vctrs::vec_cast(x, st_sfc(st_linestring())))

	x = st_sfc(st_linestring())
	expect_identical(vctrs::vec_cast(x, x), x)
	expect_error(vctrs::vec_cast(x, st_sfc(st_point())))

	x = st_sfc(st_point(), st_linestring())
	expect_identical(vctrs::vec_cast(x, x), x)
	expect_error(vctrs::vec_cast(x, st_sfc(st_point())))
})

test_that("`sfc` vectors can be sliced", {
	x = st_sfc(st_point(1:2), st_point(3:4))
	expect_identical(vctrs::vec_slice(x, 1), x[1])
	expect_identical(vctrs::vec_slice(x, 0), x[0])

	x = st_sfc(
		st_linestring(matrix(1:2, ncol = 2)),
		st_linestring(matrix(3:4, ncol = 2))
	)
	expect_identical(vctrs::vec_slice(x, 1), x[1])
	expect_identical(vctrs::vec_slice(x, 0), x[0])

	x = st_sfc(
		st_point(1:2),
		st_linestring(matrix(3:4, ncol = 2))
	)
	expect_identical(vctrs::vec_slice(x, 1), x[1])
	expect_identical(vctrs::vec_slice(x, 0), x[0])
})

test_that("`sfc` vectors can be initialized with correct missing value", {
	x = st_sfc(st_point())
	expect_identical(
		vctrs::vec_init(x, 2),
		st_sfc(st_point(), st_point())
	)

	x = st_sfc(st_linestring())
	expect_identical(
		vctrs::vec_init(x, 2),
		st_sfc(st_linestring(), st_linestring())
	)

	x = st_sfc(st_point(), st_linestring())
	expect_identical(
		vctrs::vec_init(x, 2),
		# This doesn't give a `sfc_GEOMETRY`, it gives an `sfc_GEOMETRYCOLLECTION`
		# st_sfc(st_geometrycollection(), st_geometrycollection())
		x[0][c(NA_integer_, NA_integer_)]
	)
})

test_that("`sfc` vectors can combine with unspecified in `vec_c()`", {
	na = st_point()
	x = st_point(1:2)
	out = vctrs::vec_c(c(NA, NA), st_sfc(x), NA)
	expect_identical(out, st_sfc(na, na, x, na))

	na = st_linestring()
	x = st_linestring(matrix(1:2, nrow = 1))
	out = vctrs::vec_c(c(NA, NA), st_sfc(x), NA)
	expect_identical(out, st_sfc(na, na, x, na))

	na = st_geometrycollection()
	point = st_point(1:2)
	line = st_linestring(matrix(3:4, nrow = 1))
	out = vctrs::vec_c(c(NA, NA), st_sfc(point, line), NA)
	expect_identical(out, st_sfc(na, na, point, line, na))
})

test_that("`sfc` vectors can combine with unspecified in `vec_rbind()`", {
	na = st_point()
	one = st_point(1:2)
	x = st_sfc(one)
	y = st_sfc(one, one)
	out = vctrs::vec_rbind(
		vctrs::data_frame(x = x),
		vctrs::data_frame(y = y)
	)
	expect_identical(
		out,
		vctrs::data_frame(
			x = st_sfc(one, na, na),
			y = st_sfc(na, one, one)
		)
	)

	na = st_linestring()
	one = st_linestring(matrix(1:2, nrow = 1))
	x = st_sfc(one)
	y = st_sfc(one, one)
	out = vctrs::vec_rbind(
		vctrs::data_frame(x = x),
		vctrs::data_frame(y = y)
	)
	expect_identical(
		out,
		vctrs::data_frame(
			x = st_sfc(one, na, na),
			y = st_sfc(na, one, one)
		)
	)

	na = st_geometrycollection()
	point = st_point(1:2)
	line = st_linestring(matrix(3:4, nrow = 1))
	x = st_sfc(point, line)
	y = st_sfc(line, point)
	out = vctrs::vec_rbind(
		vctrs::data_frame(x = x),
		vctrs::data_frame(y = y)
	)
	expect_identical(
		out,
		vctrs::data_frame(
			x = st_sfc(point, line, na, na),
			y = st_sfc(na, na, line, point)
		)
	)
})

test_that("`sfc` vector `n_empty` attribute is recomputed when slicing", {
	x = st_sfc(st_point(), st_point(0:1))
	expect_identical(attr(vctrs::vec_slice(x, 1), "n_empty"), 1L)
	expect_identical(attr(vctrs::vec_slice(x, 2), "n_empty"), 0L)

	x = st_sfc(st_linestring(), st_linestring(matrix(1:2, nrow = 1)))
	expect_identical(attr(vctrs::vec_slice(x, 1), "n_empty"), 1L)
	expect_identical(attr(vctrs::vec_slice(x, 2), "n_empty"), 0L)

	x = st_sfc(st_point(), st_linestring(matrix(1:2, nrow = 1)))
	expect_identical(attr(vctrs::vec_slice(x, 1), "n_empty"), 1L)
	expect_identical(attr(vctrs::vec_slice(x, 2), "n_empty"), 0L)
})

test_that("`sfc` vector `n_empty` attribute is recomputed when combining", {
	x = st_sfc(st_point())
	y = st_sfc(st_point(0:1))
	combined = vctrs::vec_c(x, y, x)
	expect_length(combined, 3)
	expect_identical(attr(combined, "n_empty"), 2L)

	x = st_sfc(st_linestring())
	y = st_sfc(st_linestring(matrix(1:2, nrow = 1)))
	combined = vctrs::vec_c(x, y, x)
	expect_length(combined, 3)
	expect_identical(attr(combined, "n_empty"), 2L)

	x = st_sfc(st_point(), st_linestring())
	y = st_sfc(st_point(), st_linestring(matrix(1:2, nrow = 1)))
	combined = vctrs::vec_c(x, y, x)
	expect_length(combined, 6)
	expect_identical(attr(combined, "n_empty"), 5L)
})

test_that("`sfc` vectors `bbox` attribute is recomputed when slicing", {
	x = st_sfc(st_point(c(1, 2)))
	y = st_sfc(st_point(c(10, 20)))
	combined = c(x, y)
	expect_identical(st_bbox(vctrs::vec_slice(combined, 1)), st_bbox(x))
	expect_identical(st_bbox(vctrs::vec_slice(combined, 2)), st_bbox(y))

	x = st_sfc(st_linestring(matrix(1:2, nrow = 1)))
	y = st_sfc(st_linestring(matrix(10:11, nrow = 1)))
	combined = c(x, y)
	expect_identical(st_bbox(vctrs::vec_slice(combined, 1)), st_bbox(x))
	expect_identical(st_bbox(vctrs::vec_slice(combined, 2)), st_bbox(y))

	x = st_sfc(st_linestring(matrix(1:2, nrow = 1)), st_point(3:4))
	y = st_sfc(st_linestring(matrix(10:11, nrow = 1)), st_point(12:15))
	combined = c(x, y)
	expect_identical(st_bbox(vctrs::vec_slice(combined, 1:2)), st_bbox(x))
	expect_identical(st_bbox(vctrs::vec_slice(combined, 3:4)), st_bbox(y))
})

test_that("`precision` and `crs` attributes of `sfc` vectors are restored when slicing", {
	x = st_sfc(st_point(), st_point(), precision = 1e-4, crs = 3857)
	out = vctrs::vec_slice(x, 1)
	expect_identical(st_precision(x), st_precision(out))
	expect_identical(st_crs(x), st_crs(out))

	x = st_sfc(st_linestring(), st_linestring(), precision = 1e-4, crs = 3857)
	out = vctrs::vec_slice(x, 1)
	expect_identical(st_precision(x), st_precision(out))
	expect_identical(st_crs(x), st_crs(out))

	x = st_sfc(st_point(), st_linestring(), precision = 1e-4, crs = 3857)
	out = vctrs::vec_slice(x, 1)
	expect_identical(st_precision(x), st_precision(out))
	expect_identical(st_crs(x), st_crs(out))
})

test_that("`precision` and `crs` attributes of `sfc` vectors must be the same when combining", {
	x = st_sfc(st_point(c(pi, pi)), precision = 1e-4, crs = 3857)
	y = st_sfc(st_point(c(0, 0)), precision = 1e-2, crs = 3857)
	z = st_sfc(st_point(c(0, 0)), precision = 1e-4, crs = 4326)
	expect_identical(st_precision(x), st_precision(vctrs::vec_c(x, x)))
	expect_identical(st_crs(x), st_crs(vctrs::vec_c(x, x)))
	expect_error(vctrs::vec_c(x, y), "precisions not equal")
	expect_error(vctrs::vec_c(x, z), "arguments have different crs")
	expect_error(c(x, z), "arguments have different crs")

	x = st_sfc(st_linestring(matrix(1:2, nrow = 1)), precision = 1e-4, crs = 3857)
	y = st_sfc(st_linestring(matrix(1:2, nrow = 1)), precision = 1e-2, crs = 3857)
	z = st_sfc(st_linestring(matrix(1:2, nrow = 1)), precision = 1e-4, crs = 4326)
	expect_identical(st_precision(x), st_precision(vctrs::vec_c(x, x)))
	expect_identical(st_crs(x), st_crs(vctrs::vec_c(x, x)))
	expect_error(vctrs::vec_c(x, y), "precisions not equal")
	expect_error(vctrs::vec_c(x, z), "arguments have different crs")
	expect_error(c(x, z), "arguments have different crs")

	point = st_point()
	line = st_linestring(matrix(1:2, nrow = 1))
	x = st_sfc(point, line, precision = 1e-4, crs = 3857)
	y = st_sfc(point, line, precision = 1e-2, crs = 3857)
	z = st_sfc(point, line, precision = 1e-4, crs = 4326)
	expect_identical(st_precision(x), st_precision(vctrs::vec_c(x, x)))
	expect_identical(st_crs(x), st_crs(vctrs::vec_c(x, x)))
	expect_error(vctrs::vec_c(x, y), "precisions not equal")
	expect_error(vctrs::vec_c(x, z), "arguments have different crs")
	expect_error(c(x, z), "arguments have different crs")
})

# ------------------------------------------------------------------------------
# Miscellaneous

test_that("`vec_locate_matches()` works with `sfc` vectors", {
	x = c(
		st_sfc(st_point(c(0, 0))),
		st_sfc(st_point(c(0, 1))),
		st_sfc(st_point(c(2, 1)))
	)

	y = c(
		st_sfc(st_point(c(0, 0))),
		st_sfc(st_point(c(0, 3))),
		st_sfc(st_point(c(0, 0))),
		st_sfc(st_point(c(0, 1)))
	)

	out = vctrs::vec_locate_matches(x, y)
	expect_identical(out$needles, c(1L, 1L, 2L, 3L))
	expect_identical(out$haystack, c(1L, 3L, 4L, NA))
})
