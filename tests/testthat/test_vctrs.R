context("sf: vctrs implementations")

test_that("`sfc` vectors are treated as vectors", {
	expect_true(vctrs::vec_is(st_sfc(st_point())))
})

test_that("`n_empty` attribute of `sfc` vectors is restored", {
	pt1 = st_sfc(st_point(c(NA_real_, NA_real_)))
	pt2 = st_sfc(st_point(0:1))

	x = c(pt1, pt2)
	expect_identical(attr(vctrs::vec_slice(x, 1), "n_empty"), 1L)
	expect_identical(attr(vctrs::vec_slice(x, 2), "n_empty"), 0L)

	combined = vctrs::vec_c(pt1, pt2, pt1)
	expect_length(combined, 3)
	expect_identical(attr(combined, "n_empty"), 2L)
})

test_that("bbox attributes of `sfc` vectors are restored", {
	pt1 = st_sfc(st_point(c(1L, 2L)))
	pt2 = st_sfc(st_point(c(10L, 20L)))

	x = c(pt1, pt2)
	expect_identical(st_bbox(vctrs::vec_slice(x, 1)), st_bbox(pt1))
	expect_identical(st_bbox(vctrs::vec_slice(x, 2)), st_bbox(pt2))

	combined = vctrs::vec_c(pt1, pt2)
	expect_identical(st_bbox(x), st_bbox(combined))
})

test_that("`precision` and `crs` attributes of `sfc` vectors are restored", {
	x = st_sfc(st_point(c(pi, pi)), precision = 1e-4, crs = 3857)
	out = vctrs::vec_slice(x, 1)
	expect_identical(st_precision(x), st_precision(out))
	expect_identical(st_crs(x), st_crs(out))
})

test_that("`precision` and `crs` attributes of `sfc` vectors are combined", {
	x = st_sfc(st_point(c(pi, pi)), precision = 1e-4, crs = 3857)
	y = st_sfc(st_point(c(0, 0)), precision = 1e-4, crs = 3857)

	out = vctrs::vec_c(x, y)
	expect_identical(st_precision(x), st_precision(out))
	expect_identical(st_crs(x), st_crs(out))

	y = st_sfc(st_point(c(0, 0)), precision = 1e-2, crs = 3857)
	expect_error(vctrs::vec_c(x, y), "precisions not equal")

	y = st_sfc(st_point(c(0, 0)), precision = 1e-4, crs = 4326)
	expect_error(vctrs::vec_c(x, y), "coordinate reference systems not equal")
})

test_that("`sfc` vectors have a common type", {
	pt = st_sfc(st_point())
	ln = st_sfc(st_linestring())
	expect_identical(class(vctrs::vec_ptype2(pt, pt)), c("sfc_POINT", "sfc"))
	expect_identical(class(vctrs::vec_ptype2(ln, ln)), c("sfc_LINESTRING", "sfc"))
	expect_identical(class(vctrs::vec_ptype2(pt, ln)), c("sfc_GEOMETRY", "sfc"))
})
