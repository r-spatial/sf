context("sf: vctrs implementations")

test_that("`sfc` vectors are treated as vectors", {
	expect_true(vctrs::vec_is(st_sfc(st_point())))
})

test_that("`n_empty` attribute of `sfc` vectors is restored", {
	x <- st_sfc(
		st_point(c(NA_real_, NA_real_)),
		st_point(0:1)
	)
	expect_identical(attr(vctrs::vec_slice(x, 1), "n_empty"), 1L)
	expect_identical(attr(vctrs::vec_slice(x, 2), "n_empty"), 0L)
})

test_that("`precision` and `crs` attributes of `sfc` vectors are restored", {
	x <- st_sfc(st_point(c(pi, pi)), precision = 1e-4, crs = 3857)
	out <- vctrs::vec_slice(x, 1)
	expect_identical(st_precision(x), st_precision(out))
	expect_identical(st_crs(x), st_crs(out))
})

test_that("`sfc` vectors have a common type", {
	pt <- st_sfc(st_point())
	ln <- st_sfc(st_linestring())
	expect_identical(class(vctrs::vec_ptype2(pt, pt)), c("sfc_POINT", "sfc"))
	expect_identical(class(vctrs::vec_ptype2(ln, ln)), c("sfc_LINESTRING", "sfc"))
	expect_identical(class(vctrs::vec_ptype2(pt, ln)), c("sfc_GEOMETRY", "sfc"))
})
