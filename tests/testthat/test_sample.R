test_that("st_sample works", {
        # local modifications: jarodmeng@
        # skip this test because lwgeom package is not available.
        testthat::skip_if_not_installed("lwgeom")
	nc = read_sf(system.file("shape/nc.shp", package="sf"))
	n = 100
	sample_default = st_sample(x = nc, size = n)
	expect_s3_class(sample_default, "sfc")
	sample_exact = st_sample(x = nc, size = n, exact = TRUE)
	expect_equal(length(sample_exact), n)
})
