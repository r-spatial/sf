context("sf: attribute selection")

nc <- st_read(system.file("shape/nc.shp", package = "sf"))

test_that("st_attr works", {
	# Works without messages, errors, warnings, etc.
	expect_silent(x <- st_attr(nc))
	# Returns a data.frame
	expect_is(x, "data.frame")
	# Drop only the sf_column, keep all features
	expect_true(is.null(attr(x, "sf_column")))
	expect_true(nrow(nc) == nrow(x))
	expect_true(ncol(nc) == (ncol(x) + 1))
	# Equivalence to setting sf object geometry to NULL
	st_geometry(nc) <- NULL
	expect_identical(x, nc)
	# Throw errors for non-sf objects
	foo <- data.frame(a = 1:2, b = letters[1:2])
	expect_error(st_attr(foo))
})

