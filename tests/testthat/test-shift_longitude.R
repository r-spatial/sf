context("st_shift_longitude")

test_that("st_shift_longitude", {
	pt1 = st_point(c(-170, 50))
	pt2 = st_point(c(170, 50))
	(sfc = st_sfc(pt1, pt2))
	sfc = st_set_crs(sfc, 4326)

	crd1 = st_coordinates(st_shift_longitude(sfc))[1, "X"]
	crd2 = st_coordinates(st_shift_longitude(sfc))[2, "X"]

	# sfc
	## pt1 should be shifted but pt2 should not
	expect_equal(crd1, st_coordinates(sfc)[1, "X"] + 360)
	expect_equal(crd2, st_coordinates(sfc)[2, "X"])

	# sf
	d = st_as_sf(data.frame(id = 1:2, geometry = sfc))

	## same as above
	expect_equal(crd1, st_coordinates(st_shift_longitude(d))[1, "X"])
	expect_equal(crd2, st_coordinates(d)[2, "X"])

	# non-projected crs
	## NA
	d = st_set_crs(d, NA)
	expect_error(st_shift_longitude(d))

	## arbitrary crs
	sfc = st_transform(sfc, 3035)
	expect_error(st_shift_longitude(sfc))
})

