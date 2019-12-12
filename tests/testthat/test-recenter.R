context("st_recenter")

test_that("st_recenter", {
	pt1 = st_point(c(-170, 50))
	pt2 = st_point(c(170, 50))
	(sfc = st_sfc(pt1, pt2))
	sfc = st_set_crs(sfc, 4326)

	jnk = st_coordinates(st_recenter(sfc))[1, "X"]

	expect_equal(jnk, st_coordinates(sfc)[1, "X"] + 360)

	d = st_as_sf(data.frame(id = 1:2, geometry = sfc))

	expect_equal(jnk, st_coordinates(st_recenter(d))[1, "X"])
})

