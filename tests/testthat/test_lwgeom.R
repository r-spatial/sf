context("sf: lwgeom")

test_that("st_make_valid works", {
	x = st_sfc(st_polygon(list(rbind(c(0,0),c(0.5,0),c(0.5,0.5),c(0.5,0),c(1,0),c(1,1),c(0,1),c(0,0)))))
	fls = suppressWarnings(st_is_valid(x, FALSE))
	expect_false(fls)
	if (!is.na(sf_extSoftVersion()['lwgeom'])) {
    	y = st_make_valid(x)
		expect_true(st_is_valid(y))
    	expect_true(st_is_valid(st_make_valid(x[[1]])))
    	expect_true(st_is_valid(st_make_valid(st_sf(a = 1, geom = x))))
		expect_equal(st_geohash(st_sfc(st_point(c(1.5,3.5)), st_point(c(0,90))), 2), c( "s0","up"))
		expect_equal(st_geohash(st_sfc(st_point(c(1.5,3.5)), st_point(c(0,90))), 10), c("s095fjhkbx","upbpbpbpbp"))
	}
})

