# Expect the z range, strip attributes to only compare values.
expect_st_z_range <- function(object, expected) {
	expect_equal(unclass(st_z_range(object)), expected, check.attributes = FALSE)
}

test_that("st_z_range and st_z_range returns correct value from sfg objects", {
	
	pt <- st_point(x = c(0,1,3,3))
	
	expect_st_z_range(pt, c(3,3))
	expect_equal(st_z_range(pt), st_z_range(pt))

	mp <- st_multipoint(x = matrix(c(0,1,1,1,0,2,5,5), ncol = 4, byrow = TRUE))
	expect_st_z_range(mp, c(1, 5))
	expect_equal(st_z_range(mp), st_z_range(mp))

	ls <- st_linestring(x = matrix(c(0,1,1,1,0,2,5,5,0,3,10,10), ncol = 4, byrow = TRUE))
	expect_st_z_range(ls, c(1, 10))
	expect_equal(st_z_range(ls), st_z_range(ls))

	mls <- st_multilinestring(x = list(ls, matrix(c(0,1,5,5,0,1,-1,-1), ncol = 4, byrow = TRUE)))
	expect_st_z_range(mls, c(-1, 10))
	expect_equal(st_z_range(mls), st_z_range(mls))

	pl <- st_polygon(x = list(matrix(c(0,0,1,1,0,1,2,2,1,1,3,3,1,0,4,4,0,0,1,1), ncol = 4, byrow = T)))
	expect_st_z_range(pl, c(1, 4))
	expect_equal(st_z_range(pl), st_z_range(pl))

	mpl <- st_multipolygon(
		x = list(pl, st_polygon(
			x = list(matrix(c(0,0,10,10,0,-1,9,9,-1,-1,-10,-10,-1,0,-5,-5,0,0,10,10),
							 ncol = 4, byrow = TRUE))))
		)
	expect_st_z_range(mpl, c(-10, 10))
	expect_equal(st_z_range(mpl), st_z_range(mpl))

	gc <- st_geometrycollection(x = list(pt, mp))
	expect_st_z_range(gc, c(1, 5))
	expect_equal(st_z_range(gc), st_z_range(gc))

	gc <- st_geometrycollection(x = list(ls, pl))
	expect_st_z_range(gc, c(1, 10))
	expect_equal(st_z_range(gc), st_z_range(gc))

	gc <- st_geometrycollection(x = list(pt, mpl))
	expect_st_z_range(gc, c(-10, 10))
	expect_equal(st_z_range(gc), st_z_range(gc))
})


test_that("sf::st_z_range and sf::st_z_range returns correct value from sfc objects", {
	pt <- st_sfc(st_point( x = c(0,1,3,3)))
	# expect_equal(attr( pt, "zbox" ), c(3, 3)) # FIXME: now NULL
	expect_st_z_range(pt, c(3, 3))
	expect_equal(st_z_range(pt), st_z_range(pt))

	mp <- st_sfc(st_multipoint( x = matrix(c(0,1,1,1,0,2,5,5), ncol = 4, byrow = TRUE)))
	expect_st_z_range(mp, c(1, 5))
	expect_equal(st_z_range(mp), st_z_range(mp))

	ls <- st_sfc(st_linestring(x = matrix(c(0,1,1,1,0,2,5,5,0,3,10,10), ncol = 4, byrow = TRUE)))
	expect_st_z_range(ls, c(1, 10))
	expect_equal(st_z_range(ls), st_z_range(ls))

	mls <- st_sfc(st_multilinestring(x = list(ls[[1]], matrix(c(0,1,5,5,0,1,-1,-1), ncol = 4, byrow = TRUE))))
	expect_st_z_range(mls, c(-1, 10))
	expect_equal(st_z_range(mls), st_z_range(mls))

	pl <- st_sfc(st_polygon(x = list(matrix(c(0,0,1,1,0,1,2,2,1,1,3,3,1,0,4,4,0,0,1,1), ncol = 4, byrow = TRUE))))
	expect_st_z_range(pl, c(1, 4))
	expect_equal(st_z_range(pl), st_z_range(pl))

	mpl <- st_sfc(st_multipolygon(x = list(pl[[1]], st_polygon( x = list( matrix(c(0,0,10,10,0,-1,9,9,-1,-1,-10,-10,-1,0,-5,-5,0,0,10,10), ncol = 4, byrow = TRUE))))))
	expect_st_z_range(mpl, c(-10, 10))
	expect_equal(st_z_range(mpl), st_z_range(mpl))

	gc <- st_sfc(st_geometrycollection(x = list(pt[[1]], mp[[1]])))
	expect_st_z_range(gc, c(1, 5))
	expect_equal(st_z_range(gc), st_z_range(gc))

	gc <- st_sfc(st_geometrycollection(x = list(ls[[1]], pl[[1]])))
	expect_st_z_range(gc, c(1, 10))
	expect_equal(st_z_range(gc), st_z_range(gc))

	gc <- st_sfc(st_geometrycollection(x = list(pt[[1]], mpl[[1]])))
	expect_st_z_range(gc, c(-10, 10))
	expect_equal(st_z_range(gc), st_z_range(gc))

})

test_that("zmrange works on more compliated examples", {

	set.seed(123)
	m <- matrix(rnorm(300), ncol = 3)
	expected <- c(min(m[,3]), max(m[,3]))

	ls <- st_linestring(x = m)
	expect_st_z_range(ls, expected)

	ls <- st_sfc(ls)
	expect_st_z_range(ls, expected)
	expect_equal(
		unclass(attr(ls, "z_range")),
		expected,
		check.attributes = FALSE
	)

	ls <- st_sf(geometry = ls)
	expect_st_z_range(ls, expected)
	expect_equal(
		unclass(attr(ls$geometry, "z_range")),
		expected,
		check.attributes = FALSE
	)
    n <- 100
    lst <- list()
    min_z <- numeric(n)
    max_z <- numeric(n)

    set.seed(123)

    for(i in seq_along(n)) {
    	m <- matrix(rnorm(sample(seq(3,300, by = 3), size = 1)), ncol = 3)
    	min_z[i] <- min(m[,3])
    	max_z[i] <- max(m[,3])
    	lst[[i]] <- st_linestring(m)
    }

    sfc <- st_sfc(lst)
    expect_st_z_range(sfc, c(min(min_z), max(max_z)))

})

test_that("transform includes zm in output", {

	skip_if(sf_extSoftVersion()[["GDAL"]] <= "2.1.0")

	p1 = st_point(c(7,52,52))
	p2 = st_point(c(-30,20,20))
	sfc = st_sfc(p1, p2, crs = 4326)

	res <- st_transform(sfc, 3857)
	expect_contains(names(attributes(res)), "z_range")
	expect_equal(st_z_range(res[[1]]), st_z_range(sfc[[1]]))

	p1 = st_point(c(7,52,52,7))
	p2 = st_point(c(-30,20,20,-30))
	sfc = st_sfc(p1, p2, crs = 4326)

	res <- st_transform(sfc, 3857)
	expect_contains(names(attributes(res)), c("z_range", "m_range"))
	expect_equal(st_z_range(res[[1]]), st_z_range(sfc[[1]]))
	expect_equal(st_m_range(res[[1]]), st_m_range(sfc[[1]]))

})


test_that("XYM-only objects correctly calculate M (and not Z)", {
	skip_if(sf_extSoftVersion()[["GDAL"]] <= "2.1.0")

	sf_m <- st_read(system.file("/shape/storms_xyzm.shp", package = "sf"), quiet = TRUE)
	m <- st_coordinates(sf_m)

	mmin <- min(m[, 3])
	mmax <- max(m[, 3])
	expect_equal(unclass(st_m_range(sf_m)), c(mmin, mmax), check.attributes = FALSE)

	sf_z <- st_read(system.file("/shape/storms_xyz.shp", package = "sf"), quiet = TRUE)

	expect_equal(
		unclass(st_m_range(sf_m)),
		unclass(st_z_range(sf_z)),
		check.attributes = FALSE
	)

	expect_null(st_z_range(sf_m))
	expect_null(st_m_range(sf_z))
})
