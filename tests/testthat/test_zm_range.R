
test_that("sf::st_z_range and sf::st_z_range returns correct value from sfg objects", {

	pt <- sf::st_point( x = c(0,1,3,3))
	expect_true( all( sf::st_z_range( pt ) == c(3,3) ) )
	expect_true( all( sf::st_z_range( pt ) == sf::st_z_range( pt ) ) )

	mp <- sf::st_multipoint( x = matrix(c(0,1,1,1,0,2,5,5), ncol = 4, byrow = T))
	expect_true( all( sf::st_z_range( mp ) == c(1,5) ) )
	expect_true( all( sf::st_z_range( mp ) == sf::st_z_range( mp ) ) )

	ls <- sf::st_linestring(x = matrix(c(0,1,1,1,0,2,5,5,0,3,10,10), ncol = 4, byrow = T))
	expect_true( all( sf::st_z_range( ls ) == c(1,10) ) )
	expect_true( all( sf::st_z_range( ls ) == sf::st_z_range( ls ) ) )

	mls <- sf::st_multilinestring(x = list(ls, matrix(c(0,1,5,5,0,1,-1,-1), ncol = 4, byrow = T)))
	expect_true( all( sf::st_z_range( mls ) == c(-1, 10 ) ) )
	expect_true( all( sf::st_z_range( mls ) == sf::st_z_range( mls ) ) )

	pl <- sf::st_polygon(x = list(matrix(c(0,0,1,1,0,1,2,2,1,1,3,3,1,0,4,4,0,0,1,1), ncol = 4, byrow = T)))
	expect_true( all( sf::st_z_range( pl ) == c(1, 4)))
	expect_true( all( sf::st_z_range( pl ) == sf::st_z_range( pl ) ) )

	mpl <- sf::st_multipolygon(x = list(pl, sf::st_polygon( x = list( matrix(c(0,0,10,10,0,-1,9,9,-1,-1,-10,-10,-1,0,-5,-5,0,0,10,10), ncol = 4, byrow = T) ) ) ) )
	expect_true( all( sf::st_z_range( mpl ) == c(-10, 10) ) )
	expect_true( all( sf::st_z_range( mpl ) == sf::st_z_range( mpl ) ) )

	gc <- sf::st_geometrycollection(x = list(pt, mp))
	expect_true( all( sf::st_z_range( gc ) == c(1, 5) ) )
	expect_true( all( sf::st_z_range( gc ) == sf::st_z_range( gc ) ) )

	gc <- sf::st_geometrycollection(x = list(ls, pl))
	expect_true( all( sf::st_z_range( gc ) == c(1, 10) ) )
	expect_true( all( sf::st_z_range( gc ) == sf::st_z_range( gc ) ) )

	gc <- sf::st_geometrycollection(x = list(pt, mpl))
	expect_true( all( sf::st_z_range( gc ) == c(-10, 10) ) )
	expect_true( all( sf::st_z_range( gc ) == sf::st_z_range( gc ) ) )

})


test_that("sf::st_z_range and sf::st_z_range returns correct value from sfc objects", {

	pt <- sf::st_sfc( sf::st_point( x = c(0,1,3,3)))
	expect_true( all( attr( pt, "zbox" ) == c(3,3) ) )
	expect_true( all( sf::st_z_range( pt ) == c(3,3) ) )
	expect_true( all( sf::st_z_range( pt ) == sf::st_z_range( pt ) ) )

	mp <- sf::st_sfc( sf::st_multipoint( x = matrix(c(0,1,1,1,0,2,5,5), ncol = 4, byrow = T)))
	expect_true( all( sf::st_z_range( mp ) == c(1,5) ) )
	expect_true( all( sf::st_z_range( mp ) == sf::st_z_range( mp ) ) )

	ls <- sf::st_sfc( sf::st_linestring(x = matrix(c(0,1,1,1,0,2,5,5,0,3,10,10), ncol = 4, byrow = T)))
	expect_true( all( sf::st_z_range( ls ) == c(1,10) ) )
	expect_true( all( sf::st_z_range( ls ) == sf::st_z_range( ls ) ) )

	mls <- sf::st_sfc( sf::st_multilinestring(x = list(ls[[1]], matrix(c(0,1,5,5,0,1,-1,-1), ncol = 4, byrow = T))))
	expect_true( all( sf::st_z_range( mls ) == c(-1, 10 ) ) )
	expect_true( all( sf::st_z_range( mls ) == sf::st_z_range( mls ) ) )

	pl <- sf::st_sfc( sf::st_polygon(x = list(matrix(c(0,0,1,1,0,1,2,2,1,1,3,3,1,0,4,4,0,0,1,1), ncol = 4, byrow = T))))
	expect_true( all( sf::st_z_range( pl ) == c(1, 4)))
	expect_true( all( sf::st_z_range( pl ) == sf::st_z_range( pl ) ) )

	mpl <- sf::st_sfc( sf::st_multipolygon(x = list(pl[[1]], sf::st_polygon( x = list( matrix(c(0,0,10,10,0,-1,9,9,-1,-1,-10,-10,-1,0,-5,-5,0,0,10,10), ncol = 4, byrow = T) ) ) ) ))
	expect_true( all( sf::st_z_range( mpl ) == c(-10, 10) ) )
	expect_true( all( sf::st_z_range( mpl ) == sf::st_z_range( mpl ) ) )

	gc <- sf::st_sfc( sf::st_geometrycollection(x = list(pt[[1]], mp[[1]])))
	expect_true( all( sf::st_z_range( gc ) == c(1, 5) ) )
	expect_true( all( sf::st_z_range( gc ) == sf::st_z_range( gc ) ) )

	gc <- sf::st_sfc( sf::st_geometrycollection(x = list(ls[[1]], pl[[1]])))
	expect_true( all( sf::st_z_range( gc ) == c(1, 10) ) )
	expect_true( all( sf::st_z_range( gc ) == sf::st_z_range( gc ) ) )

	gc <- sf::st_sfc( sf::st_geometrycollection(x = list(pt[[1]], mpl[[1]])))
	expect_true( all( sf::st_z_range( gc ) == c(-10, 10) ) )
	expect_true( all( sf::st_z_range( gc ) == sf::st_z_range( gc ) ) )

})

test_that("zmrange works on more compliated examples", {

	set.seed(123)
	m <- matrix(rnorm(300), ncol = 3)
	expected <- c(min(m[,3]), max(m[,3]))

	ls <- sf::st_linestring(x = m )
	expect_true( all( sf::st_z_range(ls) == expected ) )

	ls <- sf::st_sfc( ls )
	expect_true( all( sf::st_z_range(ls) == expected ) )
	expect_true( all( attr(ls, "z_range") == expected ) )

	ls <- sf::st_sf( geometry = ls )
	expect_true( all( sf::st_z_range(ls) == expected ) )
    expect_true( all( attr(ls$geometry, "z_range") == expected ) )

    n <- 100
    lst <- list()
    min_z <- numeric(n)
    max_z <- numeric(n)

    set.seed(123)

    for(i in 1:n) {
    	m <- matrix(rnorm(sample(seq(3,300,by=3), size = 1)), ncol = 3)
    	min_z[i] <- min(m[,3])
    	max_z[i] <- max(m[,3])
    	lst[[i]] <- sf::st_linestring( m )
    }

    sfc <- sf::st_sfc( lst )

    expect_true( all (sf::st_z_range( sfc ) == c(min(min_z), max(max_z)) ) )

})

test_that("transform includes zm in output", {

	skip_if_not(sf_extSoftVersion()["GDAL"] > "2.1.0")

	p1 = st_point(c(7,52,52))
	p2 = st_point(c(-30,20,20))
	sfc = st_sfc(p1, p2, crs = 4326)

	res <- st_transform(sfc, 3857)
	expect_true( "z_range" %in% names( attributes(res) ) )
	expect_equal( sf::st_z_range(res[[1]]), sf::st_z_range(sfc[[1]]) )

	p1 = st_point(c(7,52,52,7))
	p2 = st_point(c(-30,20,20,-30))
	sfc = st_sfc(p1, p2, crs = 4326)

	res <- st_transform(sfc, 3857)
	expect_true( "z_range" %in% names( attributes(res) ) )
	expect_equal( sf::st_z_range(res[[1]]), sf::st_z_range(sfc[[1]]) )
	expect_true( "m_range" %in% names( attributes(res) ) )
	expect_equal( sf::st_m_range(res[[1]]), sf::st_m_range(sfc[[1]]) )

})


test_that("XYM-only objects correctly calculate M (and not Z)", {
	skip_if_not(sf_extSoftVersion()["GDAL"] > "2.1.0")

	sf_m <- sf::st_read(system.file("/shape/storms_xyzm.shp", package = "sf"), quiet = TRUE)
	m <- sf::st_coordinates( sf_m )

	mmin <- min( m[, 3] ); mmax <- max( m[, 3] )
	expect_true( all( sf::st_m_range( sf_m ) == c(mmin, mmax) ) )

	sf_z <- sf::st_read(system.file("/shape/storms_xyz.shp", package = "sf"), quiet = TRUE)

	expect_true( all( sf::st_m_range( sf_m ) == sf::st_z_range( sf_z ) ) )

	expect_null(sf::st_z_range(sf_m))
	expect_null(sf::st_m_range(sf_z))
})
