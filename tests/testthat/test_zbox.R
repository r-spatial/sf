## tests:
## zbox is transformed under st_transform / st_crs

## needs to return nice error if no zbox exists
a <- st_sf(a = 1:2, geom = st_sfc(st_point(0:1), st_point(1:2)), crs = 4326)
sf::st_z_range( a )

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

