## tests:
## zbox is transformed under st_transform / st_crs
## zbox is deleted uner st_zm
## zbox is printed
## zbox is attached as an attribute
## zbox is calculated correctly
## zbox not attached if dim is 'XY'
## zbox is attached when using sf::st_sfc()

## needs to return nice error if no zbox exists
a <- st_sf(a = 1:2, geom = st_sfc(st_point(0:1), st_point(1:2)), crs = 4326)
sf::st_zbox( a )

test_that("sf::st_zbox returns correct value from sfg objects", {

	pt <- sf::st_point( x = c(0,1,3))
	expect_true( all( sf::st_zbox( pt ) == c(3,3) ) )

	mp <- sf::st_multipoint( x = matrix(c(0,1,1,0,2,5), ncol = 3, byrow = T))
	expect_true( all( sf::st_zbox( mp ) == c(1,5) ) )

	ls <- sf::st_linestring(x = matrix(c(0,1,1,0,2,5,0,3,10), ncol = 3, byrow = T))
	expect_true( all( sf::st_zbox( ls ) == c(1,10) ) )

	mls <- sf::st_multilinestring(x = list(ls, matrix(c(0,1,5,0,1,-1), ncol = 3, byrow = T)))
	expect_true( all( sf::st_zbox( mls ) == c(-1, 10 ) ) )

	pl <- sf::st_polygon(x = list(matrix(c(0,0,1,0,1,2,1,1,3,1,0,4,0,0,1), ncol = 3, byrow = T)))
	expect_true( all( sf::st_zbox( pl ) == c(1, 4)))

	mpl <- sf::st_multipolygon(x = list(pl, sf::st_polygon( x = list( matrix(c(0,0,10,0,-1,9,-1,-1,-10,-1,0,-5,0,0,10), ncol = 3, byrow = T) ) ) ) )
	expect_true( all( sf::st_zbox( mpl ) == c(-10, 10) ) )

	gc <- sf::st_geometrycollection(x = list(pt, mp))
	expect_true( all( sf::st_zbox( gc ) == c(1, 5) ) )

	gc <- sf::st_geometrycollection(x = list(ls, pl))
	expect_true( all( sf::st_zbox( gc ) == c(1, 10) ) )

	gc <- sf::st_geometrycollection(x = list(pt, mpl))
	expect_true( all( sf::st_zbox( gc ) == c(-10, 10) ) )

})


test_that("sf::st_zbox returns correct value from sfc objects", {

	pt <- sf::st_sfc( sf::st_point( x = c(0,1,3) ) )
	expect_true( all( sf::st_zbox( pt ) == c(3,3) ) )

	mp <- sf::st_sfc(  sf::st_multipoint( x = matrix(c(0,1,1,0,2,5), ncol = 3, byrow = T)) )
	expect_true( all( sf::st_zbox( mp ) == c(1,5) ) )

	ls <- sf::st_sfc( sf::st_linestring(x = matrix(c(0,1,1,0,2,5,0,3,10), ncol = 3, byrow = T)) )
	expect_true( all( sf::st_zbox( ls ) == c(1,10) ) )

	mls <- sf::st_sfc( sf::st_multilinestring(x = list(ls[[1]] , matrix(c(0,1,5,0,1,-1), ncol = 3, byrow = T))) )
	expect_true( all( sf::st_zbox( mls ) == c(-1, 10 ) ) )

	pl <- sf::st_sfc( sf::st_polygon(x = list(matrix(c(0,0,1,0,1,2,1,1,3,1,0,4,0,0,1), ncol = 3, byrow = T))) )
	expect_true( all( sf::st_zbox( pl ) == c(1, 4)))

	mpl <- sf::st_sfc( sf::st_multipolygon(x = list(pl[[1]], sf::st_polygon( x = list( matrix(c(0,0,10,0,-1,9,-1,-1,-10,-1,0,-5,0,0,10), ncol = 3, byrow = T) ) ) ) ) )
	expect_true( all( sf::st_zbox( mpl ) == c(-10, 10) ) )

	gc <- sf::st_sfc( sf::st_geometrycollection(x = list(pt[[1]], mp[[1]])) )
	expect_true( all( sf::st_zbox( gc ) == c(1, 5) ) )

	gc <- sf::st_sfc( sf::st_geometrycollection(x = list(ls[[1]], pl[[1]])) )
	expect_true( all( sf::st_zbox( gc ) == c(1, 10) ) )

	gc <- sf::st_sfc( sf::st_geometrycollection(x = list(pt[[1]], mpl[[1]])) )
	expect_true( all( sf::st_zbox( gc ) == c(-10, 10) ) )

})
