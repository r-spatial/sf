context("sf: st_collectionextract")

pt <- st_point(c(1, 0))
ls <- st_linestring(matrix(c(4, 3, 0, 0), ncol = 2))
poly1 <- st_polygon(list(matrix(c(5.5, 7, 7, 6, 5.5, 0, 0, -0.5, -0.5, 0), ncol = 2)))
poly2 <- st_polygon(list(matrix(c(6.6, 8, 8, 7, 6.6, 1, 1, 1.5, 1.5, 1), ncol = 2)))
multipoly <- st_multipolygon(list(poly1, poly2))

i <- st_geometrycollection(list(pt, ls, poly1, multipoly))

j <- st_geometrycollection(list(pt, ls, poly1, poly2, multipoly))

## A GEOMETRYCOLLECTION
aa <- rbind(st_sf(a=1, geom = st_sfc(i)),
			st_sf(a=2, geom = st_sfc(j)))

##
## A GEOMETRY of single types
bb <- rbind(
	st_sf(a = 1, geom = st_sfc(pt)),
	st_sf(a = 2, geom = st_sfc(ls)),
	st_sf(a = 3, geom = st_sfc(poly1)),
	st_sf(a = 4, geom = st_sfc(multipoly))
)

## A GEOMETRY of mixed single types and GEOMETRYCOLLECTIONS
cc <- rbind(aa, bb)

test_that("st_collectionextract works with sfg objects", {
	st_collectionextract(i, "POLYGON")
	st_collectionextract(j, "POLYGON")
	st_collectionextract(i, "POINT")
	st_collectionextract(i, "LINESTRING")
})

test_that("st_collectionextract works with sfc objects", {
	st_collectionextract(st_geometry(aa), "POLYGON")
	st_collectionextract(st_geometry(aa), "LINESTRING")
	st_collectionextract(st_geometry(aa), "POINT")
	st_collectionextract(st_geometry(bb), "POINT")
	st_collectionextract(st_geometry(cc), "POLYGON")
})


test_that("st_collectionextract works with sf objects", {
	st_collectionextract(aa, "POLYGON")
	st_collectionextract(aa, "LINESTRING")
	st_collectionextract(aa, "POINT")
	st_collectionextract(bb, "POLYGON")
	st_collectionextract(cc, "POLYGON")
})

test_that("st_collectionextract behaves with unexpected inputs", {
	expect_warning(st_collectionextract(poly1, "POLYGON"))
	## Returns empty geometry
	st_collectionextract(st_geometrycollection(list(pt, ls)), "POLYGON")
})

