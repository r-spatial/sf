pt <- st_point(c(1, 0))
ls <- st_linestring(matrix(c(4, 3, 0, 0), ncol = 2))
poly1 <- st_polygon(list(matrix(c(5.5, 7, 7, 6, 5.5, 0, 0, -0.5, -0.5, 0), ncol = 2)))
poly2 <- st_polygon(list(matrix(c(6.6, 8, 8, 7, 6.6, 1, 1, 1.5, 1.5, 1), ncol = 2)))
multipoly <- st_multipolygon(list(poly1, poly2))

i <- st_geometrycollection(list(pt, ls, poly1, poly2))

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

test_that("st_collection_extract works with sfg objects", {
	expect_s3_class(st_collection_extract(i, "POLYGON"), "sfc_POLYGON")
	expect_s3_class(st_collection_extract(j, "POLYGON"), "sfc_MULTIPOLYGON")
	expect_s3_class(st_collection_extract(i, "POINT"), "POINT")
	expect_s3_class(st_collection_extract(i, "LINESTRING"), "LINESTRING")
})

test_that("st_collection_extract works with sfc objects", {
	expect_s3_class(st_collection_extract(st_geometry(aa), "POLYGON"), "sfc_MULTIPOLYGON")
	expect_s3_class(st_collection_extract(st_geometry(aa), "LINESTRING"), "sfc_LINESTRING")
	expect_s3_class(st_collection_extract(st_geometry(aa), "POINT"), "sfc_POINT")
	expect_s3_class(st_collection_extract(st_geometry(bb), "POINT"), "sfc_POINT")
	expect_s3_class(st_collection_extract(st_geometry(cc), "POLYGON"), "sfc_MULTIPOLYGON")
})


test_that("st_collection_extract works with sf objects", {
	expect_s3_class(st_geometry(st_collection_extract(aa, "POLYGON")), "sfc_MULTIPOLYGON")
	expect_s3_class(st_geometry(st_collection_extract(aa, "LINESTRING")), "sfc_LINESTRING")
	expect_s3_class(st_geometry(st_collection_extract(aa, "POINT")), "sfc_POINT")
	expect_s3_class(st_geometry(st_collection_extract(bb, "POLYGON")), "sfc_MULTIPOLYGON")
	expect_s3_class(st_geometry(st_collection_extract(cc, "POLYGON")), "sfc_MULTIPOLYGON")
})

test_that("st_collection_extract behaves with unexpected inputs", {
	expect_warning(st_collection_extract(poly1, "POLYGON"),
				   "x is already of type POLYGON")
	expect_error(st_collection_extract(st_sfc(pt), "POLYGON"),
				 "x is of singular geometry type that is different to supplied type")
	expect_error(st_collection_extract(st_sf(a = "a", geom = st_sfc(pt)), "POLYGON"),
				 "x is of singular geometry type that is different to supplied type")
	## Returns empty geometry
	expect_warning(st_collection_extract(st_sfc(pt, ls), "POLYGON"),
				   "x contains no geometries of specified type")
	expect_warning(st_collection_extract(st_sf(a = c("a", "b"), geom = st_sfc(ls, pt)),
										"POLYGON"),
				   "x contains no geometries of specified type")
	expect_warning(zero_len <- st_collection_extract(st_geometrycollection(list(pt, ls)), "POLYGON"),
	"x contains no geometries of specified type")
	expect_length(zero_len, 0L)
	expect_s3_class(zero_len, "sfg")
	expect_true(st_is(zero_len, "POLYGON"))
})

