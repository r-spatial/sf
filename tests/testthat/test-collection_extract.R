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

test_that("empty collections do not discard nonempty components (#2367)", {
  collection = st_geometrycollection(list(pt, ls, poly1, poly2))
  x = st_sfc(st_geometrycollection(), collection, st_geometrycollection(),
    st_geometrycollection(list(pt)), crs = 4326)
  features = st_sf(id = c("empty-before", "mixed", "empty-between", "point"), geom = x)

  for (type in c("POINT", "LINESTRING", "POLYGON")) {
    expected = st_collection_extract(x[c(2, 4)], type)
    actual = st_collection_extract(x, type)
    expect_equal(actual, expected)
    expect_equal(st_crs(actual), st_crs(x))
    expect_equal(attr(actual, "n_empty"), 0L)
    result = st_collection_extract(features, type)
    expect_equal(st_geometry(result), actual)
    expect_equal(result$id, switch(type, POINT = c("mixed", "point"),
      LINESTRING = "mixed", POLYGON = c("mixed", "mixed")))
  }

  empty = x[c(1, 3)]
  expect_warning(empty_result <- st_collection_extract(empty, "POLYGON"),
    "x contains no geometries of specified type")
  expect_length(empty_result, 0L)
  expect_s3_class(empty_result, "sfc_GEOMETRYCOLLECTION")
  expect_equal(st_crs(empty_result), st_crs(x))
  expect_warning(no_match <- st_collection_extract(x[c(1, 4)], "POLYGON"),
    "x contains no geometries of specified type")
  expect_length(no_match, 0L)
  expect_equal(st_crs(no_match), st_crs(x))
})

test_that("collection extraction preserves names and fields after empty parents", {
  for (count in 1:2) {
    x = st_sfc(empty = st_geometrycollection(),
      polygons = st_geometrycollection(rep(list(poly1), count)), crs = 4326)
    expected = st_sfc(rep(list(poly1), count), crs = 4326)
    names(expected) = rep("polygons", count)
    result = st_collection_extract(x, "POLYGON")
    expect_equal(result, expected)
    expect_identical(names(result), rep("polygons", count))

    features = st_sf(id = c("empty", "live"), value = c(10L, 20L), geom = x)
    extracted = st_collection_extract(features, "POLYGON")
    expect_equal(st_geometry(extracted), expected)
    expect_identical(extracted$id, rep("live", count))
    expect_identical(extracted$value, rep(20L, count))
  }
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

