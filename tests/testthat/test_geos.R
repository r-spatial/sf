context("sf: geos tests")

test_that("CPL_goes_relate works", {
  r1 = sf:::CPL_geos_relate(st_sfc(st_point(c(0,0))), st_sfc(st_linestring(rbind(c(0,0),c(1,1)))))
  library(sp)
  p = SpatialPoints(matrix(0,1,2))
  l = Lines(list(Line(rbind(c(0,0),c(1,1)))), "ID")
  sl = SpatialLines(list(l))
  library(rgeos)
  r2 = gRelate(p, sl)
  expect_true(all(r1 == r2))
})

test_that("CPL_geos_is_valid works", {
  expect_true( sf:::CPL_geos_is_valid(
  	st_sfc(st_polygon(list(cbind(c(0,1,1,0,0), c(0,0,1, 1,0)))))))
  expect_warning(
    expect_false(sf:::CPL_geos_is_valid(
  	st_sfc(st_polygon(list(cbind(c(0,1,1,.5,0),c(0,0,1,-1,0)))))))
    )
})

test_that("geos ops give warnings and errors on longlat", {
	nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
	x = nc[1:2,]
	y = nc[2:3,]
	expect_silent(st_equals(x, y))
	expect_silent(st_equals_exact(x, y, 0.01))
	l = st_sfc(st_linestring(matrix(1:10, ncol=2)), crs = st_crs(nc))
	expect_silent(st_polygonize(l))

	expect_message(st_intersects(x,y))
	expect_message(st_disjoint(x,y))
	expect_message(st_touches(x,y))
	expect_message(st_crosses(x,y))
	expect_message(st_within(x,y))
	expect_message(st_contains(x,y))
	expect_message(st_overlaps (x,y))
	expect_message(st_covers(x,y))
	expect_message(st_covered_by(x,y))

	expect_warning(st_buffer(x, .1))
	expect_warning(st_simplify(x, .1))
	expect_warning(st_centroid(x))
	expect_warning(st_segmentize(l, 0.1))

	expect_silent(st_area(x))
	expect_silent(st_length(l))

	# errors:
	expect_error(st_distance(x, y))
	expect_error(st_triangulate(x))
})
