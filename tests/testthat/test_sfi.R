context("sf")

test_that("MtrxSet is being called", {
  outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
  hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
  hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
  pts = list(outer, hole1, hole2)
  pl1 = st_polygon(pts)
  expect_identical(st_as_wkt(pl1),
  "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 2, 2 2, 2 1, 1 1), (5 5, 5 6, 6 6, 6 5, 5 5))")
})

test_that("Dimension works", {
  expect_identical(sf:::Dimension(st_point(1:2)), "XY")
  expect_identical(sf:::Dimension(st_point(1:3)), "XYZ")
  expect_identical(sf:::Dimension(st_point(1:3, "XYZ")), "XYZ")
  expect_identical(sf:::Dimension(st_point(1:3, "XYM")), "XYM")
  expect_identical(sf:::Dimension(st_point(1:4)), "XYZM")
})

test_that("st_multilinestring works", {
  outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
  hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
  hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
  pts = list(outer, hole1, hole2)
  ml1 = st_multilinestring(pts)
  expect_identical(st_as_wkt(ml1), "MULTILINESTRING((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 2, 2 2, 2 1, 1 1), (5 5, 5 6, 6 6, 6 5, 5 5))")
})

test_that("xx2multixx works", {
  expect_identical(sf:::POINT2MULTIPOINT(st_point(1:2)), st_multipoint(matrix(1:2,1)))
  m = matrix(1:6,,2)
  expect_identical(sf:::LINESTRING2MULTILINESTRING(st_linestring(m)), st_multilinestring(list(m)))
  outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
  hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
  hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
  pts = list(outer, hole1, hole2)
  expect_identical(sf:::POLYGON2MULTIPOLYGON(st_polygon(pts)), st_multipolygon(list(pts)))
})

test_that("format works", {
	expect_identical(format(st_multipoint(matrix(1:6/6,3))), "MULTIPOINT(0.16666666666666...")
	expect_identical(format(st_sfc(st_multipoint(matrix(1:6/6,3)))), 
		"MULTIPOINT(0.16666666666666...")
	expect_identical(obj_sum.sfc(st_sfc(st_multipoint(matrix(1:6/6,3)))), 
		"MULTIPOINT(0...")
	expect_identical(type_sum.sfc(st_sfc(st_multipoint(matrix(1:6/6,3)))), "simple_feature")
})

test_that("coerceType works in sfc", {
  m = matrix(1:6,,2)
  sfc = st_sfc(st_linestring(m), st_multilinestring(list(m)))
  expect_true(all(class(sfc) == c("sfc_MULTILINESTRING", "sfc")))
  sfc = st_sfc(st_point(1:2), st_multilinestring(list(m)))
  expect_true(all(class(sfc) == c("sfc_GEOMETRYCOLLECTION", "sfc")))
})
