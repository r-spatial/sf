context("sf")

test_that("MtrxSet is being called", {
  outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
  hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
  hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
  pts = list(outer, hole1, hole2)
  pl1 = st_polygon(pts)
  expect_identical(st_as_text(pl1),
  "POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 2, 2 2, 2 1, 1 1), (5 5, 5 6, 6 6, 6 5, 5 5))")
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
  expect_identical(st_as_text(ml1), "MULTILINESTRING ((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 2, 2 2, 2 1, 1 1), (5 5, 5 6, 6 6, 6 5, 5 5))")
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
	digits = options("digits")[[1]]
	options(digits = 16)
	expect_identical(format(st_multipoint(matrix(1:6/6,3))), "MULTIPOINT ((0.166666666666...")
	expect_identical(format(st_sfc(st_multipoint(matrix(1:6/6,3)))),
		"MULTIPOINT ((0.166666666666...")
	options(digits = digits)
	expect_identical(obj_sum.sfc(st_sfc(st_multipoint(matrix(1:6/6,3)))),
		"MULTIPOINT (...")
	expect_identical(type_sum.sfc(st_sfc(st_multipoint(matrix(1:6/6,3)))), "MULTIPOINT")
})

test_that("Ops work for sfc", {
	expect_identical(st_point(c(1,2,3)) + 4, st_point(c(5,6,7)))
	expect_identical(st_point(c(1,2,3)) * 3 + 4, st_point(c(7, 10, 13)))
	m = matrix(0, 2, 2)
	diag(m) = c(1, 3)
	expect_identical(st_point(c(1,2)) * m + c(2,5), st_point(c(3,11)))
	m = matrix(1:6,,2)
	expect_identical(sf:::LINESTRING2MULTILINESTRING(st_linestring(m)), st_multilinestring(list(m)))
	outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
	hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
	hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
	pts = list(outer, hole1, hole2)
	expect_true(inherits(st_multipolygon(list(pts)) * 2 + 3, "MULTIPOLYGON"))
	gc = st_geometrycollection(list(st_multipolygon(list(pts)), st_point(c(2,2))))
	m = matrix(0, 2, 2)
	diag(m) = c(1, 3)
	expect_true(inherits(gc * m - 3, "GEOMETRYCOLLECTION"))
})

test_that("Ops work for sfg", {
	x = st_sfc(st_point(0:1), st_point(1:2))
	y = st_sfc(st_point(2:3), st_point(3:4))
	expect_equal(x + 2, y)
})

test_that("st_dimension returns NA", {
	expect_equal(st_dimension(st_point()), NA_integer_)
})
