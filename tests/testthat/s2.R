skip_if_not_installed("s2")
context("sf: s2")

test_that("s2 roundtrips work", {
  library(s2)
  nc = st_geometry(st_read(system.file("shape/nc.shp", package="sf")))
  s2 = st_as_sfc(st_as_s2(nc), crs = st_crs(nc))

  # precision?
  expect_equal(sum(lengths(st_equals(s2, nc))), 0L)
  expect_equal(sum(lengths(st_equals(st_set_precision(s2, 1e8), st_set_precision(nc, 1e8))) == 1), 98L)
  expect_equal(sum(lengths(st_equals(st_set_precision(s2, 1e7), st_set_precision(nc, 1e7))) == 1), 100L)
})

test_that("as_s2_geography() is defined for sf and sfc objects", {
  nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
  expect_is(s2::as_s2_geography(nc), "s2_geography")
  expect_is(s2::as_s2_geography(st_geometry(nc)), "s2_geography")
})

test_that("s2 polygon creation", {
  outer = rbind(c(10,35),  # CCW:
      c(20,10),
      c(40,15),
      c(45,45),
      c(10,35))
  inner = rbind(c(30,20), # CW:
      c(20,30),
      c(35,35),
      c(30,20))
#  expect_silent(l <- s2polyline(outer))
#  expect_error(p <- s2polygon(l))
#  expect_silent(l <- s2polyline(outer[1:4,]))
#  expect_silent(p <- s2polygon(l))

#  expect_silent(i <- s2polyline(inner))
#  expect_error(p <- s2polygon(i))
#  expect_silent(i <- s2polyline(inner[1:3,]))
#  expect_silent(p <- s2polygon(i))
})
