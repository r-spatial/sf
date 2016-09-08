context("sfc")

test_that("we can print sfc objects", {
  pt1 = st_point(1:2)
  pt2 = st_point(3:4)
  s1 = st_sf(a = c("x", "y"), geom = st_sfc(pt1, pt2))
  print(s1)
  print(st_sfc())
  expect_equal(length(st_sfc()), 0)
})
