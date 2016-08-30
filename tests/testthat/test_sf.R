context("sf: subset; p4s")

test_that("we can subset sf objects", {
  pt1 = st_point(1:2)
  pt2 = st_point(3:4)
  s1 = st_sf(a = c("x", "y"), geom = st_sfc(list(pt1, pt2)))
  expect_equal(s1[[1]], factor(c("x", "y")))
  expect_equal(s1[,1], data.frame(x = c("x", "y")))

  expect_equal(nrow(s1[1,]), 1)
  expect_equal(st_bbox(s1[1,]), c(xmin=1,ymin=2,xmax=1,ymax=2))
  expect_equal(st_p4s(s1[1,]), NULL)

  a = c("x", "y")
  g = st_sfc(list(pt1, pt2))
  expect_warning(st_sf(a,g,g), "more than one geometry column: ignoring all but first")
})

test_that("we can create points sf from data.frame", {
  data(meuse, package = "sp") # load data.frame from sp
  meuse_sf = st_as_sf(meuse, coords = c("x", "y"), epsg = 28992)
  meuse_sf[1:5,]
  summary(meuse_sf[1:5,])
  expect_identical(class(meuse_sf), c("sf", "data.frame"))
})
