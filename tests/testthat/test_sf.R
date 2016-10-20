context("sf: subset")

test_that("we can subset sf objects", {
  pt1 = st_point(1:2)
  pt2 = st_point(3:4)
  s1 = st_sf(a = c("x", "y"), geom = st_sfc(pt1, pt2))
  expect_equal(s1[[1]], factor(c("x", "y")))
  expect_equal(s1[,1], data.frame(x = c("x", "y")))

  expect_equal(nrow(s1[1,]), 1)
  expect_equal(st_bbox(s1[1,]), c(xmin=1,ymin=2,xmax=1,ymax=2))

  a = c("x", "y")
  g = st_sfc(pt1, pt2)
  expect_warning(st_sf(a,g,g), "more than one geometry column: ignoring all but first")
})

test_that("we can create points sf from data.frame", {
  data(meuse, package = "sp") # load data.frame from sp
  meuse_sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
  meuse_sf[1:5,]
  summary(meuse_sf[1:5,])
  expect_identical(class(meuse_sf), c("sf", "data.frame"))
})

test_that("drop_zm works", {
  pt = st_point(1:2)
  ptz = st_point(1:3, "XYZ")
  ptm = st_point(1:3, "XYM")
  ptzm = st_point(1:4, "XYZM")
  pl = st_multilinestring(list(matrix(1:10,,2), matrix(1:10,,2)))
  plz = st_multilinestring(list(matrix(1:15,,3), matrix(1:15,,3)), "XYZ")
  plm = st_multilinestring(list(matrix(1:15,,3), matrix(1:15,,3)), "XYM")
  plzm = st_multilinestring(list(matrix(1:20,,4), matrix(1:20,,4)), "XYZM")
  expect_identical(pt, st_drop_zm(ptz))
  expect_identical(pt, st_drop_zm(ptm))
  expect_identical(pt, st_drop_zm(ptzm))
  expect_identical(pl, st_drop_zm(plz))
  expect_identical(pl, st_drop_zm(plm))
  expect_identical(pl, st_drop_zm(plzm))
  expect_identical(st_sfc(pt,pl), st_drop_zm(st_sfc(ptz,plz)))
  expect_identical(st_sfc(pt,pl), st_drop_zm(st_sfc(ptm,plm)))
  expect_identical(st_sfc(pt,pl), st_drop_zm(st_sfc(ptzm,plzm)))
  expect_identical(st_sf(a = 1:2, geom = st_sfc(pt,pl)), st_drop_zm(st_sf(a = 1:2, geom = st_sfc(ptzm,plzm))))
})
