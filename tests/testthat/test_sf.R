test_that("we can subset sf objects", {
  pt1 = st_point(1:2)
  pt2 = st_point(3:4)
  s1 = st_sf(a = c("x", "y"), geom = st_sfc(pt1, pt2))
  expect_equal(as.character(s1[[1]]), c("x", "y"))
  expect_equal(s1[,1], s1) #data.frame(x = c("x", "y")))

  expect_equal(nrow(s1[1,]), 1)
  expect_equal(c(st_bbox(s1[1,])), c(xmin=1,ymin=2,xmax=1,ymax=2))

  a = c("x", "y")
  g = st_sfc(pt1, pt2)
  expect_silent(xxx <- st_sf(a, g, g))
  expect_silent(st_sf(a, geom1 = g, geom2 = g, sf_column_name = "geom2"))
  x = st_sf(a, geom1 = g, geom2 = g, sf_column_name = "geom2")
  expect_silent(st_geometry(x) <- "geom2")
  expect_silent(st_geometry(x) <- "geom1")
})

test_that("we can create points sf from data.frame", {
  skip_if_not_installed("sp")
  data(meuse, package = "sp") # load data.frame from sp
  meuse_sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
  meuse_sf[1:5,]
  summary(meuse_sf[1:5,])
  expect_identical(class(meuse_sf), c("sf", "data.frame"))
})

test_that("st_zm works", {
  pt = st_point(1:2)
  ptz = st_point(1:3, "XYZ")
  ptm = st_point(1:3, "XYM")
  ptzm = st_point(1:4, "XYZM")
  pl = st_multilinestring(list(matrix(1:10,,2), matrix(1:10,,2)))
  plz = st_multilinestring(list(matrix(1:15,,3), matrix(1:15,,3)), "XYZ")
  plm = st_multilinestring(list(matrix(1:15,,3), matrix(1:15,,3)), "XYM")
  plzm = st_multilinestring(list(matrix(1:20,,4), matrix(1:20,,4)), "XYZM")
  expect_identical(pt, st_zm(ptz))
  expect_identical(pt, st_zm(ptm))
  expect_identical(pt, st_zm(ptzm))
  expect_identical(pl, st_zm(plz))
  expect_identical(pl, st_zm(plm))
  expect_identical(pl, st_zm(plzm))
  expect_identical(st_sfc(pt,pl), st_zm(st_sfc(ptz,plz)))
  expect_identical(st_sfc(pt,pl), st_zm(st_sfc(ptm,plm)))
  expect_identical(st_sfc(pt,pl), st_zm(st_sfc(ptzm,plzm)))
  expect_identical(st_sf(a = 1:2, geom = st_sfc(pt,pl)), st_zm(st_sf(a = 1:2, geom = st_sfc(ptzm,plzm))))
  expect_identical(st_zm(pt, drop = FALSE, what = "Z"), st_point(c(1:2,0)))
  expect_silent(st_zm(pl, drop = FALSE, what = "Z"))
})

test_that("rbind/cbind work", {
  # cbind/rbind:
  x = st_sf(a = 1:2, geom = st_sfc(list(st_point(0:1), st_point(0:1)), crs = 4326))
  # don't warn when replacing crs with identical value:
  if (version$major == "3") {
  	if (version$minor >= "3.0") {
      expect_silent(xxx <- cbind(x, x, x))
      rbind(x, x, x)
	}
  }
})

test_that("st_as_sf bulk points work", {
  skip_if_not_installed("sp")
  data(meuse, package = "sp") # load data.frame from sp
  x <- meuse
  meuse_sf = st_as_sf(x, coords = c("x", "y"), crs = 28992)
  xyz_sf = st_as_sf(x, coords = c("y", "x", "dist"))
  xym_sf = st_as_sf(x, coords = c("y", "x", "dist"), dim = "XYM")
  xyzm_sf = st_as_sf(x, coords = c("x", "y", "dist", "zinc"), dim = "XYZM")
  expect_identical(class(meuse_sf), c("sf", "data.frame"))
  expect_identical(class(xyz_sf), c("sf", "data.frame"))
  expect_identical(class(xym_sf), c("sf", "data.frame"))
  expect_identical(class(xyzm_sf), c("sf", "data.frame"))
  expect_length(unclass(st_geometry(meuse_sf)[[1]]), 2L)
  expect_length(unclass(st_geometry(xyz_sf)[[1]]), 3L)
  expect_length(unclass(st_geometry(xym_sf)[[1]]), 3L)
  expect_length(unclass(st_geometry(xyzm_sf)[[1]]), 4L)


})

test_that("transform work", {
  skip_if_not_installed("sp")
  data(meuse, package = "sp")
  x  = st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
  x2 = transform(x, elev2 = elev^2, lead_zinc = lead/zinc)
  expect_true(inherits(x, 'sf'))
  expect_identical(class(x2), class(x))
  expect_identical(st_bbox(x), st_bbox(x))
  expect_identical(st_crs(x), st_crs(x))
  expect_identical(x$elev^2, x2$elev2)
})

test_that("empty agr attribute is named after subset", {
	sf = st_sf(data.frame(x = st_sfc(st_point(1:2))))
	out = sf[, "geometry"]
	agr = attr(out, "agr")
	expect_identical(names(agr), character())
})
