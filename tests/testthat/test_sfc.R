context("sfc")

test_that("we can print sfc objects", {
  pt1 = st_point(1:2)
  pt2 = st_point(3:4)
  s1 = st_sf(a = c("x", "y"), geom = st_sfc(pt1, pt2))
  expect_output(print(s1), "Simple feature collection")
  expect_output(print(st_sfc()), "Geometry set for 0 features")
  expect_equal(length(st_sfc()), 0)
})

test_that("st_is_longlat works", {
  pt1 = st_point(1:2)
  pt2 = st_point(3:4)
  s1 = st_sf(a = c("x", "y"), geom = st_sfc(pt1, pt2))
  expect_equal(st_is_longlat(s1), NA)
  s2 = s1
  st_crs(s2) = 3857
  expect_false(st_is_longlat(s2))
  st_crs(s1) = 4326
  expect_true(st_is_longlat(s1))
})

test_that("st_crs returns NA for sfg objects", {
  pt1 = st_point(1:2)
  expect_true(is.na(st_crs(pt1)))
})

test_that("st_as_binary can non-native big endian", {
  gc = st_geometrycollection()
  r = st_as_binary(gc)
  r[1] = if (r[1] == 1) {
  	as.raw(0)
  } else {
  	as.raw(1)
  }
  r[2:5] = rev(r[2:5]) # swap bytes
  expect_identical(gc, st_as_sfc(structure(list(r), class = "WKB"), pureR = T)[[1]])
  expect_identical(gc, st_as_sfc(structure(list(r), class = "WKB"), pureR = T, EWKB = TRUE)[[1]])
})

test_that("st_crs<- gives warnings on changing crs", {
	x = st_sfc(list(st_point(0:1), st_point(0:1)))
	y = x
	expect_silent(st_crs(y) <- 4326)
	expect_silent(st_crs(y) <- 4326)
	expect_warning(st_crs(y) <- 3857)
	x = st_sfc(list(st_point(0:1), st_point(0:1)), crs = 4326)
	#expect_silent(st_sfc(x, crs = "+proj=longlat +datum=WGS84 +no_defs"))
	#expect_silent(st_sfc(x, crs = "+proj=longlat +datum=WGS84")) #-->> breaks build on CRAN flavor fedora-gcc
	# but do when it changes:
	expect_warning(st_sfc(x, crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
})
