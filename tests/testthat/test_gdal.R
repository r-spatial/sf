context("sf: gdal tests")

test_that("st_transform works", {
  skip_if_not_installed("sp")
  skip_if_not_installed("rgdal")
  library(sp)

  s = st_sfc(st_point(c(1,1)), st_point(c(10,10)), st_point(c(5,5)), crs = 4326)
  s1.tr = st_transform(s, 3857)

  sp = as(s, "Spatial")
  sp.tr = spTransform(sp, CRS("+init=epsg:3857"))
  s2.tr = st_as_sfc(sp.tr)
  #attr(s1.tr, "crs")$proj4string = ""
  #attr(s2.tr, "crs")$proj4string = ""
  st_crs(s1.tr) = NA_crs_
  st_crs(s2.tr) = NA_crs_
  if (sf_extSoftVersion()["proj.4"] < "5.0.0") # FIXME:
    expect_equal(s1.tr, s2.tr)

  toCrs = 3857
  s1.tr = st_transform(s, toCrs)
  #attr(s1.tr, "crs")$proj4string = ""
  st_crs(s1.tr) = NA_crs_
  st_crs(s2.tr) = NA_crs_
  if (sf_extSoftVersion()["proj.4"] < "5.0.0") # FIXME:
    expect_equal(s1.tr, s2.tr)

  expect_silent({
    sf.tr = st_transform(st_sf(a=1:3, s), toCrs) # for sf
    sfg.tr = st_transform(structure(s[[1]], proj4string="+proj=longlat +datum=WGS84 +no_defs"), toCrs) # sfg
  })
})

test_that("gdal can be loaded, unloaded, and loaded", {
  expect_silent({
  sf:::.onLoad()
  sf:::.onUnload()
  sf:::.onLoad()
  }
  )
})

test_that("st_wrap_dateline works", {
	expect_silent(x <- st_wrap_dateline(st_sfc(st_linestring(rbind(c(-179,0),c(179,0))), crs = 4326)))
})

test_that('gdal_subdatasets works', {
  skip_if_not(sf_extSoftVersion()[["GDAL"]] >= "2.1.0")
  fname = system.file("nc/cropped.nc", package = "sf")
  sd2 = gdal_subdatasets(fname)[[2]]
})

# context("gdal utils")
test_that('gdal_utils work', {
  skip_on_appveyor() # FIXME:
  skip_if_not(sf_extSoftVersion()[["GDAL"]] >= "2.1.0")

  fname = system.file("nc/cropped.nc", package = "sf")
  #fname = system.file("tif/geomatrix.tif", package = "sf")
  info = gdal_utils("info", fname, quiet = TRUE)
  sd2 = gdal_subdatasets(fname)[[2]]
  info = gdal_utils("info", sd2, quiet = TRUE)
  tf = tempfile()
  tf2 = tempfile()
  tf3 = tempfile()
  #tf = "foo"
  #gdal_utils("rasterize", points, tif) -> need a good example
  #gdal_utils("warp", sd2, tf, c("-t_srs", "+proj=utm +zone=11 +datum=WGS84"))
  #expect_true(gdal_utils("warp", sd2, tf, c("-t_srs", "+proj=utm +zone=11 +datum=WGS84")))
  #gdal_utils("warp", sd2, tf, c("-t_srs", "+proj=utm +zone=11 +datum=WGS84"))
  #expect_true(gdal_utils("warp", sd2, tf))
  #expect_true(gdal_utils("rasterize", sd2, tf))
  expect_true(gdal_utils("translate", sd2, tf))
  expect_true(gdal_utils("vectortranslate", sd2, tf2))
  expect_warning(gdal_utils("nearblack", sd2, tf))
  # create point geom:
  points = system.file("gpkg/nc.gpkg", package="sf")
  expect_true(gdal_utils("grid", points, tf))
  expect_true(gdal_utils("buildvrt", sd2, tf3))
  expect_true(gdal_utils("demprocessing", sd2, tf, processing = "hillshade"))
})

# gdalwarp -t_srs '+proj=utm +zone=11 +datum=WGS84' -overwrite NETCDF:avhrr-only-v2.19810901.nc:anom utm11.tif
# becomes:
# st_gdalwarp("NETCDF:avhrr-only-v2.19810901.nc:anom", "utm11.tif", c("-t_srs", "+proj=utm +zone=11 +datum=WGS84"))
