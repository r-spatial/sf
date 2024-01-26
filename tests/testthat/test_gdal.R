test_that("st_transform works", {
  skip_if_not_installed("sp")
  library(sp)

  s = st_sfc(st_point(c(1,1)), st_point(c(10,10)), st_point(c(5,5)), crs = 4326)
  s1.tr = st_transform(s, 3857)

  sp = as(s, "Spatial")
#  sp.tr = spTransform(sp, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +nadgrids=@null +no_defs")) # web mercator
#  s2.tr = st_as_sfc(sp.tr)
  #attr(s1.tr, "crs")$proj4string = ""
  #attr(s2.tr, "crs")$proj4string = ""
  st_crs(s1.tr) = NA_crs_
#  st_crs(s2.tr) = NA_crs_
#  if (sf_extSoftVersion()["proj.4"] < "5.0.0") # FIXME:
#    expect_equal(s1.tr, s2.tr)

  toCrs = 3857
  s1.tr = st_transform(s, toCrs)
  #attr(s1.tr, "crs")$proj4string = ""
  st_crs(s1.tr) = NA_crs_
#  st_crs(s2.tr) = NA_crs_
#  if (sf_extSoftVersion()["proj.4"] < "5.0.0") # FIXME:
#    expect_equal(s1.tr, s2.tr)

  expect_silent({
    sf.tr = st_transform(st_sf(a=1:3, s), toCrs) # for sf
    sfg.tr = st_transform(structure(s[[1]], proj4string="+proj=longlat +datum=WGS84 +no_defs"), toCrs) # sfg
  })
})

#test_that("gdal can be loaded, unloaded, and loaded", {
#  expect_silent({
#  unload_gdal()
#  load_gdal()
#  }
#  )
#})

test_that("st_wrap_dateline works", {
	expect_silent(x <- st_wrap_dateline(st_sfc(st_linestring(rbind(c(-179,0),c(179,0))), crs = 4326)))
})

test_that('gdal_subdatasets works', {
  skip_if_not(sf_extSoftVersion()[["GDAL"]] >= "2.1.0")
  skip_if_not(sf_extSoftVersion()[["GDAL"]] < "2.5.0") # FIXME:
  skip_on_os("mac") # FIXME:
  fname = system.file("nc/cropped.nc", package = "sf")
  sd2 = gdal_subdatasets(fname)[[2]]
})

# context("gdal utils")
test_that('gdal_utils work', {
  skip_on_appveyor() # FIXME:
  skip_if_not(Sys.getenv("USER") %in% c("edzer", "travis"))
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
  shp = system.file("shape/nc.shp", package="sf")
  gpkg = paste0(tempfile(), ".gpkg")
  options = c("-f", "GPKG", "-overwrite", "-nlt", "PROMOTE_TO_MULTI", 
  	"-oo", "ADJUST_TYPE=NO", "-doo", "FLATTEN_NESTED_ATTRIBUTES=NO")
  expect_true(gdal_utils("vectortranslate", shp, gpkg, options = options))
  expect_warning(gdal_utils("nearblack", sd2, tf))
  # create point geom:
  points = system.file("gpkg/nc.gpkg", package="sf")
  expect_true(gdal_utils("grid", points, tf))
  expect_true(gdal_utils("buildvrt", sd2, tf3))
  expect_warning(gdal_utils("buildvrt", sd2, tf3, c("-oo", "FOO=BAR"))) # fake opening options
  expect_error(gdal_utils("buildvrt", "foo.tif", tf3, c("-oo", "FOO=BAR")), "cannot open source dataset")
  expect_true(gdal_utils("demprocessing", sd2, tf, processing = "hillshade"))
  # check gdalfootprint
  skip_if_not(sf_extSoftVersion()[["GDAL"]] >= "3.8.0")
  tif <- system.file("tif/geomatrix.tif", package="sf")
  tf4 <- tempfile(fileext = ".gpkg")
  expect_true(gdal_utils("footprint", tif, tf4))
})

# gdalwarp -t_srs '+proj=utm +zone=11 +datum=WGS84' -overwrite NETCDF:avhrr-only-v2.19810901.nc:anom utm11.tif
# becomes:
# st_gdalwarp("NETCDF:avhrr-only-v2.19810901.nc:anom", "utm11.tif", c("-t_srs", "+proj=utm +zone=11 +datum=WGS84"))

test_that('gdal_addo works', {

    skip_on_cran()

    has_overviews = function(x){
        info = gdal_utils(source = x, quiet = TRUE)
        grepl("overview", info, ignore.case = TRUE)
    }

   has_compressed_overviews = function(x){
        # Check if sidecar overview file has compression, x is tif path
        path = paste0(x, ".ovr") # overview file
        info = gdal_utils(source = path, quiet = TRUE)
        if(!file.exists(path))
            return(NA)
        grepl("compression", info, ignore.case = TRUE)
    }

    # setup
    dir = file.path(tempdir(), "gdal_addo")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE))  # cleanup when done
    tif = file.path(dir, "geomatrix.tif")
    file.copy(system.file("tif/geomatrix.tif", package = "sf"),
                           tif, overwrite = TRUE)

    expect_false(has_overviews(tif))

    # Default arguments
    expect_no_error(gdal_addo(tif)) # internal overview
    expect_true(has_overviews(tif))
    expect_true(is.na(has_compressed_overviews(tif))) # no overview file

    # Clean overviews
    expect_no_error(gdal_addo(tif, clean = TRUE))
    expect_false(has_overviews(tif))

    # Overviews in separate file
    expect_no_error(gdal_addo(tif, read_only = TRUE))
    expect_false(has_compressed_overviews(tif)) # uncompressed overview file

    # Clean overviews
    expect_no_error(gdal_addo(tif, clean = TRUE))
    expect_false(has_overviews(tif))

    # Compression via config_options works
    expect_no_error(gdal_addo(tif, read_only = TRUE,
                              config_options = c(COMPRESS_OVERVIEW="LZW")))
    expect_true(has_compressed_overviews(tif))

})
