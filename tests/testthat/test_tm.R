test_that("st_read and write handle date and time", {
    # local modifications: jarodmeng@
    # skip this test because it requires GDAL drivers
    testthat::skip_on_google()
	Sys.setenv(TZ="") # local time
    x = st_sf(a = 1:2, b=c(5.6,3), dt = Sys.Date()+1:2, tm = Sys.time()+2:3, 
              geometry = structure(st_sfc(st_point(c(1,1)), st_point(c(2,2)))))
    shp <- paste0(tempfile(), ".shp")
    gpkg <- paste0(tempfile(), ".gpkg")
    
    st_write(x[-4], shp[1], quiet = TRUE)
    x2 = st_read(shp[1], quiet = TRUE)
    expect_equal(x[-4], x2)
    
    st_write(x, gpkg, quiet = TRUE)
    x2 = st_read(gpkg, quiet = TRUE)
    expect_equal(x[["a"]], x2[["a"]])
    expect_equal(x[["b"]], x2[["b"]])
    expect_equal(x[["dt"]], x2[["dt"]])
    expect_equal(x[["tm"]], x2[["tm"]])

	Sys.setenv(TZ="UTC") # GMT
    x = st_sf(a = 1:2, b=c(5.6,3), dt = Sys.Date()+1:2, tm = Sys.time()+2:3, 
              geometry = structure(st_sfc(st_point(c(1,1)), st_point(c(2,2)))))
    shp <- paste0(tempfile(), ".shp")
    gpkg <- paste0(tempfile(), ".gpkg")
    
    st_write(x[-4], shp[1], quiet = TRUE)
    x2 = st_read(shp[1], quiet = TRUE)
    expect_equal(x[-4], x2)
    
    st_write(x, gpkg, quiet = TRUE)
    x2 = st_read(gpkg, quiet = TRUE)
    expect_equal(x[["a"]], x2[["a"]])
    expect_equal(x[["b"]], x2[["b"]])
    expect_equal(x[["dt"]], x2[["dt"]])
    expect_equal(x[["tm"]], x2[["tm"]])
})
