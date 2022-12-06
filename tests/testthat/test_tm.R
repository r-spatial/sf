context("sf: date and time")

test_that("st_read and write handle date and time", {
	Sys.setenv(TZ="") # local time
    x = st_sf(a = 1:2, b=c(5.6,3), dt = Sys.Date()+1:2, tm = Sys.time()+2:3, 
              geometry = structure(st_sfc(st_point(c(1,1)), st_point(c(2,2)))))
    shp <- paste0(tempfile(), ".shp")
    gpkg <- paste0(tempfile(), ".gpkg")

   st_crs(x) = st_crs("ENGCRS[\"Undefined Cartesian SRS with unknown unit\",EDATUM[\"Unknown engineering datum\"],CS[Cartesian,2],AXIS[\"X\",unspecified,ORDER[1],LENGTHUNIT[\"unknown\",0]],AXIS[\"Y\",unspecified,ORDER[2],LENGTHUNIT[\"unknown\",0]]]")
    
    st_write(x[-4], shp[1], quiet = TRUE)
    x2 = st_read(shp[1], quiet = TRUE)
    expect_equal(x[-4], x2, check.attributes=FALSE)
    expect_true(st_crs(x[-4])$wkt == st_crs(x2)$wkt)
    
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

   st_crs(x) = st_crs("ENGCRS[\"Undefined Cartesian SRS with unknown unit\",EDATUM[\"Unknown engineering datum\"],CS[Cartesian,2],AXIS[\"X\",unspecified,ORDER[1],LENGTHUNIT[\"unknown\",0]],AXIS[\"Y\",unspecified,ORDER[2],LENGTHUNIT[\"unknown\",0]]]")
    
    st_write(x[-4], shp[1], quiet = TRUE)
    x2 = st_read(shp[1], quiet = TRUE)
    expect_equal(x[-4], x2, check.attributes=FALSE)
    expect_true(st_crs(x[-4])$wkt == st_crs(x2)$wkt)
    
    st_write(x, gpkg, quiet = TRUE)
    x2 = st_read(gpkg, quiet = TRUE)
    expect_equal(x[["a"]], x2[["a"]])
    expect_equal(x[["b"]], x2[["b"]])
    expect_equal(x[["dt"]], x2[["dt"]])
    expect_equal(x[["tm"]], x2[["tm"]])
})
