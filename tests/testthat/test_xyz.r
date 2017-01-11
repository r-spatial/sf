# PR to add a test suite for XYZ transformations, with two items . 
# 
# Currently these tests are failing: 
#   
#   ## Bug ?
#   
#   * planar transformations preserve Z
# * geocentric transformations preserve Z
# 
# Expectation:  XYZ geometries should preserve the Z dimension for planar  (4326 to laea) or geocentric (4326 to geocent)
# 
# Result:   Z is dropped for  the non-MULTI geometries. 
# 
# ## New feature needed? 
# 
# This was mentioned in #103 as a possible improvement. 
# 
# * geocentric transformations ADD Z
# 
# Expectation: transformation to geocent should convert XY to XYZ
# 
# Result:  XY geometries stay as XY. 
# 
# 
# 
# See also discussion here
# 
# https://github.com/edzer/sfr/issues/103
# 

## create a set of geometries
## p, mp, ml, pol, mpol 
## pz, mpz, mlz, polz, mpolz

p <- st_point(c(0, 0))
pz <- st_point(c(0, 0, 0))

mp <- st_multipoint(rbind(p, p * 2))
mpz <- st_multipoint(rbind(pz, pz * 2))

l1 <- cbind(0:1, 0:1)
l1z <- cbind(0:1, 0:1, 0:1)

l <- st_linestring(l1)
lz <- st_linestring(l1z)

ml <- st_multilinestring(replicate(2, l1, simplify = FALSE))
mlz <- st_multilinestring(replicate(2, l1z, simplify = FALSE))

p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
pol1 <- list(p1,p2)
pol1z <- list(cbind(p1, 0), cbind(p2, 0))

pol <- st_polygon(pol1)
polz <- st_polygon(pol1z)

mpol <- st_multipolygon(list(pol1, pol1))
mpolz <- st_multipolygon(list(pol1z, pol1z))

## worker functions to apply transformations
## do we need to test inverse versions of these as well? vertical? (see below)
do_planar <- function(x) st_transform(st_sfc(x, crs = 4326), crs = "+proj=laea")
do_geo <- function(x) st_transform(st_sfc(x, crs = 4326), crs = "+proj=geocent")

## is this XYZ?
hasz <- function(x) UseMethod("hasz")  
hasz.sfg <- function(x) grepl("XYZ", rev(class(x))[3L])
## only for single length columns
hasz.sfc <- function(x) hasz(x[[1]])

## all as expected 
test_that("has Z has not Z", {
  expect_false(hasz(p))
  expect_false(hasz(mp))
  expect_false(hasz(l))
  expect_false(hasz(ml))
  expect_false(hasz(pol))
  expect_false(hasz(mpol))
  
  expect_true(hasz(pz))
  expect_true(hasz(mpz))
  expect_true(hasz(lz))
  expect_true(hasz(mlz))
  expect_true(hasz(polz))
  expect_true(hasz(mpolz))
  
})

## only the m-kinds match expection
test_that("planar transformations preserve Z", {
  expect_true(hasz(do_planar(pz)))
  expect_true(hasz(do_planar(mpz)))
  expect_true(hasz(do_planar(lz)))
  expect_true(hasz(do_planar(mlz)))
  expect_true(hasz(do_planar(polz)))
  expect_true(hasz(do_planar(mpolz)))
}
)

## only the m-kinds match expection
test_that("geocentric transformations preserve Z", {
  expect_true(hasz(do_geo(pz)))
  expect_true(hasz(do_geo(mpz)))
  expect_true(hasz(do_geo(lz)))
  expect_true(hasz(do_geo(mlz)))
  expect_true(hasz(do_geo(polz)))
  expect_true(hasz(do_geo(mpolz)))
}
)

## wishful thinking?
test_that("geocentric transformations ADD Z", {
  expect_true(hasz(do_geo(p)))
  expect_true(hasz(do_geo(mp)))
  expect_true(hasz(do_geo(l)))
  expect_true(hasz(do_geo(ml)))
  expect_true(hasz(do_geo(pol)))
  expect_true(hasz(do_geo(mpol)))
}
)

## 
## 
## need an installation with the grid files
# test_that("vertical transformations preserve Z", {
#   expect_true(hasz(do_vertical(pz)))
#   expect_true(hasz(do_vertical(mpz)))
#   expect_true(hasz(do_vertical(lz)))
#   expect_true(hasz(do_vertical(mlz)))
#   expect_true(hasz(do_vertical(polz)))
#   expect_true(hasz(do_vertical(mpolz)))
# }
# )

## Will a vertical transformation ADD Z?
## 
## need an installation with the grid files
# test_that("vertical transformations ADD Z", {
#   expect_true(hasz(do_vertical(p)))
#   expect_true(hasz(do_vertical(mp)))
#   expect_true(hasz(do_vertical(l)))
#   expect_true(hasz(do_vertical(ml)))
#   expect_true(hasz(do_vertical(pol)))
#   expect_true(hasz(do_vertical(mpol)))
# }
# )


## these need GRID FILE installations that I don't have
## 
## https://github.com/edzer/sfr/issues/103#issuecomment-266014403
#do_vertical <- function(x) st_transform(st_sfc(x, crs = 7421), crs = 4326)
#do_vertical <- function(x) st_transform(st_sfc(x, crs = 5498), crs = 4326)

# ## find a vertical crs
# library(sf)
# i <- 5498
# 
# test <- FALSE
# 
# while(!test) {
#   i <- i + 1
#   a <- try(sf:::crs_parameters(st_crs(i)), silent = TRUE)
#   if (inherits(a, "try-error")) next;
#   if (a$IsVertical) {
##    stop()
#     #b <- try(st_transform(st_sfc(pz, crs = 4326), crs = i), silent = TRUE)
#     #if (!inherits(b, "try-error")) stop()
#   }
# 
# }

# ## 5498, needs grid files
# sf:::crs_parameters(st_crs(5498))
# $SemiMajor
# 6378137 m
# 
# $InvFlattening
# [1] 298.2572
# 
# $units_gdal
# [1] "degree"
# 
# $IsVertical
# [1] TRUE
# 
# $WktPretty
# [1] "COMPD_CS[\"GRS 1980(IUGG, 1980) + Unnamed Vertical Datum\",\n    GEOGCS[\"GRS 1980(IUGG, 1980)\",\n        DATUM[\"unknown\",\n            SPHEROID[\"GRS80\",6378137,298.257222101],\n            TOWGS84[0,0,0,0,0,0,0]],\n        PRIMEM[\"Greenwich\",0],\n        UNIT[\"degree\",0.0174532925199433]],\n    VERT_CS[\"Unnamed\",\n        VERT_DATUM[\"Unnamed\",2005,\n            EXTENSION[\"PROJ4_GRIDS\",\"g2012a_conus.gtx,g2012a_alaska.gtx,g2012a_guam.gtx,g2012a_hawaii.gtx,g2012a_puertorico.gtx,g2012a_samoa.gtx\"]],\n        UNIT[\"Meter\",1.0],\n        AXIS[\"Up\",UP]]]"
# 
# $Wkt
# [1] "COMPD_CS[\"GRS 1980(IUGG, 1980) + Unnamed Vertical Datum\",GEOGCS[\"GRS 1980(IUGG, 1980)\",DATUM[\"unknown\",SPHEROID[\"GRS80\",6378137,298.257222101],TOWGS84[0,0,0,0,0,0,0]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.0174532925199433]],VERT_CS[\"Unnamed\",VERT_DATUM[\"Unnamed\",2005,EXTENSION[\"PROJ4_GRIDS\",\"g2012a_conus.gtx,g2012a_alaska.gtx,g2012a_guam.gtx,g2012a_hawaii.gtx,g2012a_puertorico.gtx,g2012a_samoa.gtx\"]],UNIT[\"Meter\",1.0],AXIS[\"Up\",UP]]]"
# 
# $ud_unit
# 1 arc_degree
# 
