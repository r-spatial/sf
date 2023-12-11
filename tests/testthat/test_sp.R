test_that("we can convert points & lines to and from sp objects", {
  skip_if_not_installed("sp")
  pt1 = st_point(1:2)
  pt2 = st_point(3:4)
  s1 = st_sf(a = c("x", "y"), geom = st_sfc(pt1, pt2))
  sp = as(s1, "Spatial")
  s2 = st_as_sf(sp)
  # expect_identical(s1, s2) -> name differences
  expect_identical(class(st_geometry(s2)), c("sfc_POINT", "sfc")) #-> name differences
  l = st_linestring(matrix(1:6,3))
  l1 = st_sf(a = 1, geom = st_sfc(l))
  sp_l = as(l1, "Spatial")
  expect_identical(class(sp_l)[[1]], "SpatialLinesDataFrame") #-> name differences
  l2 = st_as_sf(sp_l)
  expect_identical(class(st_geometry(l2)), c("sfc_LINESTRING", "sfc")) #-> name differences
  # test multilinestring -> sp
  l = st_multilinestring(list(matrix(1:6,3), matrix(11:16,3), matrix(21:26,3)))
  l1 = st_sf(a = 1, geom = st_sfc(l))
  sp_l = as(l1, "Spatial")
  expect_identical(class(sp_l)[[1]], "SpatialLinesDataFrame") #-> name differences
  l2 = st_as_sf(sp_l)
  expect_identical(class(st_geometry(l2)), c("sfc_MULTILINESTRING", "sfc")) #-> name differences
})

test_that("we can convert SpatialPolygons objects without SF comments to sfc and back", {
  skip_if_not_installed("sp")
  # skip_if_not(package_version(sf_extSoftVersion()[["GEOS"]]) >= "3.11.0"); #2079
  library(sp)
# nested holes https://github.com/r-spatial/evolution/issues/9
  p1 <- Polygon(cbind(x=c(0, 0, 10, 10, 0), y=c(0, 10, 10, 0, 0)), hole=FALSE) # I
  p2 <- Polygon(cbind(x=c(3, 3, 7, 7, 3), y=c(3, 7, 7, 3, 3)), hole=TRUE) # H
  p8 <- Polygon(cbind(x=c(1, 1, 2, 2, 1), y=c(1, 2, 2, 1, 1)), hole=TRUE) # H
  p9 <- Polygon(cbind(x=c(1, 1, 2, 2, 1), y=c(5, 6, 6, 5, 5)), hole=TRUE) # H
  p3 <- Polygon(cbind(x=c(20, 20, 30, 30, 20), y=c(20, 30, 30, 20, 20)), hole=FALSE) # I
  p4 <- Polygon(cbind(x=c(21, 21, 29, 29, 21), y=c(21, 29, 29, 21, 21)), hole=TRUE) # H
  p5 <- Polygon(cbind(x=c(22, 22, 28, 28, 22), y=c(22, 28, 28, 22, 22)), hole=FALSE) # I
  p6 <- Polygon(cbind(x=c(23, 23, 27, 27, 23), y=c(23, 27, 27, 23, 23)), hole=TRUE) # H
  p7 <- Polygon(cbind(x=c(13, 13, 17, 17, 13), y=c(13, 17, 17, 13, 13)), hole=FALSE) # I
  p10 <- Polygon(cbind(x=c(24, 24, 26, 26, 24), y=c(24, 26, 26, 24, 24)), hole=FALSE) # I
  p11 <- Polygon(cbind(x=c(24.25, 24.25, 25.75, 25.75, 24.25), y=c(24.25, 25.75, 25.75, 24.25, 24.25)), hole=TRUE) # H
  p12 <- Polygon(cbind(x=c(24.5, 24.5, 25.5, 25.5, 24.5), y=c(24.5, 25.5, 25.5, 24.5, 24.5)), hole=FALSE) # I
  p13 <- Polygon(cbind(x=c(24.75, 24.75, 25.25, 25.25, 24.75), y=c(24.75, 25.25, 25.25, 24.75, 24.75)), hole=TRUE) # H
  p9a <- Polygon(cbind(x=c(1, 1, 2, 2, 1), y=c(6, 7, 7, 6, 6)), hole=TRUE) # H
  p7a <- Polygon(cbind(x=c(14, 14, 15, 15, 14), y=c(13, 14, 14, 13, 13)), hole=TRUE) # H
  lp <- list(p1, p2, p13, p7, p7a, p6, p5, p4, p3, p8, p11, p12, p9, p9a, p10)
  spls <- SpatialPolygons(list(Polygons(lp, ID="1")))
  expect_equal(comment(spls), "FALSE")
  expect_null(comment(slot(spls, "polygons")[[1]]))
  spls_sfc <- sf::st_as_sfc(spls)
# rsbivand fork coerce_comments 2022-12-21
  spls_rt <- as(spls_sfc, "Spatial")
  expect_equal(comment(spls_rt), "TRUE")
  expect_equal(comment(slot(spls_rt, "polygons")[[1]]), "0 1 1 1 0 0 6 0 8 0 10 0 12")
})

test_that("as() can convert GEOMETRY to Spatial (#131)", {
  skip_if_not_installed("sp")
  single <- list(rbind(c(0,0), c(1,0), c(1, 1), c(0,1), c(0,0))) %>% st_polygon()
  multi <- list(single + 2, single + 4) %>% st_multipolygon()

  # polygons
  w <- st_sfc(single, multi)
  # class is GEOMETRY
  expect_s4_class(as(w, "Spatial"), "SpatialPolygons")
  expect_s4_class(as(st_cast(w, "MULTIPOLYGON"), "Spatial"), "SpatialPolygons")

  # lines
  lns <- st_cast(w, "MULTILINESTRING")
  expect_s4_class(as(lns, "Spatial"), "SpatialLines")

  expect_warning(ln <- st_cast(w, "LINESTRING"), "first ring")
  expect_s4_class(as(ln, "Spatial"), "SpatialLines")

  # points
  expect_warning(pt <- st_cast(w, "POINT"), "first coordinate")
  expect_s4_class(as(pt, "Spatial"), "SpatialPoints")

  pts <- st_cast(w, "MULTIPOINT")
  expect_s4_class(as(pts, "Spatial"), "SpatialMultiPoints")

  expect_warning(pt <- st_cast(w, "POINT"), "first coordinate")
  expect_s4_class(as(pt, "Spatial"), "SpatialPoints")
})

test_that("as_Spatial can convert sf (#519)", {
  skip_if_not_installed("sp")
  h <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  u <- as(h, "Spatial")
  s <- as_Spatial(h)
  g <- as_Spatial(st_geometry(h))

  identical(u, s)
  expect_s4_class(s, "SpatialPolygonsDataFrame")
  expect_s4_class(g, "SpatialPolygons")
  expect_s4_class(as(st_geometry(h), "Spatial"), "SpatialPolygons")
})

test_that("Can convert `XY` objects to sp", {
  skip_if_not_installed("sp")
  expect_s4_class(as(st_point(1:2), "Spatial"), "SpatialPoints")
  expect_error(as(st_point(1:3), "Spatial"))
  expect_error(as(st_point(1:4), "Spatial"))
})

test_that("Can't convert `M` dimension to sp", {
  skip_if_not_installed("sp")
  skip_if_not(sf_extSoftVersion()[["GDAL"]] >= "2.1.0")
  x <- read_sf(system.file("shape/storms_xyzm_feature.shp", package = "sf"), quiet = TRUE)
  expect_error(as_Spatial(x), "not supported by sp")
})

test_that("conversion to sp breaks on empty geometries", {
  skip_if_not_installed("sp")
  mysfc <- st_sfc(list(
    st_polygon(list(matrix(c(1,3,2,1,0,0,1,0), 4, 2))),
    st_polygon()  # empty polygon
  ))
  expect_error(as_Spatial(mysfc), "conversion failed")
})
