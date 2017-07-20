context("sf: sp conversion tests")

test_that("we can convert points & lines to and from sp objects", {
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

test_that("as() can convert GEOMETRY to Spatial (#131)", {
  single <- list(rbind(c(0,0), c(1,0), c(1, 1), c(0,1), c(0,0))) %>% st_polygon()
  multi <- list(single + 2, single + 4) %>% st_multipolygon()
  
  # polygons
  w <- st_sfc(single, multi)
  # class is GEOMETRY
  expect_is(as(w, "Spatial"), "SpatialPolygons")
  expect_is(as(st_cast(w, "MULTIPOLYGON"), "Spatial"), "SpatialPolygons")

  # lines
  lns <- st_cast(w, "MULTILINESTRING")
  expect_is(as(lns, "Spatial"), "SpatialLines")
  
  expect_warning(ln <- st_cast(w, "LINESTRING"), "first ring")
  expect_is(as(ln, "Spatial"), "SpatialLines")
  
  # points
  expect_warning(pt <- st_cast(w, "POINT"), "first coordinate")
  expect_is(as(pt, "Spatial"), "SpatialPoints")
  
  pts <- st_cast(w, "MULTIPOINT")
  expect_is(as(pts, "Spatial"), "SpatialMultiPoints")
  
  expect_warning(pt <- st_cast(w, "POINT"), "first coordinate")
  expect_is(as(pt, "Spatial"), "SpatialPoints")
})
