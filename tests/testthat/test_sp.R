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
