m <- rbind(c(0,0), c(1,0), c(1, 1), c(0,1), c(0,0))
s <- matrix(c(2, 0), 5, 2, byrow = TRUE)
cc <- list(
  points = list(
    single = m[1, ] %>% st_point(),
    multi = m %>% st_multipoint(),
    multi_empty = st_multipoint()
  ),
  lines = list(
    single = m %>% st_linestring(), 
    multi = list(m, m + s) %>% st_multilinestring()
  ),
  polygons = list(
    single = list(m + s)  %>% st_polygon(), 
    multi = list(list(m), list(m + s)) %>% st_multipolygon()
  )
)

test_that("st_cast() can coerce to MULTI* or GEOMETRY", {
  # st_cast
  # ======
  # points
  pt <- st_sfc(cc$points$single, cc$points$single)
  expect_s3_class(st_cast(pt), "sfc_POINT")
  pts <- st_sfc(cc$points$single, cc$points$multi, cc$points$multi_empty)
  expect_s3_class(st_cast(pts), "sfc_MULTIPOINT")
  expect_warning(pt <- st_cast(pts, "POINT"), "first coordinate")
  expect_s3_class(pt, "sfc_POINT")
  expect_s3_class(st_cast(pts, "MULTIPOINT"), "sfc_MULTIPOINT")
  expect_error(st_cast(pts, "LINESTRING"), "cannot create LINESTRING from POINT")
  expect_error(st_cast(pts, "MULTILINESTRING"), "cannot create MULTILINESTRING from POINT")
  expect_error(st_cast(pts, "POLYGON"), "cannot create POLYGON from POINT")
  expect_error(st_cast(pts, "MULTIPOLYGON"), "cannot create MULTIPOLYGON from POINT")

  # multipoints
  mp <- st_sfc(st_multipoint(m[1:4,]))
  expect_s3_class(mp, "sfc_MULTIPOINT")
  expect_s3_class(st_cast(mp, "MULTIPOINT"), "sfc_MULTIPOINT")
  expect_s3_class(st_cast(mp, "POINT"), "sfc_POINT")
  expect_silent(st_cast(mp, "POINT"))
  expect_warning(st_cast(mp[[1]], "POINT"), "point from first coordinate only")
  expect_s3_class(st_cast(mp, "POLYGON"), "sfc_POLYGON")
  expect_s3_class(st_cast(mp[[1]], "POLYGON"), "POLYGON")
  expect_s3_class(st_cast(mp, "LINESTRING"), "sfc_LINESTRING")
  expect_s3_class(st_cast(mp[[1]], "LINESTRING"), "LINESTRING")
  expect_error(st_cast(mp, "MULTIPOLYGON"), "smaller steps")
  expect_s3_class(st_cast(mp[[1]], "MULTIPOLYGON"), "MULTIPOLYGON")
  expect_s3_class(st_cast(mp, "MULTILINESTRING"), "sfc_MULTILINESTRING")
  expect_s3_class(st_cast(mp[[1]], "MULTILINESTRING"), "MULTILINESTRING")
  expect_error(st_cast(mp, "GEOMETRYCOLLECTION"), "smaller steps")
  expect_s3_class(st_cast(mp[[1]], "GEOMETRYCOLLECTION"), "GEOMETRYCOLLECTION")
  
  # lines
  ln <- st_sfc(cc$lines$single, cc$lines$single)
  expect_s3_class(st_cast(ln), "sfc_LINESTRING")
  lns <- st_sfc(cc$lines$single, cc$lines$multi)
  expect_s3_class(st_cast(lns), "sfc_MULTILINESTRING")
  expect_warning(ln <- st_cast(lns, "POINT"), "first coordinate")
  expect_s3_class(ln, "sfc_POINT") 
  expect_s3_class(st_cast(lns, "MULTIPOINT"), "sfc_MULTIPOINT")
  expect_warning(ln2 <- st_cast(lns, "LINESTRING"), "first linestring")
  expect_s3_class(ln2, "sfc_LINESTRING")
  expect_s3_class(st_cast(lns, "MULTILINESTRING"), "sfc_MULTILINESTRING")
  expect_s3_class(st_cast(lns, "POLYGON"), "sfc_POLYGON")
  expect_s3_class(st_cast(lns, "MULTIPOLYGON"), "sfc_MULTIPOLYGON")
  
  # polygons
  pl <- st_sfc(cc$polygons$single, cc$polygons$single)
  expect_s3_class(st_cast(pl), "sfc_POLYGON")
  pls <- st_sfc(cc$polygons$single, cc$polygons$multi)
  expect_s3_class(st_cast(pls), "sfc_MULTIPOLYGON")
  expect_warning(pl <- st_cast(pls, "POINT"), "first coordinate")
  expect_s3_class(pl, "sfc_POINT") 
  expect_s3_class(st_cast(pls, "MULTIPOINT"), "sfc_MULTIPOINT")
  expect_warning(pl2 <- st_cast(pls, "LINESTRING"), "first ring")
  expect_s3_class(pl2, "sfc_LINESTRING")
  expect_s3_class(st_cast(pls, "MULTILINESTRING"), "sfc_MULTILINESTRING")
  expect_warning(pl3 <- st_cast(pls, "POLYGON"), "first part")
  expect_s3_class(pl3, "sfc_POLYGON")
  expect_s3_class(st_cast(pls, "MULTIPOLYGON"), "sfc_MULTIPOLYGON")
  
  # mixed
  expect_s3_class(st_cast(st_sfc(cc$points$single, cc$lines$multi)), "sfc_GEOMETRY")
  expect_s3_class(st_cast(st_sfc(cc$lines$multi, cc$polygons$multi)), "sfc_GEOMETRY")
  
  expect_s3_class(st_cast(st_sfc(cc$lines$multi, cc$polygons$multi)), "sfc_GEOMETRY")
  expect_s3_class(st_cast(st_sfc(cc$points$multi, cc$polygons$multi)), "sfc_GEOMETRY")
  expect_s3_class(st_cast(st_sfc(cc$points$multi, cc$lines$multi, cc$polygons$multi)), 
                  "sfc_GEOMETRY")
  expect_s3_class(st_cast(st_sfc(list(cc$points$multi, cc$lines$multi, cc$polygons$multi))), 
                  "sfc_GEOMETRY")
})

test_that("st_cast preserves crs (#154)", {
  expect_identical(st_cast(st_sfc(cc$points$single, cc$lines$multi, crs = 4326)) %>% st_crs(), 
              st_sfc(cc$points$single, cc$lines$multi, crs = 4326) %>% st_crs())
})

test_that("st_cast can crack GEOMETRYCOLLECTION", {
  gc1 <- st_geometrycollection(list(st_linestring(rbind(c(0,0),c(1,1),c(2,1)))))
  gc2 <- st_geometrycollection(list(st_multilinestring(list(
    rbind(c(2,2),c(1,3)), rbind(c(0,0),c(1,1),c(2,1))))))
  gc3 <- st_geometrycollection(list(st_multilinestring(list(
    rbind(c(4,4),c(4,3)), rbind(c(2,2),c(2,1),c(3,1))))))
  gc4 <- st_geometrycollection(list(st_multipoint(rbind(c(1,5), c(4,3)))))
  
  sfc <- st_sfc(gc1, gc2, gc3)
  expect_s3_class(st_cast(sfc), "sfc_GEOMETRY")  # first, it cracks the collection
  expect_s3_class(st_cast(st_cast(sfc)), "sfc_MULTILINESTRING")  # then cast to multi*
#  expect_warning(expect_is(st_cast(sfc, "POINT"), "sfc_POINT"), "first coordinate")
#  expect_equal(st_cast(sfc, "POINT") %>% length, sfc %>% length)
# @etienne: I think this is more useful; attr(x, "ids") contains the original lengths
  expect_s3_class(st_cast(sfc, "MULTIPOINT"), "sfc_MULTIPOINT")
  #expect_is(st_cast(sfc, "LINESTRING"), "sfc_LINESTRING")
  expect_error(st_cast(sfc, "LINESTRING"))
  expect_error(st_cast(sfc, "MULTILINESTRING"))
  expect_s3_class(st_cast(sfc) %>% st_cast("MULTILINESTRING"), "sfc_MULTILINESTRING")

  # Can deal with GCs containing empty geometries - #1767
  gc5 <- st_as_sfc(
    c("GEOMETRYCOLLECTION (POLYGON ((5.5 0, 7 0, 7 -0.5, 6 -0.5, 5.5 0)))",
      "GEOMETRYCOLLECTION (POLYGON EMPTY)"
    )
  )
  expect_is(st_cast(gc5), "sfc_POLYGON")
  expect_equal(st_is_empty(st_cast(gc5)), c(FALSE, TRUE))
  
  sfc2 <- st_sfc(gc1, gc2, gc4) 
  expect_s3_class(sfc2 %>% st_cast, "sfc_GEOMETRY")
  expect_equal(sapply(sfc2 %>% st_cast, class)[2, ], c("LINESTRING", "MULTILINESTRING", "MULTIPOINT"))
})

test_that("can cast empty polygon (#1094)", {
  poly <- st_as_sfc(c('MULTIPOLYGON(((3 1,3 5,6 5,3 1)))', 'POLYGON EMPTY'))
  expect_s3_class(st_cast(poly), "sfc_MULTIPOLYGON")
})
