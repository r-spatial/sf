context("sf: wkt")

test_that("well-known text", {
  gcol <- st_geometrycollection(list(st_point(1:2), st_linestring(matrix(1:4,2))))
  expect_message(x <- print(gcol), 
                 "GEOMETRYCOLLECTION\\(POINT\\(1 2\\), LINESTRING\\(1 3, 2 4\\)\\)", all = TRUE)
  expect_equal(x, "GEOMETRYCOLLECTION(POINT(1 2), LINESTRING(1 3, 2 4))")
  
  p1 = st_point(1:3)
  p2 = st_point(5:7)
  sfc = st_sfc(p1,p2)
  expect_identical(st_as_text(sfc), c("POINTZ(1 2 3)", "POINTZ(5 6 7)"))
})
