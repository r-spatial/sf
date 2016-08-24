context("sf")

test_that("bbox", {
  p1 = st_point(1:3)
  bb = bbox(p1)
  expect_true(bbox(p1) == c(1,1,2,2))
  expect_true(names(bbox(p1)) == c("xmin","xmax","ymin","ymax"))
  x = st_geometrycollection(list(st_point(1:2),st_linestring(matrix(1:4,2))))
  expect_true(bbox(x) == c(1,2,2,4))
  expect_true(names(bbox(x)) == c("xmin","xmax","ymin","ymax"))
})
