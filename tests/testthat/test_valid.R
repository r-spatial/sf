context("st_is_valid")

# https://github.com/r-spatial/sf/issues/1760
test_that("st_is_valid", {
  p1 <- st_as_sfc("POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))")
  p2 <- st_as_sfc("POLYGON((0 0, 0 10, 10 0))")
  set1 <- c(p1, p2, p1, p2, p1, p2)
  expect_identical(st_is_valid(set1), c(TRUE, NA))
  expect_identical(st_is_valid(set1, reason = TRUE), 
    c("Valid Geometry", NA, "Valid Geometry", NA, "Valid Geometry", NA))
})


