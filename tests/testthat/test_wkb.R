context("sf")

test_that("double geometry columns trigger a warning", {
  wkb = structure(list("01010000204071000000000000801A064100000000AC5C1441"), class = "WKB")
  g = st_as_sfc(wkb, EWKB = TRUE)[[1]]
  attr(g, "epsg") <- NULL
  expect_true(identical(g, st_point(c(181072,333611))))
  wkb = structure(list("0x01010000204071000000000000801A064100000000AC5C1441"), class = "WKB")
  g = st_as_sfc(wkb, EWKB = TRUE)[[1]]
  attr(g, "epsg") <- NULL
  expect_true(identical(g, st_point(c(181072,333611))))
})
