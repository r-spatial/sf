context("sf: wkb tests")

test_that("well-known binary is read correctly", {
  wkb = structure(list("01010000204071000000000000801A064100000000AC5C1441"), class = "WKB")
  g = st_as_sfc(wkb, EWKB = TRUE)[[1]]
  attr(g, "epsg") <- NULL
  expect_true(identical(g, st_point(c(181072,333611))))
  wkb = structure(list("0x01010000204071000000000000801A064100000000AC5C1441"), class = "WKB")
  g = st_as_sfc(wkb, EWKB = TRUE)[[1]]
  attr(g, "epsg") <- NULL
  expect_true(identical(g, st_point(c(181072,333611))))
})

test_that("Char -> Raw conversion in R and C++ gives identical results", {
  expect_identical(
    sf:::charToWKB(      "0x01010000204071000000000000801A064100000000AC5C1441"),
	sf:::HexToRaw(skip0x("0x01010000204071000000000000801A064100000000AC5C1441"))[[1]]
  )
  expect_identical(
    sf:::charToWKB("01010000204071000000000000801A064100000000AC5C1441"),
	sf:::HexToRaw( "01010000204071000000000000801A064100000000AC5C1441")[[1]]
  )
})
