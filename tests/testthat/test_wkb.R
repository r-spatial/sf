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
    sf:::hex_to_raw(           "0x01010000204071000000000000801A064100000000AC5C1441"),
	sf:::CPL_hex_to_raw(skip0x("0x01010000204071000000000000801A064100000000AC5C1441"))[[1]]
  )
  expect_identical(
    sf:::hex_to_raw("01010000204071000000000000801A064100000000AC5C1441"),
	sf:::CPL_hex_to_raw( "01010000204071000000000000801A064100000000AC5C1441")[[1]]
  )
})

test_that("C++ conversion of big-endian fails on little-endian machine", {
  wkb = structure(list("0x00010000204071000000000000801A064100000000AC5C1441"), class = "WKB")
  if (.Platform$endian == "little")
    expect_error(st_as_sfc(wkb, EWKB = TRUE), "non native endian: use pureR = TRUE")
})
