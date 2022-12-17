test_that("well-known binary is read correctly", {
  wkb = structure(list("01010000204071000000000000801A064100000000AC5C1441"), class = "WKB")
  g = st_as_sfc(wkb, EWKB = TRUE)[[1]]
  attr(g, "epsg") <- NULL
  expect_true(identical(g, st_point(c(181072,333611))))
  wkb = structure(list("0x01010000204071000000000000801A064100000000AC5C1441"), class = "WKB")
  g = st_as_sfc(wkb, EWKB = TRUE)[[1]]
  attr(g, "epsg") <- NULL
  expect_true(identical(g, st_point(c(181072,333611))))
  wkb = structure(list("0x01010000204071000000000000801A064100000000AC5C1441"))
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
    sf:::hex_to_raw(     "01010000204071000000000000801A064100000000AC5C1441"),
	sf:::CPL_hex_to_raw( "01010000204071000000000000801A064100000000AC5C1441")[[1]]
  )
  expect_identical( # upper case, lower case:
    sf:::CPL_hex_to_raw("01010000204071000000000000801A064100000000AC5C1441"),
	sf:::CPL_hex_to_raw("01010000204071000000000000801a064100000000ac5c1441")
  )
  expect_error(sf:::CPL_hex_to_raw("01010000204071000000000000801A064100000000AC5C144z")) # note the z
})

test_that("Reading of big-endian and little-endian gives the same result", {
  x = structure(list("0x01010000204071000000000000801A064100000000AC5C1441"), class = "WKB")
  y = structure(list("0x00200000010000714041061A800000000041145CAC00000000"), class = "WKB")
  expect_identical(st_as_sfc(x, EWKB = TRUE), st_as_sfc(y, EWKB = TRUE))
  expect_identical(st_as_sfc(x, EWKB = TRUE, pureR = TRUE), st_as_sfc(y, EWKB = TRUE, pureR = TRUE))
  expect_identical(st_as_sfc(x, EWKB = TRUE), st_as_sfc(y, EWKB = TRUE, pureR = TRUE))
})

test_that("Reading of truncated buffers results in a proper error", {
  skip_on_os("mac") # doesn't give the message thrown

  wkb = structure(list("010100002040710000"), class = "WKB")
  expect_error(st_as_sfc(wkb, EWKB = TRUE), "WKB buffer too small. Input file corrupt?")
  wkb = structure(list("01"), class = "WKB")
  expect_error(st_as_sfc(wkb, EWKB = FALSE), "WKB buffer too small. Input file corrupt?")
  wkb = structure(list("0x01010000204071000000000000801A064100000000AC5C144"), class = "WKB")
  expect_error(st_as_sfc(wkb, EWKB = TRUE), "WKB buffer too small. Input file corrupt?")
})

test_that("st_as_sfc() honors crs argument", {
  raw = st_as_binary(st_point(c(26e5, 12e5)))

  list = list(raw)
  blob = blob::blob(raw)
  wkb = as_wkb(list)

  expect_identical(st_as_sfc(raw, crs = 2056), st_as_sfc(wkb, crs = 2056))
  expect_identical(st_as_sfc(list, crs = 2056), st_as_sfc(wkb, crs = 2056))
  expect_identical(st_as_sfc(blob, crs = 2056), st_as_sfc(wkb, crs = 2056))
})
