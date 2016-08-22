context("sf")

test_that("double geometry columns trigger a warning", {
  g = st_sfc(list(st_point(1:2)))
  expect_warning(st_sf(a=3,g,g))
})
