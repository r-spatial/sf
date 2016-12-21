context("sf: dplyr syntax")

suppressMessages(library(dplyr))
nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)

test_that("select ", {
  expect_true(nc %>% select_("AREA", attr(., "sf_column"))  %>% inherits("sf"))
  expect_true(nc %>% select(AREA) %>% inherits("sf"))
})
