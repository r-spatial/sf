context("sf: dplyr syntax")

suppressMessages(library(dplyr))
nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)

test_that("select works", {
  expect_true(nc %>% select_("AREA", attr(., "sf_column"))  %>% inherits("sf"))
  expect_true(nc %>% select(AREA) %>% inherits("sf"))
})

suppressMessages(library(tidyr))
test_that("separate works", {
  expect_true(st_read(system.file("shape/nc.shp", package="sf")) %>%
    separate(CNTY_ID, c("a", "b"), sep = 2) %>% inherits("sf"))
})
