context("sf: dplyr syntax")

suppressMessages(library(dplyr))
nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)

test_that("select works", {
  expect_true(nc %>% select_("AREA", attr(., "sf_column"))  %>% inherits("sf"))
  expect_true(nc %>% select(AREA) %>% inherits("sf"))
})

suppressMessages(library(tidyr))
test_that("separate and unite work", {
  expect_true(nc %>% separate(CNTY_ID, c("a", "b"), sep = 2) %>% inherits("sf"))
  expect_true(nc %>% separate(CNTY_ID, c("a", "b"), sep = 2) %>%
	unite(CNTY_ID_NEW, c("a", "b"), sep = "") %>% inherits("sf"))
})

test_that("group/ungroup works", {
 tbl = tibble(a = c(1,1,2,2), g = st_sfc(st_point(0:1), st_point(1:2), st_point(2:3), st_point(3:4)))
 d = st_sf(tbl)
 e <- d %>% group_by(a) %>% ungroup
 expect_equal(as.data.frame(d), as.data.frame(e))
})

test_that("sample_n etc work", {
 tbl = tibble(a = c(1,1,2,2), g = st_sfc(st_point(0:1), st_point(1:2), st_point(2:3), st_point(3:4)))
 d = st_sf(tbl)
 sample_n(d, 2)
 sample_frac(d, .5)
 d %>% group_by(a) %>% nest
})

test_that("st_intersection of tbl returns tbl", {
 nc = read_sf(system.file("shape/nc.shp", package="sf"))
 nc = st_transform(nc[1:3,], 3857)
 st_agr(nc) = "constant"
 expect_is(nc, "tbl_df")
 expect_is(st_intersection(nc[1:3], nc[4:6]), "tbl_df")
})

test_that("unnest works", {
  skip_if_not_installed("tidyr")
  skip_if_not(utils::packageVersion("tidyr") > "0.7.2")
  nc = read_sf(system.file("shape/nc.shp", package = "sf")) %>%
    slice(1:2) %>%
    transmute(y = list(c("a"), c("b", "c")))
  unnest_explicit = unnest(nc, y)
  unnest_implicit = unnest(nc)
  # The second row is duplicated because the "b" and "c" become separate rows
  expected = nc[c(1,2,2), ] %>% mutate(y = c("a", "b", "c"))
  # Would use expect_equal, but doesn't work with geometry cols
  expect_identical(unnest_explicit, expected)
  expect_identical(unnest_implicit, expected)
})
