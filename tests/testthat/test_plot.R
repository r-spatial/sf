context("sf: plot")

test_that("plot.sf() support characters", {
  m <- list(rbind(c(0,0), c(1,0), c(1, 1), c(0,1), c(0,0))) %>% st_polygon()
  x <- data.frame(a = c("a", "b"), stringsAsFactors = FALSE) %>% st_as_sf(geom = st_sfc(m, m + 2))
  expect_silent(plot(x))
})

test_that("plot.sf warns on more than 15 attributes", {
  demo(nc, ask = FALSE, echo = FALSE)
  x = cbind(nc,nc)
  expect_warning(plot(x))
})
