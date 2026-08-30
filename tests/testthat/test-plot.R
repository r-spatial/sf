test_that("plot.sf() support characters", {
  m <- list(rbind(c(0,0), c(1,0), c(1, 1), c(0,1), c(0,0))) |> st_polygon()
  x <- data.frame(a = c("a", "b"), stringsAsFactors = FALSE) |> st_as_sf(geom = st_sfc(m, m + 2))
  expect_silent(plot(x))
})

test_that("plot.sf warns on more than 15 attributes", {
  nc = st_read(system.file("shape/nc.shp", package="sf"), "nc", quiet = TRUE)
  x = cbind(nc,nc)
  expect_warning(plot(x))
})

test_that("plot.sf deals with key.length in cm", {
  nc = st_read(system.file("shape/nc.shp", package="sf"), "nc", quiet = TRUE)
  expect_silent(plot(nc[1], key.length = lcm(5), key.pos = 2))
  expect_silent(plot(nc[1], key.length = lcm(5), key.pos = 1))
  nc$f = factor(rep(c("a", "b"), each = 50))
  expect_silent(plot(nc["f"], key.length = lcm(5), key.pos = 2))
  expect_silent(plot(nc["f"], key.length = lcm(5), key.pos = 1))
})

test_that("plot.sf restores the graphics state after a key error", {
	nc = st_read(system.file("shape/nc.shp", package="sf"), "nc", quiet = TRUE)
	nc$toy = c("A,    B", rep("A", length.out = nrow(nc) - 1))

	png_file = tempfile(fileext = ".png")
	png(png_file, type = "cairo", width = 800, height = 600)
	par(family = "sans")
	on.exit({
		dev.off()
		unlink(png_file)
	})

	expect_silent(plot(nc["toy"]))
	expect_error(plot(nc["toy"], key.pos = 2), "key.width too small")
	expect_silent(plot(nc["toy"], key.pos = 2, key.width = lcm(6)))
})
