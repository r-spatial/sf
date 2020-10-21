context("sf: read tests")

# note that the directory here is relative
test_that("we can read a zipped shapefile using st_read", {
	nc <- st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267, quiet = TRUE)
	expect_identical(class(nc), c("sf", "data.frame"))
	expect_equal(dim(nc), c(100, 15))
})
