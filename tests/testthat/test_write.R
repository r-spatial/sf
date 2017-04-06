context("sf: write")

data(meuse, package = "sp")
meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

test_that("sf can write to all writable formats", {
    # write to all formats available
    tf <- tempfile()
    drvs <- st_drivers()$name[sapply(st_drivers()$name, 
		function(x) is_driver_can(x, operation = "write"))] %>% as.character()
    excluded_drivers = c("gps", # requires options
                         "gtm", # doesn't handle attributes
                         "nc",  # requires appropriate datum -> but writes in 4326, see below
                         "map", # doesn't support points
						 "ods") # generates valgrind error
    for (ext in setdiff(names(extension_map[extension_map %in% drvs]), excluded_drivers)) {
        st_write(meuse, paste0(tf, ".", ext), quiet = TRUE)
		cat(paste(ext, "\n"))
	}
	if ("netCDF" %in% drvs) {
		st_write(st_transform(meuse, st_crs(4326)), paste0(tf, ".nc"), quiet = TRUE)
		cat(paste(".nc", "\n"))
	}
})

test_that("sf can write units (#264)", {
    tf <- tempfile(fileext = ".gpkg")
    meuse[["length"]] <- meuse[["cadmium"]]
    units(meuse$length) <- units::make_unit("km")
    st_write(meuse, tf, quiet = TRUE)
    disc <- st_read(tf, quiet = TRUE)
    expect_is(disc[["length"]], "numeric")
    expect_equal(as.numeric(meuse[["length"]]), disc[["length"]])
})
