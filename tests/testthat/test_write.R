context("sf: write")

library(sp)
data(meuse)
meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

test_that("sf can write to all writable formats", {
    # write to all formats available
    tf <- tempfile()
    drvs <- st_drivers()$name[sapply(st_drivers()$name, function(x) is_driver_can(x, operation = "write"))] %>% as.character()
    excluded_drivers = c("gps", # requires options
                         "gtm", # doesn't handle attributes
                         "nc",  # requires appropriate datum -> writes in 4326, below
                         "map") # doesn't support points
    for (ext in setdiff(names(extension_map[extension_map %in% drvs]),
                        excluded_drivers))
        st_write(meuse, paste0(tf, ".", ext))
	if ("netCDF" %in% drvs)
		st_write(st_transform(meuse, st_crs(4326)), paste0(tf, ".nc"))
})

test_that("sf can write units (#264)", {
    tf <- tempfile(fileext = ".gpkg")
    meuse[["length"]] <- meuse[["cadmium"]]
    units(meuse$length) <- units::make_unit("km")
    st_write(meuse, tf)
    disc <- st_read(tf)
    expect_is(disc[["length"]], "numeric")
    expect_equal(as.numeric(meuse[["length"]]), disc[["length"]])
})
