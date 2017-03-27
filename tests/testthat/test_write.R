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
                         "nc",  # requires appropriate datum
                         "map") # doesn't support points
    for (ext in setdiff(names(extension_map[extension_map %in% drvs]),
                        excluded_drivers))
        st_write(meuse, paste0(tf, ".", ext))
})
