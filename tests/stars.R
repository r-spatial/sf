suppressPackageStartupMessages(library(sf))

tif = system.file("tif/geomatrix.tif", package = "sf")

gdal_metadata(tif)
gdal_metadata(tif, NA_character_)
try(gdal_metadata(tif, "wrongDomain")) 
gdal_metadata(tif, c("IMAGE_STRUCTURE"))
gdal_metadata(tif, c("DERIVED_SUBDATASETS"))

# library(stars)
d = structure(list(x = structure(list(from = 1, to = 20, offset = 1841001.75, 
    delta = 1.5, geotransform = c(1841001.75, 1.5, -5, 1144003.25, 
    -5, -1.5), refsys = "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs ", 
    point = TRUE, values = NULL), .Names = c("from", "to", "offset", 
"delta", "geotransform", "refsys", "point", "values"), class = "dimension"), 
    y = structure(list(from = 1, to = 20, offset = 1144003.25, 
        delta = -1.5, geotransform = c(1841001.75, 1.5, -5, 1144003.25, 
        -5, -1.5), refsys = "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs ", 
        point = TRUE, values = NULL), .Names = c("from", "to", 
    "offset", "delta", "geotransform", "refsys", "point", "values"
    ), class = "dimension")), .Names = c("x", "y"), class = "dimensions")
x1 = st_as_sfc(d, as_points = TRUE, use_cpp = TRUE)
x2 = st_as_sfc(d, as_points = TRUE, use_cpp = FALSE)
identical(x1, x2)
y1 = st_as_sfc(d, as_points = FALSE, use_cpp = TRUE)
y2 = st_as_sfc(d, as_points = FALSE, use_cpp = FALSE)
identical(y1, y2)
