suppressPackageStartupMessages(library(sf))

if (require(maps, quietly = TRUE)) {
 m = map(xlim = c(4,9), ylim = c(48,55), fill = TRUE, plot = FALSE)
 st_as_sf(m)
 m = map(xlim = c(4,9), ylim = c(48,55), plot = FALSE)
 st_as_sf(m, fill = FALSE)
 st_as_sf(map(), fill = FALSE)
 st_as_sf(map(fill = TRUE))
 st_as_sf(map(), fill = FALSE, group = FALSE)
 st_as_sf(map(fill = TRUE), group = FALSE)
}
