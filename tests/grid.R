# MULTIPOLYGONS
suppressPackageStartupMessages(library(sf))
library(grid)
nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
grid.newpage()
# pushViewport(viewport(width = 0.8, height = 0.8))
pushViewport(st_viewport(nc))
invisible(lapply(st_geometry(nc), function(x) grid.draw(st_as_grob(x, gp = gpar(fill = 'red')))))

# POLYGONS
# nc = st_read(system.file("gpkg/nc.gpkg", package="sf"), "nc.gpkg", type = 3)
nc = st_read(system.file("shape/nc.shp", package="sf"), type = 3, quiet = TRUE)
grid.newpage()
pushViewport(st_viewport(nc))
invisible(lapply(st_geometry(nc), function(x) grid.draw(st_as_grob(x, gp = gpar(fill = 'red')))))

# POINTS:
data(meuse, package = "sp")
meuse_sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992, relation_to_geometry = "field")
grid.newpage()
pushViewport(st_viewport(meuse_sf))
invisible(lapply(st_geometry(meuse_sf), 
	function(x) grid.draw(st_as_grob(x, gp = gpar(fill = 'red')))))


# MULTIPOINTS
mp = st_multipoint(cbind(runif(100), runif(100)))
grid.newpage()
pushViewport(st_viewport(mp))
grid.draw(st_as_grob(mp, gp = gpar(fill = 'red')))

# LINESTRING
ls = st_linestring(cbind(1:10, rnorm(10)))
grid.newpage()
pushViewport(st_viewport(ls))
grid.draw(st_as_grob(ls, gp = gpar(fill = 'red')))

# MULTILINESTRING
ls = st_multilinestring(list(cbind(1:10, 5+rnorm(10)), cbind(1:10, rnorm(10)), cbind(1:10, -5+rnorm(10))))
grid.newpage()
pushViewport(st_viewport(ls))
grid.draw(st_as_grob(ls, gp = gpar(fill = 'red')))

# POINTS, right aspect in Long/Lat:
meuse_ll = st_transform(meuse_sf, 4326)
grid.newpage()
pushViewport(st_viewport(meuse_ll))
invisible(lapply(st_geometry(meuse_ll), 
	function(x) grid.draw(st_as_grob(x, gp = gpar(fill = 'red')))))

# WRONG aspect:
st_crs(meuse_ll) = NA
grid.newpage()
pushViewport(st_viewport(meuse_ll))
invisible(lapply(st_geometry(meuse_ll), 
	function(x) grid.draw(st_as_grob(x, gp = gpar(fill = 'red')))))
