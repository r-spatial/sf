suppressPackageStartupMessages(library(sf))

tif = system.file("tif/geomatrix.tif", package = "sf")

gdal_metadata(tif)
gdal_metadata(tif, NA_character_)
try(gdal_metadata(tif, "wrongDomain")) 
gdal_metadata(tif, c("IMAGE_STRUCTURE"))
try(length(gdal_metadata(tif, c("DERIVED_SUBDATASETS")))) # fails on Fedora 26

if (require(stars, quietly = TRUE)) {
  tif = system.file("tif/geomatrix.tif", package = "sf")
  r = read_stars(tif)
  d = (st_dimensions(r))
  gt =  c(1841001.75, 1.5, -5, 1144003.25, -5, -1.5)
  x1 = st_as_sfc(d, as_points = TRUE, use_cpp = TRUE, geotransform = gt)
  x2 = st_as_sfc(d, as_points = TRUE, use_cpp = FALSE, geotransform = gt)
  print(identical(x1, x2))
  y1 = st_as_sfc(d, as_points = FALSE, use_cpp = TRUE, geotransform = gt)
  y2 = st_as_sfc(d, as_points = FALSE, use_cpp = FALSE, geotransform = gt)
  print(identical(y1, y2))

  # rectilinear grid:
  m = matrix(1:20, nrow = 5, ncol = 4)
  x = c(0,0.5,1,2,4,5)
  y = c(0.3,0.5,1,2,2.2)
  r = st_as_stars(list(m = m), dimensions = st_dimensions(x = x, y = y, .raster = c("x", "y")))
  print(st_as_sfc(st_dimensions(r), as_points = TRUE))
  print(st_as_sfc(st_dimensions(r), as_points = FALSE))

  # curvilinear grid:
  lon = st_as_stars(matrix(1:5, 4, 5, byrow = TRUE))
  lat = st_as_stars(matrix(1:4, 4, 5))
  ll = c(X1 = lon, X2 = lat)
  curv = st_as_stars(st_as_stars(t(m)), curvilinear = setNames(ll, c("X1", "X2")))
  print(st_as_sfc(st_dimensions(curv), as_points = TRUE))
  print(st_as_sfc(st_dimensions(curv), as_points = FALSE))

  demo(nc, echo = FALSE, ask = FALSE)
  print(x <- st_rasterize(nc)) # default grid:
  print(p <- st_as_sf(x, as_points = FALSE)) # polygonize: follow raster boundaries
  print(p <- st_as_sf(x, as_points = FALSE, use_integer = TRUE)) # polygonize integers: follow raster boundaries
  print(try(p <- st_as_sf(x, as_points = TRUE))) # polygonize: contour, requies GDAL >= 2.4.0
  if (utils::packageVersion("stars") >= "0.2-1") {
    write_stars(read_stars(tif), tempfile(fileext = ".tif"))
    write_stars(read_stars(tif, proxy = TRUE), tempfile(fileext = ".tif"))
    write_stars(read_stars(tif, proxy = TRUE), tempfile(fileext = ".tif"), chunk_size = c(200,200))
  	na.tif = read_stars(system.file("tif/na.tif", package = "stars"))
  	write_stars(na.tif, "na.tif")
  	write_stars(na.tif, "na.tif", NA_value = -999)
  	na.tif = read_stars(system.file("tif/na.tif", package = "stars"), NA_value = -999)
  	write_stars(na.tif, "na.tif")
  	write_stars(na.tif, "na.tif", NA_value = -999)
  	na.tif = read_stars(system.file("tif/na.tif", package = "stars"), NA_value = -999, proxy = TRUE)
  	write_stars(na.tif, "na.tif")
  	write_stars(na.tif, "na.tif", NA_value = -999)
  }
  # https://github.com/mtennekes/tmap/issues/368
  if (utils::packageVersion("stars") > "0.4-0") {
    lc = system.file('tif/lc.tif', package = 'stars')
    if (lc != "") {
	    r = read_stars(lc, RAT = "Land Cover Class")
	    r <- droplevels(r)
    }
  }
}

r = gdal_read(tif)
gt = c(0,1,0,0,0,1)
gdal_inv_geotransform(gt)
rc = expand.grid(x=1:3, y = 1:3)
#(xy = xy_from_colrow(rc, gt))
#xy_from_colrow(xy, gt, inverse = TRUE)
crs <- gdal_crs(tif)

try(gdal_metadata("foo"))
gdal_metadata(tif)
