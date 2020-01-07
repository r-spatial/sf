# nocov start
resampling_method = function(option = "near") {
	if (length(option) != 1)
		stop("warper options should have length 1")
	switch(option,
		near = 0,
		bilinear = 1,
		cubic = 2,
		cubicspline = 3,
		lanczos = 4,
		average = 5,
		mode = 6,
		max = 8,
		min = 9,
		med = 10,
		q1 = 11,
		q3 = 12,
		stop(paste("unknown option:", options))
	)
}
# nocov end

#' Native interface to gdal utils
#' @name gdal_utils
#' @param util character; one of \code{info}, \code{warp}, \code{rasterize}, \code{translate}, \code{vectortranslate}, \code{buildvrt}, \code{demprocessing}, \code{nearblack}, \code{grid}
#' @param source character; name of input layer(s); for \code{warp} or \code{buidvrt} this can be more than one
#' @param destination character; name of output layer
#' @param options character; raster layer read options
#' @param quiet logical; if \code{TRUE}, suppress printing of output for \code{info}
#' @param processing character; processing options for \code{demprocessing}
#' @param colorfilename character; name of color file for \code{demprocessing} (mandatory if \code{processing="color-relief"})
#' @return \code{info} returns a character vector with the raster metadata; all other utils return (invisibly) a logical indicating success (i.e., \code{TRUE}); in case of failure, an error is raised.
#' @export
#' @examples
#'
#' if (sf_extSoftVersion()["GDAL"] > "2.1.0") {
#' # info utils can be used to list information about about a raster
#' # dataset. More info: https://gdal.org/programs/gdalinfo.html
#' in_file <- system.file("tif/geomatrix.tif", package = "sf")
#' gdal_utils("info", in_file, options = c("-mm", "-proj4"))
#'
#' # vectortranslate utils can be used to convert simple features data between
#' # file formats. More info: https://gdal.org/programs/ogr2ogr.html
#' in_file <- system.file("shape/storms_xyz.shp", package="sf")
#' out_file <- paste0(tempfile(), ".gpkg")
#' gdal_utils(
#'   util = "vectortranslate",
#'   source = in_file,
#'   destination = out_file, # output format must be specified for GDAL < 2.3
#'   options = c("-f", "GPKG")
#' )
#' # The parameters can be specified as c("name") or c("name", "value"). The
#' # vectortranslate utils can perform also various operations during the
#' # conversion process. For example we can reproject the features during the
#' # translation.
#' gdal_utils(
#'   util = "vectortranslate",
#'   source = in_file,
#'   destination = out_file,
#'   options = c(
#'   "-f", "GPKG", # output file format for GDAL < 2.3
#'   "-s_srs", "EPSG:4326", # input file SRS
#'   "-t_srs", "EPSG:2264", # output file SRS
#'   "-overwrite"
#'   )
#' )
#' st_read(out_file)
#' # The parameter s_srs had to be specified because, in this case, the in_file
#' # has no associated SRS.
#' st_read(in_file)
#' }
gdal_utils = function(util = "info", source, destination, options = character(0),
		quiet = FALSE, processing = character(0), colorfilename = character(0)) {

	if ("-co" %in% options)
		options["-co" == options] = "-oo"
	if ("-oo" %in% options) { # -oo indicating opening options
		ooi = which("-oo" == options)
		oo = options[ooi + 1]
		options = options[-c(ooi, ooi+1)]
	} else
		oo = character(0)
	if ("-doo" %in% options) { # -oo indicating destination opening options
		ooi = which("-doo" == options)
		doo = options[ooi + 1]
		options = options[-c(ooi, ooi+1)]
	} else
		doo = character(0)

	if ("-doo" %in% options) # -oo indicating opening options
		stop("-doo options not (yet) supported; consider raising an issue") # nocov

	ret = switch(util,
			info = CPL_gdalinfo(source, options, oo),
			warp = CPL_gdalwarp(source, destination, options, oo, doo),
			warper = CPL_gdal_warper(source, destination, as.integer(resampling_method(options)), oo, doo), # nocov
			rasterize = {  # nocov start
				overwrite = any(options %in% c("-of", "-a_nodata", "-init", "-a_srs", "-co",
						"-te", "-tr", "-tap", "-ts", "-ot")) # https://gdal.org/programs/gdal_rasterize.html
				CPL_gdalrasterize(source, destination, options, oo, doo, overwrite)
			}, # nocov end
			translate = CPL_gdaltranslate(source, destination, options, oo),
			vectortranslate = CPL_gdalvectortranslate(source, destination, options, oo, doo),
			buildvrt = CPL_gdalbuildvrt(source, destination, options, oo),
			demprocessing = CPL_gdaldemprocessing(source, destination, options, processing, colorfilename, oo),
			nearblack = CPL_gdalnearblack(source, destination, options, oo, doo),
			grid = CPL_gdalgrid(source, destination, options, oo),
			stop(paste("unknown util value for gdal_utils:", util))
		)

	if (util == "info") {
		if (! quiet)
			cat(ret)
		invisible(ret)
	} else {
		# ret indicates error:
		if (ret)
			stop(paste0("gdal_utils ", util, ": an error occured"))
		invisible(! ret) # success
	}
}
