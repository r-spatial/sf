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
		sum = 13,
		stop(paste("unknown option:", options))
	)
}
# nocov end

#' Native interface to gdal utils
#' @name gdal_utils
#' @param util character; one of \code{info}, \code{warp}, \code{rasterize}, \code{translate}, \code{vectortranslate} (for ogr2ogr), \code{buildvrt}, \code{demprocessing}, \code{nearblack}, \code{grid}, \code{mdiminfo} and \code{mdimtranslate} (the last two requiring GDAL 3.1), \code{ogrinfo} (requiring GDAL 3.7), \code{footprint} (requiring GDAL 3.8)
#' @param source character; name of input layer(s); for \code{warp}, \code{buidvrt} or \code{mdimtranslate} this can be more than one
#' @param destination character; name of output layer
#' @param options character; options for the utility
#' @param config_options named character vector with GDAL config options, like \code{c(option1=value1, option2=value2)}
#' @param quiet logical; if \code{TRUE}, suppress printing the output for \code{info} and \code{mdiminfo}, and suppress printing progress
#' @param processing character; processing options for \code{demprocessing}
#' @param colorfilename character; name of color file for \code{demprocessing} (mandatory if \code{processing="color-relief"})
#' @param read_only logical; only for `ogrinfo`: if `TRUE`, source is opened in read-only mode
#' @return \code{info} returns a character vector with the raster metadata; all other utils return (invisibly) a logical indicating success (i.e., \code{TRUE}); in case of failure, an error is raised.
#' @export
#' @seealso \link{gdal_addo} for adding overlays to a raster file; \link{st_layers} to query geometry type(s) and crs from layers in a (vector) data source
#' @examples
#'
#' if (compareVersion(sf_extSoftVersion()["GDAL"], "2.1.0") == 1) {
#' # info utils can be used to list information about a raster
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
#' # conversion process. For example, we can reproject the features during the
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
		quiet = !(util %in% c("info", "gdalinfo", "ogrinfo", "vectorinfo", 
							  "mdiminfo")) || ("-multi" %in% options),
		processing = character(0), colorfilename = character(0),
		config_options = character(0), read_only = FALSE) {

	stopifnot(is.character(options), is.character(config_options))
	if (!quiet && "-multi" %in% options)
		stop("with -multi quiet should be set to FALSE")
#	if ("-co" %in% options)
#		options["-co" == options] = "-oo"
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

	quiet = as.logical(quiet)

	ret = switch(util,
			gdalinfo =, info = CPL_gdalinfo(if (missing(source)) character(0) else source, options, oo, config_options),
			vectorinfo =, ogrinfo = CPL_ogrinfo(if (missing(source)) character(0) else source, options, oo, config_options, 
												isTRUE(read_only) || "-ro" %in% options),
			warp = CPL_gdalwarp(source, destination, options, oo, doo, config_options, quiet, "-overwrite" %in% options),
			warper = CPL_gdal_warper(source, destination, as.integer(resampling_method(options)),
				oo, doo, config_options, quiet), # nocov
			rasterize = {  # nocov start
				overwrite = any(options %in% c("-of", "-a_nodata", "-init", "-a_srs", "-co",
						"-te", "-tr", "-tap", "-ts", "-ot")) # https://gdal.org/programs/gdal_rasterize.html
				CPL_gdalrasterize(source, destination, options, oo, doo, config_options, overwrite, quiet)
			}, # nocov end
			footprint = CPL_gdalfootprint(source, destination, options, oo, config_options, quiet),
			translate = CPL_gdaltranslate(source, destination, options, oo, config_options, quiet),
			vectortranslate = CPL_gdalvectortranslate(source, destination, options, oo, doo, config_options, quiet),
			buildvrt = CPL_gdalbuildvrt(if (missing(source)) character(0) else source, destination, options, oo, config_options, quiet),
			demprocessing = CPL_gdaldemprocessing(source, destination, options, processing, colorfilename, oo, config_options, quiet),
			nearblack = CPL_gdalnearblack(source, destination, options, oo, config_options, doo, quiet),
			grid = CPL_gdalgrid(source, destination, options, oo, config_options, quiet),
			mdiminfo = CPL_gdalmdiminfo(source, options, oo, config_options),
			mdimtranslate = CPL_gdalmdimtranslate(source, destination, options, oo, config_options, quiet),
			stop(paste("unknown util value for gdal_utils:", util))
		)

	if (util %in% c("info", "gdalinfo", "ogrinfo", "vectorinfo", "mdiminfo")) {
		if (! quiet)
			cat(ret)
		invisible(ret)
	} else { # ret indicates error:
		if (ret)
			stop(paste0("gdal_utils ", util, ": an error occured"))
		invisible(! ret) # success
	}
}
