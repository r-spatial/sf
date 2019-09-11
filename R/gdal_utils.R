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
gdal_utils = function(util = "info", source, destination, options = character(0), 
		quiet = FALSE, processing = character(0), colorfilename = character(0)) {

	ret = switch(util,
			info = CPL_gdalinfo(source, options),
			warp = CPL_gdalwarp(source, destination, options),
			warper = CPL_gdal_warper(source, destination, as.integer(resampling_method(options))), # nocov
			rasterize = {  # nocov start
				overwrite = any(options %in% c("-of", "-a_nodata", "-init", "-a_srs", "-co", 
						"-te", "-tr", "-tap", "-ts", "-ot")) # https://gdal.org/programs/gdal_rasterize.html
				CPL_gdalrasterize(source, destination, options, overwrite)
			}, # nocov end
			translate = CPL_gdaltranslate(source, destination, options),
			vectortranslate = CPL_gdalvectortranslate(source, destination, options),
			buildvrt = CPL_gdalbuildvrt(source, destination, options),
			demprocessing = CPL_gdaldemprocessing(source, destination, options, processing, colorfilename),
			nearblack = CPL_gdalnearblack(source, destination, options),
			grid = CPL_gdalgrid(source, destination, options),
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
