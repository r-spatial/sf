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
			rasterize = CPL_gdalrasterize(source, destination, options),
			translate = CPL_gdaltranslate(source, destination, options),
			vectortranslate = CPL_gdalvectortranslate(source, destination, options),
			buildvrt = CPL_gdalbuildvrt(source, destination, options),
			demprocessing = CPL_gdaldemprocessing(source, destination, options, processing, colorfilename),
			nearblack = CPL_gdalnearblack(source, destination, options),
			grid = CPL_gdalgrid(source, destination, options),
			stop(paste("unknown value for util:", util))
		)

	if (util == "info") {
		if (! quiet)
			cat(ret)
		invisible(ret)
	} else {
		# ret indicates error:
		if (ret)
			stop("gdal_utils: an error occured")
		invisible(! ret) # success
	}
}
