#' @importFrom utils head tail object.size str
#' @importFrom stats runif aggregate na.omit
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom methods as slotNames new slot
#' @importFrom grid convertUnit current.viewport linesGrob pathGrob pointsGrob polylineGrob unit viewport nullGrob
#' @import graphics
#' @importFrom grDevices rgb
#' @importFrom Rcpp evalCpp
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbGetQuery dbSendQuery dbReadTable dbExecute
#' @importFrom units as_units set_units make_unit_label
#' @importFrom classInt classIntervals
#' @useDynLib sf
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

setOldClass("sf")
setOldClass(c("sfc_POINT", "sfc"))
setOldClass(c("sfc_MULTIPOINT", "sfc"))
setOldClass(c("sfc_LINESTRING", "sfc"))
setOldClass(c("sfc_MULTILINESTRING", "sfc"))
setOldClass(c("sfc_POLYGON", "sfc"))
setOldClass(c("sfc_MULTIPOLYGON", "sfc"))
setOldClass(c("sfc_GEOMETRY", "sfc"))
setOldClass("sfg")

.sf_cache <- new.env(FALSE, parent=globalenv())

.onLoad = function(libname, pkgname) {
	if (file.exists(system.file("proj/nad.lst", package = "sf")[1])) {
		# nocov start
  		assign(".sf.PROJ_LIB", Sys.getenv("PROJ_LIB"), envir=.sf_cache)
		prj = system.file("proj", package = "sf")[1]
		Sys.setenv("PROJ_LIB" = prj)
		assign(".sf.GDAL_DATA", Sys.getenv("GDAL_DATA"), envir=.sf_cache)
		gdl = system.file("gdal", package = "sf")[1]
		Sys.setenv("GDAL_DATA" = gdl)
		# nocov end
	}
	CPL_gdal_init()
	register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse)
}

.onUnload = function(libname, pkgname) {
	CPL_gdal_cleanup_all()
	if (file.exists(system.file("proj/nad.lst", package = "sf")[1])) {
		# nocov start
		Sys.setenv("PROJ_LIB"=get(".sf.PROJ_LIB", envir=.sf_cache))
		Sys.setenv("GDAL_DATA"=get(".sf.GDAL_DATA", envir=.sf_cache))
		# nocov end
	}
}

.onAttach = function(libname, pkgname) {
	m = paste0("Linking to GEOS ", CPL_geos_version(), ", GDAL ", CPL_gdal_version(), ", proj.4 ", CPL_proj_version())
	packageStartupMessage(m)
}

#' Provide the external dependencies versions of the libraries linked to sf
#' 
#' Provide the external dependencies versions of the libraries linked to sf
#' @export
sf_extSoftVersion = function() {
	structure(c(CPL_geos_version(), CPL_gdal_version(), CPL_proj_version(),
		ifelse(CPL_gdal_with_geos(), "true", "false")),
		names = c("GEOS", "GDAL", "proj.4", "GDAL_with_GEOS"))
}
