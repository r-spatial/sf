#' @importFrom utils head tail object.size
#' @importFrom stats na.omit
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom methods as slotNames new
#' @importFrom grid convertUnit current.viewport linesGrob pathGrob pointsGrob polylineGrob unit viewport
#' @import graphics
#' @importFrom Rcpp evalCpp
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbGetQuery dbSendQuery dbReadTable
#' @useDynLib sf
NULL

setOldClass("sf")
setOldClass(c("sfc_POINT", "sfc"))
setOldClass(c("sfc_MULTIPOINT", "sfc"))
setOldClass(c("sfc_LINESTRING", "sfc"))
setOldClass(c("sfc_MULTILINESTRING", "sfc"))
setOldClass(c("sfc_POLYGON", "sfc"))
setOldClass(c("sfc_MULTIPOLYGON", "sfc"))
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
	CPL_geos_init()
}

.onUnload = function(libname, pkgname) {
	CPL_gdal_cleanup_all()
	CPL_geos_finish()
	if (file.exists(system.file("proj/nad.lst", package = "sf")[1])) {
		# nocov start
		Sys.setenv("PROJ_LIB"=get(".sf.PROJ_LIB", envir=.sf_cache))
		Sys.setenv("GDAL_DATA"=get(".sf.GDAL_DATA", envir=.sf_cache))
		# nocov end
	}
}

.onAttach = function(libname, pkgname) {
	packageStartupMessage(paste0("Linking to GEOS ", CPL_geos_version(), ", GDAL ", 
		CPL_gdal_version()))
}
