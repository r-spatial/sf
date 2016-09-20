#' @importFrom utils head tail object.size
#' @importFrom stats na.omit
#' @importFrom methods as slotNames new
#' @import graphics
#' @importFrom Rcpp evalCpp
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbGetQuery dbSendQuery
#' @useDynLib sf
NULL

setOldClass("sf")
setOldClass(c("sfc_POINT", "sfc"))
setOldClass(c("sfc_MULTIPOINT", "sfc"))
setOldClass(c("sfc_LINESTRING", "sfc"))
setOldClass(c("sfc_MULTILINESTRING", "sfc"))
setOldClass(c("sfc_POLYGON", "sfc"))
setOldClass(c("sfc_MULTIPOLYGON", "sfc"))
setOldClass("sfi")

.onLoad = function(libname, pkgname) {
	if (file.exists(system.file("proj/nad.lst", package = "sf")[1])) {
		prj = system.file("proj", package = "sf")[1]
		packageStartupMessage("Setting environment variable PROJ_LIB to ", prj, "\n")
		Sys.setenv("PROJ_LIB" = prj)
		gdl = system.file("gdal", package = "sf")[1]
		packageStartupMessage("Setting environment variable GDAL_DATA to ", gdl, "\n")
		Sys.setenv("GDAL_DATA" = gdl)
	}
	CPL_gdal_init()
}

.onUnload = function(libname, pkgname) {
	CPL_gdal_cleanup_all()
}

.onAttach = function(libname, pkgname) {
	packageStartupMessage(paste0("Linking to GEOS ", CPL_geos_version(), ", GDAL ", 
		CPL_gdal_version()))
}
