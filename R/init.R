#' @importFrom utils head tail object.size
#' @importFrom stats na.omit
#' @importFrom methods as slotNames new
#' @import graphics
#' @importFrom Rcpp evalCpp
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
	CPL_gdal_init()
}

.onUnload = function(libname, pkgname) {
	CPL_gdal_cleanup_all()
}

.onAttach = function(libname, pkgname) {
	packageStartupMessage(paste0("Linking to GEOS ", CPL_geos_version(), ", GDAL ", 
		CPL_gdal_version()))
}
