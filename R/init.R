#' @importFrom utils head tail object.size
#' @importFrom stats na.omit
#' @importFrom methods as slotNames new
#' @import graphics
#' @importFrom Rcpp evalCpp
#' @useDynLib sf
NULL

setOldClass("sf")
setOldClass("sfc")
setOldClass("sfi")

.onAttach = function(libname, pkgname)
{
	packageStartupMessage(paste("Linking to GEOS", st_g_geosversion()))
}
