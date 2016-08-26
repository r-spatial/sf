#' @importFrom utils head tail
#' @importFrom stats na.omit
#' @importFrom methods as slotNames
#### @importFrom graphics plot axis box lines plot.new plot.window points
#' @import graphics
#' @importFrom sp geometry CRS Line Lines Polygon Polygons SpatialLines SpatialMultiPoints SpatialPoints SpatialPolygons addAttrToGeom
#' @import Rcpp
#' @useDynLib sf
NULL

setOldClass("sf")
setOldClass("sfc")
setOldClass("sfi")
