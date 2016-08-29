#' @importFrom utils head tail
#' @importFrom stats na.omit
#' @importFrom methods as slotNames new
#### @importFrom graphics plot axis box lines plot.new plot.window points
#' @import graphics
#### @importFrom sp geometry CRS Line Lines Polygon Polygons SpatialLines SpatialMultiPoints SpatialPoints SpatialPolygons addAttrToGeom plot bbox
#' @importFrom sp geometry bbox
#' @importFrom Rcpp evalCpp
#' @useDynLib sf
NULL

setOldClass("sf")
setOldClass("sfc")
setOldClass("sfi")

setOldClass("POINT")
setOldClass("MULTIPOINT")
setOldClass("LINESTRING")
setOldClass("POLYGON")
setOldClass("MULTILINESTRING")
setOldClass("MULTIPOLYGON")
setOldClass("GEOMETRYCOLLECTION")
