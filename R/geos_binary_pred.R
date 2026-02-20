#' Geometric binary predicates on pairs of simple feature geometry sets
#'
#' @description
#'
#' For most predicates, a spatial index is built on argument `x`;
#' see \url{https://r-spatial.org/r/2017/06/22/spatial-index.html}.
#'
#' If `prepared = TRUE`, `x` contains POINT geometries, and `y` contains polygons,
#' then the polygon geometries are prepared, rather than the points.
#' @name geos_binary_pred
#' @family geometric binary predicates for two spatial objects
#' @param remove_self logical; if `TRUE` (and `y` is missing) return only indexes of geometries different from the current index; this can be used to omit self-intersections; see examples.
#'   This argument can be used for all geometry predicates
#' @examples
#' pts = st_sfc(st_point(c(.5,.5)), st_point(c(1.5, 1.5)), st_point(c(2.5, 2.5)))
#' pol = st_polygon(list(rbind(c(0,0), c(2,0), c(2,2), c(0,2), c(0,0))))
#' (lst = st_intersects(pts, pol))
#' (mat = st_intersects(pts, pol, sparse = FALSE))
#' # which points fall inside a polygon?
#' apply(mat, 1, any)
#' lengths(lst) > 0
#' # which points fall inside the first polygon?
#' st_intersects(pol, pts)[[1]]
#' # remove duplicate geometries:
#' p1 = st_point(0:1)
#' p2 = st_point(2:1)
#' p = st_sf(a = letters[1:8], geom = st_sfc(p1, p1, p2, p1, p1, p2, p2, p1))
#' st_equals(p)
#' st_equals(p, remove_self = TRUE)
#' (u = st_equals(p, retain_unique = TRUE))
#' # retain the records with unique geometries:
#' p[-unlist(u),]
NULL
