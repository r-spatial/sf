#' try to make an invalid geometry valid
#'
#' try to make an invalid geometry valid
#' @param x object of class \code{sfg}, \code{sfg} or \code{sf}
#' @return object of the same class as \code{x}
#' @details uses the \code{lwgeom_makevalid} method also used by PostGIS' \code{ST_makevalid}.
#' @examples
#' x = st_sfc(st_polygon(list(rbind(c(0,0),c(0.5,0),c(0.5,0.5),c(0.5,0),c(1,0),c(1,1),c(0,1),c(0,0)))))
#' if (!is.na(sf_extSoftVersion()["lwgeom"])) {
#'   suppressWarnings(st_is_valid(x))
#'   y = st_make_valid(x)
#'   st_is_valid(y)
#'   y %>% st_cast()
#' }
#' @export
st_make_valid = function(x) UseMethod("st_make_valid")

#' @export
st_make_valid.sfg = function(x) {
	st_make_valid(st_geometry(x))[[1]]
}

#' @export
st_make_valid.sfc = function(x) {
	st_sfc(CPL_make_valid(x), crs = st_crs(x))
}

#' @export
st_make_valid.sf = function(x) {
	st_geometry(x) = st_make_valid(st_geometry(x))
	x
}
