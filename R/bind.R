#' Bind rows (features) of sf objects
#'
#' Bind rows (features) of sf objects
#' @param ... objects to bind
#' @param deparse.level integer; see \link[base]{rbind}
#' @name bind
#' @details both \code{rbind} and \code{cbind} have non-standard method dispatch (see \link[base]{cbind}): the \code{rbind} or \code{cbind} method for \code{sf} objects is only called when all arguments to be binded are of class \code{sf}. 
#' @export
#' @examples
#' crs = st_crs(3857)
#' a = st_sf(a=1, geom = st_sfc(st_point(0:1)), crs = crs)
#' b = st_sf(a=1, geom = st_sfc(st_linestring(matrix(1:4,2))), crs = crs)
#' c = st_sf(a=4, geom = st_sfc(st_multilinestring(list(matrix(1:4,2)))), crs = crs)
#' rbind(a,b,c)
#' rbind(a,b) %>% st_cast("POINT")
#' rbind(a,b) %>% st_cast("MULTIPOINT")
#' rbind(b,c) %>% st_cast("LINESTRING")
rbind.sf = function(..., deparse.level = 1) {
	dots = list(...)
	crs0 = st_crs(dots[[1]])
	if (length(dots) > 1L) { # check all crs are equal...
		equal_crs = vapply(dots[-1L], function(x) st_crs(x) == crs0, TRUE)
		if (!all(equal_crs))
			stop("arguments have different crs", call. = FALSE)
	}
	ret = st_sf(rbind.data.frame(...), crs = crs0)
	st_geometry(ret) = st_sfc(st_geometry(ret)) # might need to reclass to GEOMETRY
	attr(ret[[ attr(ret, "sf_column") ]], "bbox") = c(st_bbox(ret)) # recompute
	ret
}

#' Bind columns (variables) of sf objects
#'
#' Bind columns (variables) of sf objects
#' @name bind
#' @param sf_column_name character; specifies active geometry; passed on to \link{st_sf}
#' @return \code{cbind} called with multiple \code{sf} objects warns about multiple geometry columns present when the geometry column to use is not specified by using argument \code{sf_column_name}; see also \link{st_sf}.
#' @export
#' @details If you need to \code{cbind} e.g. a \code{data.frame} to an \code{sf}, use \link{data.frame} directly and use \link{st_sf} on its result, or use \link[dplyr]{bind_cols}; see examples.
#' @examples
#' cbind(a,b,c) # warns
#' dplyr::bind_cols(a,b)
#' c = st_sf(a=4, geomc = st_sfc(st_multilinestring(list(matrix(1:4,2)))), crs = crs)
#' cbind(a,b,c, sf_column_name = "geomc")
#' df = data.frame(x=3)
#' st_sf(data.frame(c, df))
#' dplyr::bind_cols(c, df)
cbind.sf = function(..., deparse.level = 1, sf_column_name = NULL) {
	# todo: handle st_agr?
	st_sf(data.frame(...), sf_column_name = sf_column_name)
}

#' @name bind
#' @export
#' @details \code{st_bind_cols} is deprecated; use \code{cbind} instead.
st_bind_cols = function(...) {
	.Deprecated("cbind")
	cbind.sf(...)
}
