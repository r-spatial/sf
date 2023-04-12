chk_equal_crs = function(dots) {
	if (length(dots) > 1L) {
		crs0 = st_crs(dots[[1]])
		vapply(dots[-1L], function(x) {
				if (st_crs(x) != crs0) 
					stop("arguments have different crs", call. = FALSE)
				TRUE
			}, TRUE)
	}
	NULL
}

#' Bind rows (features) of sf objects
#'
#' Bind rows (features) of sf objects
#' @param ... objects to bind; note that for the rbind and cbind methods, all objects have to be of class \code{sf}; see \link{dotsMethods}
#' @param deparse.level integer; see \link{rbind}
#' @name bind
#' @details both \code{rbind} and \code{cbind} have non-standard method dispatch (see \link[base]{cbind}): the \code{rbind} or \code{cbind} method for \code{sf} objects is only called when all arguments to be binded are of class \code{sf}.
#' @export
#' @examples
#' crs = st_crs(3857)
#' a = st_sf(a=1, geom = st_sfc(st_point(0:1)), crs = crs)
#' b = st_sf(a=1, geom = st_sfc(st_linestring(matrix(1:4,2))), crs = crs)
#' c = st_sf(a=4, geom = st_sfc(st_multilinestring(list(matrix(1:4,2)))), crs = crs)
#' rbind(a,b,c)
#' rbind(a,b)
#' rbind(a,b)
#' rbind(b,c)
rbind.sf = function(..., deparse.level = 1) {
	dots = list(...)
	dots = dots[!sapply(dots, is.null)]
	nr = sapply(dots, NROW)
	sf_column = if (any(nr > 0))
			attr(dots[[ which(nr > 0)[1] ]], "sf_column")
		else
			NULL
	chk_equal_crs(dots)
	ret = st_sf(rbind.data.frame(...), crs = st_crs(dots[[1]]), sf_column_name = sf_column)
	st_geometry(ret) = st_sfc(st_geometry(ret)) # might need to reclass to GEOMETRY
	bb = do.call(rbind, lapply(dots, st_bbox))
	bb = bb_wrap(c(min(bb[,1L], na.rm = TRUE), min(bb[,2L], na.rm = TRUE),
		  max(bb[,3L], na.rm = TRUE), max(bb[,4L], na.rm = TRUE)))
	attr(ret[[ attr(ret, "sf_column") ]], "bbox") = bb
	ret
}

#' Bind columns (variables) of sf objects
#'
#' Bind columns (variables) of sf objects
#' @name bind
#' @param sf_column_name character; specifies active geometry; passed on to \link{st_sf}
#' @return \code{cbind} called with multiple \code{sf} objects warns about multiple geometry columns present when the geometry column to use is not specified by using argument \code{sf_column_name}; see also \link{st_sf}.
#' @export
#' @details If you need to \code{cbind} e.g. a \code{data.frame} to an \code{sf}, use \link{data.frame} directly and use \link{st_sf} on its result, or use \link[dplyr:bind]{bind_cols}; see examples.
#' @examples
#' cbind(a,b,c) # warns
#' if (require(dplyr, quietly = TRUE))
#'   dplyr::bind_cols(a,b)
#' c = st_sf(a=4, geomc = st_sfc(st_multilinestring(list(matrix(1:4,2)))), crs = crs)
#' cbind(a,b,c, sf_column_name = "geomc")
#' df = data.frame(x=3)
#' st_sf(data.frame(c, df))
#' if (require(dplyr, quietly = TRUE))
#'   dplyr::bind_cols(c, df)
cbind.sf = function(..., deparse.level = 1, sf_column_name = NULL) {
	# TODO: handle st_agr?
	st_sf(data.frame(...), sf_column_name = sf_column_name)
}

#' @name bind
#' @export
#' @details \code{st_bind_cols} is deprecated; use \code{cbind} instead.
st_bind_cols = function(...) {
	# nocov start
	.Deprecated("cbind",
				msg = paste0("Use 'cbind' instead when all arguments",
				             " to be binded are of class sf.\n",
				             "If you need to cbind a data.frame to an sf,",
				             " use 'st_sf' or 'dplyr::bind_cols' (see the examples)."))
	cbind.sf(...)
	# nocov end
}
