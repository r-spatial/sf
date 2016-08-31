#' convert foreign object to an sf object
#'
#' convert foreign object to an sf object
#' @param x object to be converted into an object class \code{sf}
#' @param ... further arguments
#' @export
st_as_sf = function(x, ...) UseMethod("st_as_sf")

#' @name st_as_sf
#'
#' @param relation_to_geometry character vector; see details section of \link{st_sf}
#' @param coords in case of point data: coordinate names or numbers
#' @param third passed on to \link{st_point} (only when coords is given)
#' @param epsg integer denoting the epsg ID (only when coords is given)
#' @param proj4string character; giving the proj4 string (only when coords is given)
#' @param remove_coordinates logical; when coords is given, remove coordinate columns from data.frame?
#' 
#' @examples
#' pt1 = st_point(c(0,1))
#' pt2 = st_point(c(1,1))
#' st_sfc(list(pt1, pt2))
#' d = data.frame(a = 1:2)
#' d$geom = st_sfc(list(pt1, pt2))
#' df = st_as_sf(d)
#' d$geom2 = st_sfc(list(pt1, pt2))
#' st_as_sf(d) # should warn
#' data(meuse, package = "sp")
#' meuse_sf = st_as_sf(meuse, coords = c("x", "y"), epsg = 28992)
#' meuse_sf[1:3,]
#' summary(meuse_sf)
#' @export
st_as_sf.data.frame = function(x, ..., relation_to_geometry = NA_character_, coords, third = "XYZ", 
		epsg = NA_integer_, proj4string = NA_character_, remove_coordinates = TRUE) {
	if (! missing(coords)) {
		x$geometry = st_sfc(lapply(seq_len(nrow(x)), 
				function(i) st_point(unlist(x[i, coords]), third = third)
					), epsg = epsg, proj4string = proj4string)
		if (remove_coordinates)
			x[coords] = NULL
	}
	do.call(st_sf, c(as.list(x), list(...), relation_to_geometry = relation_to_geometry))
}

#' get geometry from sf object
#' 
#' get geometry from sf object
#' @param obj object of class \code{sf}
#' @param ... ignored
#' @export
st_geometry = function(obj, ...) UseMethod("st_geometry")

#' @export
st_geometry.sf = function(obj, ...) obj[[attr(obj, "sf_column")]]

#' create sf object
#' 
#' create sf, which extends data.frame-like objects with a simple feature list column
#' @name sf
#' @param ... column elements to be binded into an \code{sf} object, one of them being of class \code{sfc}
#' @param relation_to_geometry character vector; see details below.
#' @param row.names row.names for the created \code{sf} object
#' @param stringsAsFactors logical; see \link{data.frame}
#' @details \code{relation_to_geometry} specified for each non-geometry column how it relates to the geometry, and can have one of following values: "field", "lattice", "entity". "field" is used for attributes that are constant throughout the geometry (e.g. land use), "lattice" where the attribute is an aggregate value over the geometry (e.g. population density), "entity" when the attributes identifies the geometry of particular "thing", such as a building or a city. The default value, \code{NA_character_}, implies we don't know.  
#' @examples
#' g = st_sfc(list(st_point(1:2)))
#' st_sf(a=3,g)
#' st_sf(g, a=3)
#' st_sf(a=3, st_sfc(list(st_point(1:2)))) # better to name it!
#' @export
st_sf = function(..., relation_to_geometry = NA_character_, row.names, 
		stringsAsFactors = default.stringsAsFactors()) {
	x = list(...)
	# find & remove the sfc column:
	sf = sapply(x, function(x) inherits(x, "sfc"))
	if (!any(sf))
		stop("no simple features geometry column present")
	sf_column = which(sf)
	if (length(sf_column) > 1) {
		warning("more than one geometry column: ignoring all but first")
		x[sf_column[-1]] = NULL
		sf_column = sf_column[1]
	}
	if (missing(row.names))
		row.names = seq(length(x[[sf_column]]))
	df = if (length(x) == 1) # ONLY sfc
			data.frame(row.names = row.names)
		else 
			data.frame(x[-sf_column], row.names = row.names, stringsAsFactors = stringsAsFactors)

	# add sfc column, with right name:
	object = as.list(substitute(list(...)))[-1L] 
	arg_nm = sapply(object, function(x) deparse(x))
	sfc_name = if (!is.null(names(x)) && nzchar(names(x)[sf_column]))
		names(x)[sf_column]
	else 
		make.names(arg_nm[sf_column])
	df[[sfc_name]] = x[[sf_column]]

	# add attributes:
	attr(df, "sf_column") = sfc_name
	f = factor(rep(relation_to_geometry, length.out = ncol(df) - 1), 
		levels = c("field", "lattice", "entity"))
	names(f) = names(df)[-sf_column]
	attr(df, "relation_to_geometry") = f
	# TODO: check that if one of them is lattice, geom cannot be POINT
	structure(df, class = c("sf", class(df)))
}

#' @name sf
#' @param x object of class \code{sf}
#' @param i record selection, see \link{[.data.frame}
#' @param j variable selection, see \link{[.data.frame}
#' @param drop whether to drop to simpler (e.g. vector) representation, see \link{[.data.frame}
#' @details "[.sf" will return a \code{data.frame} if the geometry column (of class \code{sfc}) is dropped, an \code{sfc} object if only the geometry column is selected, otherwise the behavior depending on \code{drop} is identical to that of \link{[.data.frame}.
#' @examples
#' g = st_sfc(list(st_point(1:2), st_point(3:4)))
#' s = st_sf(a=3:4, g)
#' s[1,]
#' class(s[1,])
#' s[,1]
#' class(s[,1])
#' s[,2]
#' class(s[,2])
#' @export
"[.sf" = function(x, i, j, drop) {
	rtg = attr(x, "relation_to_geometry")
	sf_column = attr(x, "sf_column")
	x = NextMethod("[")
	if (inherits(x, "sfc")) # drop was TRUE, and we selected geom column only
		return(x)
	if (!(sf_column %in% names(x))) # geom was deselected
		return(as.data.frame(x))
	attr(x, "sf_column") = sf_column
	attr(x, "relation_to_geometry") = rtg[names(rtg) %in% names(x)]
	x
}

#' @name sf
#' @param obj object of class \code{sf} or \code{sfc}
#' @details \code{p4s} returns the PROJ.4 string; if an EPSG code is available, it constructs it from this, otherwise, it takes the \code{proj4string} attribute, if none of these is available (both are missing-valued), \code{NULL} is returned.
#' @export
st_p4s = function(obj) {
	if (inherits(obj, "sf"))
		obj = st_geometry(obj)
	epsg = attr(obj, "epsg")
	if (!is.na(epsg))
		return(paste0("+init=epsg:", epsg))
	p4s = attr(obj, "proj4string")
	if (!is.na(p4s))
		return(p4s)
	NULL
}
