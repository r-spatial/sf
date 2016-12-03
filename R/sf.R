#' Convert foreign object to an sf object
#'
#' Convert foreign object to an sf object
#' @param x object to be converted into an object class \code{sf}
#' @export
st_as_sf = function(x, ...) UseMethod("st_as_sf")

#' @name st_as_sf
#'
#' @param relation_to_geometry character vector; see details section of \link{st_sf}
#' @param coords in case of point data: names or numbers of the numeric columns holding coordinates
#' @param wkt name or number of the character column that holds WKT encoded geometries
#' @param dim passed on to \link{st_point} (only when argument coords is given)
#' @param remove logical; when coords or wkt is given, remove these columns from data.frame?
#' @param ... passed on to \link{st_sf}, might included crs
#' @details setting argument \code{wkt} annihilates the use of argument \code{coords}. If \code{x} contains a column called "geometry", \code{coords} will result in overwriting of this column by the \link{sfc} geometry list-column.  Setting \code{wkt} will replace this column with the geometry list-column, unless \code{remove_coordinates} is \code{FALSE}.
#' 
#' @examples
#' pt1 = st_point(c(0,1))
#' pt2 = st_point(c(1,1))
#' st_sfc(pt1, pt2)
#' d = data.frame(a = 1:2)
#' d$geom = st_sfc(pt1, pt2)
#' df = st_as_sf(d)
#' d$geom = c("POINT(0 0)", "POINT(0 1)")
#' df = st_as_sf(d, wkt = "geom")
#' d$geom2 = st_sfc(pt1, pt2)
#' st_as_sf(d) # should warn
#' data(meuse, package = "sp")
#' meuse_sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992, relation_to_geometry = "field")
#' meuse_sf[1:3,]
#' summary(meuse_sf)
#' @export
st_as_sf.data.frame = function(x, ..., relation_to_geometry = NA_character_, coords, wkt, 
		dim = "XYZ", remove = TRUE) {
	if (! missing(wkt)) {
		if (remove) 
			x[[wkt]] = st_as_sfc(as.character(x[[wkt]]))
		else
			x$geometry = st_as_sfc(as.character(x[[wkt]]))
	} else if (! missing(coords)) {
		x$geometry = do.call(st_sfc, c(lapply(seq_len(nrow(x)), 
				function(i) st_point(unlist(x[i, coords]), dim = dim))))
		if (remove)
			x[coords] = NULL
	}
	#do.call(st_sf, c(as.list(x), list(...), relation_to_geometry = relation_to_geometry))
	st_sf(x, ..., relation_to_geometry = relation_to_geometry)
}

#' Get, set, or replace geometry from an sf object
#' 
#' Get, set, or replace geometry from an sf object
#' @param obj object of class \code{sf} or \code{sfc}
#' @param ... ignored
#' @return st_geometry returns an object of class \link{sfc}, a list-column with geometries
#' @export
st_geometry = function(obj, ...) UseMethod("st_geometry")

#' @name st_geometry
#' @export
st_geometry.sf = function(obj, ...) obj[[attr(obj, "sf_column")]]

#' @name st_geometry
#' @export
st_geometry.sfc = function(obj, ...) obj

#' @name st_geometry
#' @export
st_geometry.sfg = function(obj, ...) st_sfc(obj)

#' @name st_geometry
#' @param x object of class \code{data.frame}
#' @param value object of class \code{sfc}
#' @export
#' @return \code{st_geometry} returns an object of class \link{sfc}. Assigning geometry to a \code{data.frame} creates an \link{sf} object, assigning it to an \link{sf} object replaces the geometry list-column.
#' @examples 
#' df = data.frame(a = 1:2)
#' sfc = st_sfc(st_point(c(3,4)), st_point(c(10,11)))
#' st_geometry(sfc)
#' st_geometry(df) <- sfc
#' class(df)
#' st_geometry(df)
#' st_geometry(df) <- sfc # replaces
`st_geometry<-` = function(x, value) UseMethod("st_geometry<-")

#' @export
`st_geometry<-.data.frame` = function(x, value) {
	stopifnot(inherits(value, "sfc"))
	st_sf(x, geometry = value)
}

#' @export
`st_geometry<-.sf` = function(x, value) {
	stopifnot(inherits(value, "sfc"))
	x[[attr(x, "sf_column")]] <- value
	x
}

#' Create sf object
#' 
#' Create sf, which extends data.frame-like objects with a simple feature list column
#' @name sf
#' @param ... column elements to be binded into an \code{sf} object, one of them being of class \code{sfc}
#' @param crs coordinate reference system: integer with the epsg code, or character with proj4string
#' @param relation_to_geometry character vector; see details below.
#' @param row.names row.names for the created \code{sf} object
#' @param stringsAsFactors logical; logical: should character vectors be converted to factors?  The `factory-fresh' default is \code{TRUE}, but this can be changed by setting \code{options(stringsAsFactors = FALSE)}.  
#' @param precision numeric; see \link{st_as_binary}
#' @details \code{relation_to_geometry} specified for each non-geometry attribute column how it relates to the geometry, and can have one of following values: "field", "lattice", "entity". "field" is used for attributes that are constant throughout the geometry (e.g. land use), "lattice" where the attribute is an aggregate value over the geometry (e.g. population density or population count), "entity" when the attributes uniquely identifies the geometry of particular "thing", such as a building ID or a city name. The default value, \code{NA_character_}, implies we don't know.  
#' @examples
#' g = st_sfc(st_point(1:2))
#' st_sf(a=3,g)
#' st_sf(g, a=3)
#' st_sf(a=3, st_sfc(st_point(1:2))) # better to name it!
#' @export
st_sf = function(..., relation_to_geometry = NA_character_, row.names, 
		stringsAsFactors = default.stringsAsFactors(), crs, precision) {
	x = list(...)
	if (length(x) == 1 && inherits(x[[1]], "data.frame"))
		x = x[[1]]
	# find & remove the sfc column:
	sf = sapply(x, function(x) inherits(x, "sfc"))
	if (! any(sf))
		stop("no simple features geometry column present")
	sf_column = which(sf)
	if (length(sf_column) > 1) {
		warning("more than one geometry column: ignoring all but first")
		x[sf_column[-1]] = NULL
		sf_column = sf_column[1]
	}
	if (missing(row.names))
		row.names = seq_along(x[[sf_column]])
	df = if (length(x) == 1) # ONLY sfc
			data.frame(row.names = row.names)
		else {
			if (inherits(x, "data.frame"))
				x[-sf_column]
			else # create a data.frame from list:
				data.frame(x[-sf_column], row.names = row.names, 
					stringsAsFactors = stringsAsFactors)
		}

	# add sfc column, with right name:
	sfc_name = if (!is.null(names(x)) && nzchar(names(x)[sf_column]))
		names(x)[sf_column]
	else {
		object = as.list(substitute(list(...)))[-1L] 
		arg_nm = sapply(object, function(x) deparse(x))
		make.names(arg_nm[sf_column])
	}
	df[[sfc_name]] = x[[sf_column]]
	if (! missing(precision))
		attr(df[[sfc_name]], "precision") = precision

	# add attributes:
	attr(df, "sf_column") = sfc_name
	f = factor(rep(relation_to_geometry, length.out = ncol(df) - 1), 
		levels = c("field", "lattice", "entity"))
	names(f) = names(df)[-sf_column]
	attr(df, "relation_to_geometry") = f
	# FIXME: check that if one of them is lattice, geom cannot be POINT
	class(df) = c("sf", class(df))
	if (! missing(crs))
		st_crs(df) = crs
	df
}

#' @name sf
#' @param x object of class \code{sf}
#' @param i record selection, see \link{[.data.frame}
#' @param j variable selection, see \link{[.data.frame}
#' @param drop whether to drop to simpler (e.g. vector) representation, see \link{[.data.frame}
#' @details "[.sf" will return a \code{data.frame} if the geometry column (of class \code{sfc}) is dropped, an \code{sfc} object if only the geometry column is selected, otherwise the behavior depending on \code{drop} is identical to that of \link{[.data.frame}.
#' @examples
#' g = st_sfc(st_point(1:2), st_point(3:4))
#' s = st_sf(a=3:4, g)
#' s[1,]
#' class(s[1,])
#' s[,1]
#' class(s[,1])
#' s[,2]
#' class(s[,2])
#' g = st_sf(a=2:3, g)
#' pol = st_sfc(st_polygon(list(cbind(c(0,3,3,0,0),c(0,0,3,3,0)))))
#' h = st_sf(r = 5, pol)
#' g[h,]
#' h[g,]
#' @export
"[.sf" = function(x, i, j, ..., drop) {
	rtg = attr(x, "relation_to_geometry")
	if (!missing(i) && (inherits(i, "sf") || inherits(i, "sfc")))
		i = sapply(st_geos_binop("intersects", x, i, ...), length) != 0
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

#' @export
print.sf = function(x, ..., n = 
		ifelse(options("max.print")[[1]] == 99999, 20, options("max.print")[[1]])) { 

	nf = length(x) - 1
	app = paste("and", nf, ifelse(nf == 1, "field", "fields"))
	print(st_geometry(x), n = 0, what = "Simple feature collection with", append = app)
	if (n > 0) {
		y <- x
		if (nrow(y) > n) {
			cat(paste("First", n, "features:\n"))
			y <- x[1:n, , drop = FALSE]
		}
		print.data.frame(y, ...)
	}
	invisible(x)
}

#' Bind rows (features) of sf objects
#'
#' Bind rows (features) of sf objects
#' @param ... objects to bind
#' @param deparse.level integer; see \link[base]{rbind}
#' @name bind
#' @export
rbind.sf = function(..., deparse.level = 1) {
	ret = base::rbind.data.frame(...)
	st_geometry(ret) = do.call(st_sfc, st_geometry(ret))
	ret
}

#' Bind columns (variables) of sf objects
#'
#' Bind columns (variables) of sf objects
#' @name bind
#' @return if \code{cbind} is called with multiple \code{sf} objects, it warns and removes all but the first geometry column from the input objects.
#' @export
cbind.sf = function(..., deparse.level = 1) {
	st_sf(base::cbind.data.frame(...))
	# do.call(st_sf, list(...))
}
