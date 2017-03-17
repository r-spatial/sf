#' Convert foreign object to an sf object
#'
#' Convert foreign object to an sf object
#' @param x object to be converted into an object class \code{sf}
#' @export
st_as_sf = function(x, ...) UseMethod("st_as_sf")

#' @name st_as_sf
#'
#' @param agr character vector; see details section of \link{st_sf}
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
#' meuse_sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant")
#' meuse_sf[1:3,]
#' summary(meuse_sf)
#' @export
st_as_sf.data.frame = function(x, ..., agr = NA_agr_, coords, wkt, 
		dim = "XYZ", remove = TRUE) {
	if (! missing(wkt)) {
		if (remove) 
			x[[wkt]] = st_as_sfc(as.character(x[[wkt]]))
		else
			x$geometry = st_as_sfc(as.character(x[[wkt]]))
	} else if (! missing(coords)) {
		classdim = getClassDim(rep(0, length(coords)), length(coords), dim, "POINT")
		x$geometry = structure( lapply(split(as.vector(t(as.matrix(x[, coords]))), 
				rep(seq_len(nrow(x)), each = length(coords))), 
				function(vec) structure(vec, class = classdim)), 
			n_empty = 0L, precision = 0, crs = st_crs(NA), 
			bbox = c(xmin = min(x[[coords[1]]], na.rm = TRUE), 
					ymin = min(x[[coords[2]]], na.rm = TRUE), 
					xmax = max(x[[coords[1]]], na.rm = TRUE),
					ymax = max(x[[coords[2]]], na.rm = TRUE)), 
			class =  c("sfc_POINT", "sfc" ))

		if (is.character(coords))
			coords = match(coords, names(x))

		if (remove)
			x = x[-coords]
	}
	st_sf(x, ..., agr = agr)
}

#' @name st_as_sf
#' @export
st_as_sf.sf = function(x, ...) x

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
#' @param value object of class \code{sfc}, or \code{character}
#' @export
#' @return \code{st_geometry} returns an object of class \link{sfc}. Assigning geometry to a \code{data.frame} creates an \link{sf} object, assigning it to an \link{sf} object replaces the geometry list-column.
#' @details when applied to a \code{data.frame} and when \code{value} is an object of class \code{sfc}, \code{st_set_geometry} and \code{st_geomtry<-} will first check for the existance of an attribute \code{sf_column} and overwrite that, or else look for list-columns of class \code{sfc} and overwrite the first of that, or else write the geometry list-column to a column named \code{geometry}.  In case \code{value} is character and \code{x} is of class \code{sf}, the "active" geometry column is set to \code{x[[value]]}. 
#' 
#' the replacement function applied to \code{sf} objects will overwrite the geometry list-column, if \code{value} is \code{NULL}, it will remove it and coerce \code{x} to a \code{data.frame}.
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
	stopifnot(inherits(value, "sfc") || is.character(value))
	if (inherits(value, "sfc"))
		stopifnot(nrow(x) == length(value))
	a = vapply(x, function(v) inherits(v, "sfc"), TRUE)

	if (any(a)) {
		w = which(a)
		sf_col = attr(x, "sf_column")
		if (! is.null(sf_col))
			x[[ sf_col ]] = value
		else {
			if (length(w) > 1)
				warning("overwriting first sfc column")
			x[[ which(a)[1L] ]] = value
		}
		st_sf(x)
	} else {
		if (is.character(value))
			x = st_sf(x, sf_column_name = value)
		else
			st_sf(x, geometry = value)
	}
}

#' @export
`st_geometry<-.sf` = function(x, value) {
	if (! is.null(value)) {
		stopifnot(inherits(value, "sfc") || is.character(value))
		if (inherits(value, "sfc"))
			stopifnot(nrow(x) == length(value))
		if (is.character(value))
			stopifnot(inherits(x[[value]], "sfc"))
	}

	if (!is.null(value) && is.character(value)) # set flag to another column:
		attr(x, "sf_column") <- value
	else # replace, remove, or set list-column
		x[[attr(x, "sf_column")]] <- value

	if (is.null(value))
		data.frame(x)
	else
		x
}

#' @name st_geometry
#' @export
st_set_geometry = function(x, value) {
	st_geometry(x) = value
	x
}

#' Create sf object
#' 
#' Create sf, which extends data.frame-like objects with a simple feature list column
#' @name sf
#' @param ... column elements to be binded into an \code{sf} object, one of them being of class \code{sfc}
#' @param crs coordinate reference system: integer with the epsg code, or character with proj4string
#' @param agr character vector; see details below.
#' @param row.names row.names for the created \code{sf} object
#' @param stringsAsFactors logical; logical: should character vectors be converted to factors?  The `factory-fresh' default is \code{TRUE}, but this can be changed by setting \code{options(stringsAsFactors = FALSE)}.  
#' @param precision numeric; see \link{st_as_binary}
#' @param sf_column_name character; name of the list-column with simple feature geometries, in case 
#' there is more than one; if there is more than one and \code{sf_column_name} is not given, the 
#' first one is selected and a warning is given
#' @details \code{agr}, attribute-geometry-relationship, specifies for each non-geometry attribute column how it relates to the geometry, and can have one of following values: "constant", "aggregate", "identity". "constant" is used for attributes that are constant throughout the geometry (e.g. land use), "aggregate" where the attribute is an aggregate value over the geometry (e.g. population density or population count), "identity" when the attributes uniquely identifies the geometry of particular "thing", such as a building ID or a city name. The default value, \code{NA_agr_}, implies we don't know.  
#' @examples
#' g = st_sfc(st_point(1:2))
#' st_sf(a=3,g)
#' st_sf(g, a=3)
#' st_sf(a=3, st_sfc(st_point(1:2))) # better to name it!
#' @export
st_sf = function(..., agr = NA_agr_, row.names, 
		stringsAsFactors = default.stringsAsFactors(), crs, precision, sf_column_name = NULL) {
	x = list(...)
	if (length(x) == 1 && inherits(x[[1L]], "data.frame"))
		x = x[[1L]]

	# find the sfc column(s):
	all_sfc_columns = vapply(x, function(x) inherits(x, "sfc"), TRUE)
	if (! any(all_sfc_columns))
		stop("no simple features geometry column present")
	else 
		all_sfc_columns = which(unlist(all_sfc_columns))

	# set names if not present:
	all_sfc_names = if (!is.null(names(x)) && nzchar(names(x)[all_sfc_columns]))
		names(x)[all_sfc_columns]
	else {
		object = as.list(substitute(list(...)))[-1L] 
		arg_nm = sapply(object, function(x) deparse(x))
		make.names(arg_nm[all_sfc_columns])
	}

	if (!is.null(sf_column_name)) {
		stopifnot(sf_column_name %in% all_sfc_names)
		sf_column = match(sf_column_name, all_sfc_names)
		sfc_name = sf_column_name
	} else {
		if (length(all_sfc_columns) > 1L)
			warning(paste0("more than one geometry column: taking `", all_sfc_names[1L],
				"'; use `sf_column_name=' to specify a different column."))
		sf_column = all_sfc_columns[1L]
		sfc_name = all_sfc_names[1L]
	}

	if (missing(row.names))
		row.names = seq_along(x[[sf_column]])
	df = if (length(x) == 1) # ONLY sfc
			data.frame(row.names = row.names)
		else {
			if (inherits(x, "data.frame"))
				x[-all_sfc_columns]
			else # create a data.frame from list:
				data.frame(x[-all_sfc_columns], row.names = row.names, 
					stringsAsFactors = stringsAsFactors)
		}


	for (i in seq_along(all_sfc_names))
		df[[ all_sfc_names[i] ]] = x[[ all_sfc_columns[i] ]]

	if (! missing(precision))
		attr(df[[sfc_name]], "precision") = precision

	# add attributes:
	attr(df, "sf_column") = sfc_name
	if (! inherits(df, "sf"))
		class(df) = c("sf", class(df))
	st_agr(df) = agr
	if (! missing(crs))
		st_crs(df) = crs
	df
}

#' @name sf
#' @param x object of class \code{sf}
#' @param i record selection, see \link{[.data.frame}
#' @param j variable selection, see \link{[.data.frame}
#' @param drop whether to drop to simpler (e.g. vector) representation, see \link{[.data.frame}
#' @param op function; geometrical binary predicate function to apply when \code{i} is a simple feature object
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
"[.sf" = function(x, i, j, ..., drop = FALSE, op = st_intersects) {
	nargs = nargs()
	agr = st_agr(x)
	if (!missing(i) && (inherits(i, "sf") || inherits(i, "sfc")))
		i = lengths(st_intersects(x, i)) != 0
	sf_column = attr(x, "sf_column")
	geom = st_geometry(x)
	if (!missing(i) && nargs > 2) { # e.g. a[3:4,] not a[3:4]
		if (is.character(i))
			i = match(i, row.names(x))
		geom = geom[i]
	}

	x = if (missing(j))
		NextMethod("[") # specifying drop would trigger a warning
	else
		NextMethod("[", drop = drop)

	if (!missing(j))
		agr = agr[j]
	else if (!missing(i) && nargs <= 2)
		agr = agr[i] # e.g., obj["name"]

	if (inherits(x, "sfc")) # drop was TRUE, and we selected geom column only
		x
	else if (! drop) {
#		st_agr(x) = agr
#		if (!(sf_column %in% names(x))) { # geom was deselected: make it sticky
#			if (inherits(x, "sf"))
#				x[[sf_column]] = geom
#			else
#				st_geometry(x) = geom
#		}
#		structure(x, "sf_column" = sf_column, 
#			"agr" = agr[match(setdiff(names(x), sf_column), names(agr))])
		if (inherits(x, "sf")) {
			st_agr(x) = agr[!is.na(names(agr))]
			attr(x, "sf_column") = sf_column
		}
		st_geometry(x) = geom
		st_agr(x) = agr[match(setdiff(names(x), sf_column), names(agr))]
		x
	} else
		as.data.frame(x)
}

#' @export
"$<-.sf" = function(x, i, value) { x[[i]] = value; x }

#' @export
"[[<-.sf" = function(x, i, value) {
	agr = st_agr(x)
	setting_geom = (i == attr(x, "sf_column")) || inherits(value, "sfc")
	if (! setting_geom) { # need to handle agr:
		ix = if (is.character(i))
				which(i == names(x))
			else
				i
		if (is.null(value)) # remove
			agr = agr[-ix]
		else {
			if (length(ix) == 0 || ix > length(names(x))) # add:
				agr = st_agr(c(as.character(agr), NA_character_))
			else # replace:
				agr[ix] = NA
		}
	}
	x = NextMethod()
	if (! setting_geom)
		st_agr(x) = agr
	x
}

#' @export
print.sf = function(x, ..., n = 
		ifelse(options("max.print")[[1]] == 99999, 20, options("max.print")[[1]])) { 

	nf = length(x) - 1
	app = paste("and", nf, ifelse(nf == 1, "field", "fields"))
	if (any(!is.na(st_agr(x))))
		app = paste0(app, "\n", "Attribute-geometry relationship: ", summarize_agr(x))
	print(st_geometry(x), n = 0, what = "Simple feature collection with", append = app)
	if (n > 0) {
		if (inherits(x, "tbl_df"))
			NextMethod()
		else {
			y <- x
			if (nrow(y) > n) {
				cat(paste("First", n, "features:\n"))
				y <- x[1:n, , drop = FALSE]
			}
			print.data.frame(y, ...)
		}
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
	dots = list(...)
	crs0 = st_crs(dots[[1]])
	if (length(dots) > 1L) { # check all crs are equal...
		equal_crs = vapply(dots[-1L], function(x) st_crs(x) == crs0, TRUE)
		if (!all(equal_crs))
			stop("arguments have different crs", call. = FALSE)
	}
	ret = st_as_sf(base::rbind.data.frame(...), crs = crs0)
	attr(ret[[ attr(ret, "sf_column") ]], "bbox") = c(st_bbox(ret)) # recompute & strip crs
	ret
}

#' Bind columns (variables) of sf objects
#'
#' Bind columns (variables) of sf objects
#' @name bind
#' @return if \code{cbind} or \code{st_bind_cols} is called with multiple \code{sf} objects, it warns and removes all but the first geometry column from the input objects.
#' @export
cbind.sf = function(..., deparse.level = 1)
	st_sf(base::cbind.data.frame(...))

#' @name bind
#' @export
st_bind_cols = function(...) {
	cbind.sf(...)
}

#' merge method for sf and data.frame object
#' 
#' merge method for sf and data.frame object
#' @param x object of class \code{sf}
#' @param y object of class \code{data.frame}
#' @param ... arguments passed on to \code{merge.data.frame}
#' @export
#' @examples
#' a = data.frame(a = 1:3, b = 5:7)
#' st_geometry(a) = st_sfc(st_point(c(0,0)), st_point(c(1,1)), st_point(c(2,2)))
#' b = data.frame(x = c("a", "b", "c"), b = c(2,5,6))
#' merge(a, b)
#' merge(a, b, all = TRUE)
merge.sf = function(x, y, ...) {
	if (inherits(y, "sf"))
		stop("merge on two sf objects not supported")
	sf_column = attr(x, "sf_column")
	ret = NextMethod()
	g = ret[[sf_column]] # may have NULL values in it
	ret[[sf_column]] = NULL
	st_geometry(ret) = fix_NULL_values(g)
	ret
}
