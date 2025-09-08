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
#' @param dim specify what 3- or 4-dimensional points reflect: passed on to \link{st_point} (only when argument coords is given)
#' @param remove logical; when coords or wkt is given, remove these columns from data.frame?
#' @param na.fail logical; if \code{TRUE}, raise an error if coordinates contain missing values
#' @param sf_column_name character; name of the active list-column with simple feature geometries; in case
#' there is more than one and \code{sf_column_name} is \code{NULL}, the first one is taken.
#' @param ... passed on to \link{st_sf}, might included named arguments \code{crs} or \code{precision}
#' @details setting argument \code{wkt} annihilates the use of argument \code{coords}. If \code{x} contains a column called "geometry", \code{coords} will result in overwriting of this column by the \link{sfc} geometry list-column.  Setting \code{wkt} will replace this column with the geometry list-column, unless \code{remove} is \code{FALSE}.
#'
#' If `coords` has length 4, and `dim` is not `XYZM`, the four columns are taken as the xmin, ymin, xmax, ymax corner coordinates of a rectangle, and polygons are returned.
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
#' if (require(sp, quietly = TRUE)) {
#'  data(meuse, package = "sp")
#'  meuse_sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant")
#'  meuse_sf[1:3,]
#'  summary(meuse_sf)
#' }
#' @export
st_as_sf.data.frame = function(x, ..., agr = NA_agr_, coords, wkt,
		dim = "XYZ", remove = TRUE, na.fail = TRUE, sf_column_name = NULL) {
	if (! missing(wkt)) {
		if (remove)
			x[[wkt]] = st_as_sfc(as.character(x[[wkt]]))
		else
			x$geometry = st_as_sfc(as.character(x[[wkt]]))
	} else if (! missing(coords)) {
		if (length(coords) == 1) {
			stopifnot(is.matrix(x[[coords]]), is.numeric(x[[coords]]))
			cc = x[[coords]]
		} else {
			if (length(coords) == 2)
				dim = "XY"
			stopifnot(length(coords) == nchar(dim), dim %in% c("XY", "XYZ", "XYZM", "XYM"))
			cc = do.call(cbind, lapply(x[coords], as.numeric))
			if (na.fail && anyNA(cc))
				stop("missing values in coordinates not allowed")
		}
		dimnames(cc) = NULL
		if (is.null(sf_column_name))
			sf_column_name = "geometry"

		x[[sf_column_name]] = if (nchar(dim) < 4 && ncol(cc) == 4) { # create POLYGONs:
				fn = function(x) st_as_sfc(st_bbox(c(xmin = x[[1]], ymin = x[[2]], xmax = x[[3]], ymax = x[[4]])))
				do.call(c, apply(as.matrix(cc), 1, fn))
			} else { # points:
				structure(vector("list", length = nrow(cc)),
				points = cc,
				points_dim = dim,
				n_empty = 0L, precision = 0, crs = NA_crs_,
				bbox = bbox.pointmatrix(cc),
				class = c("sfc_POINT", "sfc"), names = NULL)
			}

		if (remove) {
			if (is.character(coords))
				coords = match(coords, names(x))
			x = x[-coords]
		}

	}
	st_sf(x, ..., agr = agr, sf_column_name = sf_column_name)
}

#' @name st_as_sf
#' @export
st_as_sf.sf = function(x, ...) x

#' @name st_as_sf
#' @export
st_as_sf.sfc = function(x, ...) st_sf(x, ...)

#' Get, set, replace or rename geometry from an sf object
#'
#' Get, set, replace or rename geometry from an sf object
#' @param obj object of class \code{sf} or \code{sfc}
#' @param ... ignored
#' @return st_geometry returns an object of class \link{sfc}, a list-column with geometries
#' @export
st_geometry = function(obj, ...) UseMethod("st_geometry")

#' @name st_geometry
#' @export
st_geometry.sf = function(obj, ...) {
	ret =  obj[[attr(obj, "sf_column")]]
	if (!inherits(ret, "sfc")) # corrupt!
		stop('attr(obj, "sf_column") does not point to a geometry column.\nDid you rename it, without setting st_geometry(obj) <- "newname"?')
	ret
}

#' @name st_geometry
#' @export
st_geometry.sfc = function(obj, ...) obj

#' @name st_geometry
#' @export
st_geometry.sfg = function(obj, ...) st_sfc(obj)

#' @name st_geometry
#' @param x object of class \code{data.frame} or \code{sf}
#' @param value object of class \code{sfc}, or \code{character} to set, replace, or rename the geometry of \code{x}
#' @export
#' @return \code{st_geometry} returns an object of class \link{sfc}. Assigning geometry to a \code{data.frame} creates an \link{sf} object, assigning it to an \link{sf} object replaces the geometry list-column.
#' @details when applied to a \code{data.frame} and when \code{value} is an object of class \code{sfc}, \code{st_set_geometry} and \code{st_geometry<-} will first check for the existence of an attribute \code{sf_column} and overwrite that, or else look for list-columns of class \code{sfc} and overwrite the first of that, or else write the geometry list-column to a column named \code{geometry}.  In case \code{value} is character and \code{x} is of class \code{sf}, the "active" geometry column is set to \code{x[[value]]}.
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
#' st_geometry(df) <- NULL # remove geometry, coerce to data.frame
`st_geometry<-` = function(x, value) UseMethod("st_geometry<-")

#' @export
`st_geometry<-.data.frame` = function(x, value) {
	stopifnot(inherits(value, "sfc") || is.character(value))
	if (inherits(value, "sfc"))
		stopifnot(nrow(x) == length(value))
	if (is.character(value))
		st_sf(x, sf_column_name = value)
	else {
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
		} else
			x$geometry = value
		st_sf(x)
	}
}


#' @export
`st_geometry<-.sf` = function(x, value) {
	if (! is.null(value)) {
		stopifnot(is.character(value) || inherits(value, "sfc"))
		if (inherits(value, "sfc"))
			stopifnot(nrow(x) == length(value))
	}

	if (!is.null(value) && is.character(value)) { # set flag to another column:
		if (!(value %in% names(x)))
			names(x)[names(x) == attr(x, "sf_column")] = value
		attr(x, "sf_column") <- value
	} else # replace, remove, or set list-column
		x[[attr(x, "sf_column")]] <- value

	if (is.null(value))
		structure(x, sf_column = NULL, agr = NULL, class = setdiff(class(x), "sf"))
	else
		x
}

#' @name st_geometry
#' @export
#' @examples
#' sf <- st_set_geometry(df, sfc) # set geometry, return sf
#' st_set_geometry(sf, NULL) # remove geometry, coerce to data.frame
st_set_geometry = function(x, value) {
	st_geometry(x) = value
	x
}

#' @export
st_as_sfc.sf = function(x, ...) st_geometry(x)

list_column_to_sfc = function(x) {
	if (is.list(x) && !inherits(x, "data.frame")) {
		if (inherits(try(y <- st_as_sfc(x), silent = TRUE), "try-error"))
			x
		else
			y
	} else
		x
}

#' Create sf object
#'
#' Create sf, which extends data.frame-like objects with a simple feature list column.
#' To convert a data frame object to `sf`, use [st_as_sf()]
#' @name sf
#' @aliases st_sf
#' @param ... column elements to be binded into an \code{sf} object or a single \code{list} or \code{data.frame} with such columns; at least one of these columns shall be a geometry list-column of class \code{sfc} or be a list-column that can be converted into an \code{sfc} by \link{st_as_sfc}.
#' @param crs coordinate reference system, something suitable as input to \link{st_crs}
#' @param agr character vector; see details below.
#' @param row.names row.names for the created \code{sf} object
#' @param stringsAsFactors logical; see \link{st_read}
#' @param precision numeric; see \link{st_as_binary}
#' @param sf_column_name character; name of the active list-column with simple feature geometries; in case
#' there is more than one and \code{sf_column_name} is \code{NULL}, the first one is taken.
#' @param sfc_last logical; if \code{TRUE}, \code{sfc} columns are always put last, otherwise column order is left unmodified.
#' @param check_ring_dir see \link{st_read}
#' @details \code{agr}, attribute-geometry-relationship, specifies for each non-geometry attribute column how it relates to the geometry, and can have one of following values: "constant", "aggregate", "identity". "constant" is used for attributes that are constant throughout the geometry (e.g. land use), "aggregate" where the attribute is an aggregate value over the geometry (e.g. population density or population count), "identity" when the attributes uniquely identifies the geometry of particular "thing", such as a building ID or a city name. The default value, \code{NA_agr_}, implies we don't know.
#'
#' When a single value is provided to \code{agr}, it is cascaded across all input columns; otherwise, a named vector like \code{c(feature1='constant', ...)} will set \code{agr} value to \code{'constant'} for the input column named \code{feature1}. See \code{demo(nc)} for a worked example of this.
#'
#' When confronted with a data.frame-like object, \code{st_sf} will try to find a geometry column of class \code{sfc}, and otherwise try to convert list-columns when available into a geometry column, using \link{st_as_sfc}.
#' @examples
#' g = st_sfc(st_point(1:2))
#' st_sf(a=3,g)
#' st_sf(g, a=3)
#' st_sf(a=3, st_sfc(st_point(1:2))) # better to name it!
#' # create empty structure with preallocated empty geometries:
#' nrows <- 10
#' geometry = st_sfc(lapply(1:nrows, function(x) st_geometrycollection()))
#' df <- st_sf(id = 1:nrows, geometry = geometry)
#' @export
st_sf = function(..., agr = NA_agr_, row.names,
		stringsAsFactors = sf_stringsAsFactors(), crs, precision,
		sf_column_name = NULL, check_ring_dir = FALSE, sfc_last = TRUE) {
	x = list(...)
	if (length(x) == 1L && (inherits(x[[1L]], "data.frame") || (is.list(x) && !inherits(x[[1L]], "sfc"))))
		x = x[[1L]]

	# find the sfc column(s):
	all_sfc_columns = vapply(x, function(x) inherits(x, "sfc"), TRUE)
	if (! any(all_sfc_columns)) { # try to create sfc from list-columns:
		xlst = lapply(x, list_column_to_sfc)
		all_sfc_columns = vapply(xlst, function(x) inherits(x, "sfc"), TRUE)
		if (! any(all_sfc_columns))
			stop("no simple features geometry column present")
		x[all_sfc_columns] = xlst[all_sfc_columns]
	}

	all_sfc_columns = which(unlist(all_sfc_columns))

	# set names if not present:
	all_sfc_names = if (!is.null(names(x)) && any(nzchar(names(x)[all_sfc_columns])))
		names(x)[all_sfc_columns]
	else {
		object = as.list(substitute(list(...)))[-1L]
		arg_nm = sapply(object, function(x) deparse(x))
		if (identical(arg_nm, "."))
			arg_nm = "geometry"
		make.names(arg_nm[all_sfc_columns])
	}

	if (! is.null(sf_column_name)) {
		stopifnot(sf_column_name %in% all_sfc_names)
		sf_column = match(sf_column_name, all_sfc_names)
		sfc_name = sf_column_name
	} else {
		sf_column = all_sfc_columns[1L]
		sfc_name = all_sfc_names[1L]
	}

	if (missing(row.names))
		row.names = seq_along(x[[sf_column]])

	df = if (inherits(x, c("tbl_df", "tbl"))) # no worries:
			x
		else if (length(x) == 1) # ONLY one sfc
			data.frame(row.names = row.names)
		else if (!sfc_last && inherits(x, "data.frame"))
			x
		else if (sfc_last && inherits(x, "data.frame"))
			x[-all_sfc_columns]
		else if (inherits(x[[1]], c("tbl_df", "tbl")))
			x[[1]]
		else
			cbind(data.frame(row.names = row.names),
				as.data.frame(x[-all_sfc_columns],
					stringsAsFactors = stringsAsFactors, optional = TRUE))

	if (check_ring_dir) { # process:
		for (i in seq_along(all_sfc_names))
			df[[ all_sfc_names[i] ]] = st_sfc(x[[ all_sfc_columns[i] ]],
				check_ring_dir = check_ring_dir)
	} else { # copy:
		for (i in seq_along(all_sfc_names))
			df[[ all_sfc_names[i] ]] = x[[ all_sfc_columns[i] ]]
	}

	if (! missing(precision))
		attr(df[[sfc_name]], "precision") = precision

	# add attributes:
	attr(df, "sf_column") = sfc_name
	if (! inherits(df, "sf"))
		class(df) = c("sf", class(df))
	st_agr(df) = agr
	if (! missing(crs))
		st_crs(df) = crs

	if (Sys.getenv("ADD_SF_NAMESPACE") == "true")
		attr(df, ".sf_namespace") <- .sf_namespace

	df
}

.sf_namespace <- function() NULL

#' @name sf
#' @param x object of class \code{sf}
#' @param i record selection, see \link{[.data.frame}, or a \code{sf} object to work with the \code{op} argument
#' @param j variable selection, see \link{[.data.frame}
#' @param drop logical, default \code{FALSE}; if \code{TRUE} drop the geometry column and return a \code{data.frame}, else make the geometry sticky and return a \code{sf} object.
#' @param op function; geometrical binary predicate function to apply when \code{i} is a simple feature object
#' @details \code{[.sf} will return a \code{data.frame} or vector if the geometry column (of class \code{sfc}) is dropped (\code{drop=TRUE}), an \code{sfc} object if only the geometry column is selected, and otherwise return an \code{sf} object; see also \link{[.data.frame}; for \code{[.sf} \code{...} arguments are passed to \code{op}.
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
	if (!missing(i) && (inherits(i, "sf") || inherits(i, "sfc") || inherits(i, "sfg")))
		i = lengths(op(x, i, ...)) != 0
	sf_column = attr(x, "sf_column")
	geom = st_geometry(x)
	if (!missing(i) && nargs > 2) { # e.g. a[3:4,] not a[3:4]
		if (is.character(i))
			i = match(i, row.names(x))
		geom = geom[i]
	}

	# x = as.data.frame(x)
	class(x) = setdiff(class(x), "sf") # one step down
	x = if (missing(j)) {
		if (nargs == 2) # `[`(x,i)
			x[i] # do sth else for tbl?
		else
			x[i, , drop = drop]
	} else
		x[i, j, drop = drop]

	if (!missing(j))
		agr = agr[j]
	else if (!missing(i) && nargs <= 2)
		agr = agr[i] # e.g., obj["name"]

	if (inherits(x, "sfc")) # drop was TRUE, and we selected geom column only
		x
	else if (! drop) {
		x[[ sf_column ]] = geom
		x = st_sf(x, sf_column_name = sf_column, sfc_last = FALSE)
		st_set_agr(x, agr[match(setdiff(names(x), sf_column), names(agr))])
	} else
		structure(x, class = setdiff(class(x), "sf"))
}

#' @export
"$<-.sf" = function(x, i, value) {
	if (is.null(value) && inherits(x[[i]], "sfc") &&
			((is.character(i) && i == attr(x, "sf_column"))
				|| (is.integer(i) && names(x)[i] == attr(x, "sf_column"))))
		st_set_geometry(x, NULL)
	else {
		x[[i]] = value
		x
	}
}

#' @export
"[<-.sf" = function(x, i, j, value) {
	st_as_sf(st_set_agr(NextMethod()))
}

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
	x = structure(NextMethod(), class = c("sf", setdiff(class(x), "sf")))
	if (! setting_geom)
		st_agr(x) = agr
	x
}

#' @rdname sf
#' @param n maximum number of features to print; can be set globally by \code{options(sf_max_print=...)}
#' @export
print.sf = function(x, ..., n = getOption("sf_max_print", default = 10)) {

	geoms = which(vapply(x, function(col) inherits(col, "sfc"), TRUE))
	nf = length(x) - length(geoms)
	app = paste("and", nf, ifelse(nf == 1, "field", "fields"))
	if (any(!is.na(st_agr(x))))
		app = paste0(app, "\n", "Attribute-geometry relationship", ifelse(nf > 1, "s: ", ": "), summarize_agr(x))
	if (length(geoms) > 1)
		app = paste0(app, "\n", "Active geometry column: ", attr(x, "sf_column"))
	print(st_geometry(x), n = 0, what = "Simple feature collection with", append = app)
	if (n > 0) {
		if (inherits(x, c("tbl_df", "tbl"))) {
			st_geometry(x) = x[[attr(x, "sf_column")]][] # note the extra []: this reloads points
			NextMethod()
		} else {
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
	ret = NextMethod() # if data.table, drops sf_column attribute;
	class(ret) = setdiff(class(ret), "sf")
	g = ret[[sf_column]] # may have NULL values in it
	ret[[sf_column]] = st_sfc(g) # fix NULL values
	st_set_geometry(ret, sf_column) # FIXME: set agr
}

#' @export
as.data.frame.sf = function(x, ...) {
	class(x) <- setdiff(class(x), "sf")
	NextMethod()
}

#' @export
duplicated.sf <- function(x, incomparables = FALSE, fromLast = FALSE, ...) {
  if (length(x) != 1L) {
    if (any(i <- vapply(x, is.factor, NA))) {
      for (j in names(i[i])) {
        x[[j]] <- lapply(x[[j]], as.numeric)
      }
    }
    if (any(i <- (lengths(lapply(x, dim)) == 2L))) {
      for (j in names(i[i])) {
        x[[j]] <- lapply(x[[j]], split.data.frame, seq_len(nrow(x)))
      }
    }
  }
  NextMethod()
}

#' @export
#' @name st_geometry
#' @details if \code{x} is of class \code{sf}, \code{st_drop_geometry} drops the geometry of its argument, and reclasses it accordingly; otherwise it returns \code{x} unmodified. 
st_drop_geometry = function(x, ...) UseMethod("st_drop_geometry")

#' @export
#' @name st_geometry
st_drop_geometry.sf = function(x, ...) {
	st_set_geometry(x, NULL)
}

#' @export
#' @name st_geometry
st_drop_geometry.default = function(x, ...) {
	x
}

#' transform method for sf objects
#' 
#' Can be used to create or modify attribute variables; for transforming geometries see 
#' \link{st_transform}, and all other functions starting with \code{st_}.
#' 
#' @param _data object of class \code{sf}
#' @param ... Further arguments of the form `new_variable = expression`
#'
#' @export
#' @examples
#' a = data.frame(x1 = 1:3, x2 = 5:7)
#' st_geometry(a) = st_sfc(st_point(c(0,0)), st_point(c(1,1)), st_point(c(2,2)))
#' transform(a, x1_sq = x1^2)
#' transform(a, x1_x2 = x1*x2)
transform.sf <- function (`_data`, ...) {
  st_as_sf(NextMethod(), agr = st_agr(`_data`), sf_column_name = attr(`_data`, "sf_column"))
}
