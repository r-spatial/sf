# unary, interfaced through GEOS:

#' Dimension, simplicity, validity or is_empty queries on simple feature geometries
#' @name geos_query
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param NA_if_empty logical; if TRUE, return NA for empty geometries
#' @return st_dimension returns a numeric vector with 0 for points, 1 for lines, 2 for surfaces, and, if \code{NA_if_empty} is \code{TRUE}, \code{NA} for empty geometries.
#' @export
#' @examples
#' x = st_sfc(
#' 	st_point(0:1),
#' 	st_linestring(rbind(c(0,0),c(1,1))),
#' 	st_polygon(list(rbind(c(0,0),c(1,0),c(0,1),c(0,0)))),
#' 	st_multipoint(),
#' 	st_linestring(),
#' 	st_geometrycollection())
#' st_dimension(x)
#' st_dimension(x, FALSE)
st_dimension = function(x, NA_if_empty = TRUE)
	CPL_gdal_dimension(st_geometry(x), NA_if_empty)

#' @name geos_measures
#' @export
#' @return If the coordinate reference system of \code{x} was set, these functions return values with unit of measurement; see \link[units]{set_units}.
#'
#' st_area returns the area of a geometry, in the coordinate reference system used; in case \code{x} is in degrees longitude/latitude, \link[lwgeom:geod]{st_geod_area} is used for area calculation.
#' @examples
#' b0 = st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))
#' b1 = b0 + 2
#' b2 = b0 + c(-0.2, 2)
#' x = st_sfc(b0, b1, b2)
#' st_area(x)
st_area = function(x, ...) UseMethod("st_area")

#' @name geos_measures
#' @export
st_area.sfc = function(x, ...) {
	if (isTRUE(st_is_longlat(x))) {
		if (sf_use_s2())
			units::set_units(s2::s2_area(x, ...), "m^2", mode = "standard")
		else {
			if (! requireNamespace("lwgeom", quietly = TRUE))
				stop("package lwgeom required, please install it first")
			lwgeom::st_geod_area(x)
		}
	} else {
		a = CPL_area(x) # ignores units: units of coordinates
		if (!is.null(u <- st_crs(x)$ud_unit))
			units(a) = u^2 # coord units
		if (!is.null(to_m <- st_crs(x)$to_meter) && !is.na(to_m) && !inherits(a, "units"))
			a = set_units(a * to_m^2, "m^2", mode = "standard")
		a
	}
}

#' @export
st_area.sf = function(x, ...) st_area(st_geometry(x), ...)

#' @export
st_area.sfg = function(x, ...) st_area(st_geometry(x), ...)

#' @name geos_measures
#' @export
#' @return st_length returns the length of a \code{LINESTRING} or \code{MULTILINESTRING} geometry, using the coordinate reference system.  \code{POINT}, \code{MULTIPOINT}, \code{POLYGON} or \code{MULTIPOLYGON} geometries return zero.
#' @seealso \link{st_dimension}, \link{st_cast} to convert geometry types
#'
#' @examples
#' line = st_sfc(st_linestring(rbind(c(30,30), c(40,40))), crs = 4326)
#' st_length(line)
#'
#' outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#' hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#' hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#'
#' poly = st_polygon(list(outer, hole1, hole2))
#' mpoly = st_multipolygon(list(
#' 	list(outer, hole1, hole2),
#' 	list(outer + 12, hole1 + 12)
#' ))
#'
#' st_length(st_sfc(poly, mpoly))
st_length = function(x, ...) {
	x = st_geometry(x)

	if (isTRUE(st_is_longlat(x))) {
		if (sf_use_s2())
			set_units(s2::s2_length(x, ...), "m", mode = "standard")
		else {
			if (! requireNamespace("lwgeom", quietly = TRUE))
				stop("package lwgeom required, please install it first")
			lwgeom::st_geod_length(x)
		}
	} else {
		ret = CPL_length(x)
		ret[is.nan(ret)] = NA
		if (!is.null(u <- st_crs(x)$ud_unit))
			units(ret) = u
		if (!is.null(to_m <- st_crs(x)$to_meter) && !is.na(to_m) && !inherits(ret, "units"))
			ret = set_units(ret * to_m, "m", mode = "standard")
		ret
	}
}

message_longlat = function(caller) {
	m = paste("although coordinates are longitude/latitude,", caller, "assumes that they are planar")
	m = strwrap(m, width = getOption("width"))
	message(paste0(m, collapse = "\n"))
}


#' @name geos_measures
#' @export
#' @examples
#' st_perimeter(poly)
#' st_perimeter(mpoly)
st_perimeter = function(x, ...) {
	x = st_geometry(x)
	if (isTRUE(st_is_longlat(x))) { # for spherical geometries we use s2 
		if (!requireNamespace("s2", quietly = TRUE))
			stop("package s2 required to calculate the perimeter of spherical geometries")
		# ensure units are set to meters 
		units::set_units(
			s2::s2_perimeter(x, ...), 
			"m", 
			mode = "standard"
		)
	} else { # non-spherical geometries use lwgeom:
		if (!requireNamespace("lwgeom", quietly = TRUE))
			stop("package lwgeom required, please install it first")
		# note that units are handled appropriately by lwgeom
		lwgeom::st_perimeter(x)
	}
}

#' Compute geometric measurements
#'
#' Compute Euclidean or great circle distance between pairs of geometries; compute, the area or the length of a set of geometries.
#' @name geos_measures
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param y object of class \code{sf}, \code{sfc} or \code{sfg}, defaults to \code{x}
#' @param ... passed on to \link[s2]{s2_distance}, \link[s2]{s2_distance_matrix}, or \link[s2]{s2_perimeter}
#' @param dist_fun deprecated
#' @param by_element logical; if \code{TRUE}, return a vector with distance between the first elements of \code{x} and \code{y}, the second, etc; an error is raised if \code{x} and \code{y} are not the same length. If \code{FALSE}, return the dense matrix with all pairwise distances.
#' @param which character; for Cartesian coordinates only: one of \code{Euclidean}, \code{Hausdorff} or \code{Frechet}; for geodetic coordinates, great circle distances are computed; see details
#' @param par for \code{which} equal to \code{Hausdorff} or \code{Frechet}, optionally use a value between 0 and 1 to densify the geometry
#' @param tolerance ignored if \code{st_is_longlat(x)} is \code{FALSE}; otherwise, if set to a positive value, the first distance smaller than \code{tolerance} will be returned, and true distance may be smaller; this may speed up computation. In meters, or a \code{units} object convertible to meters.
#' @return If \code{by_element} is \code{FALSE} \code{st_distance} returns a dense numeric matrix of dimension length(x) by length(y); otherwise it returns a numeric vector the same length as \code{x} and \code{y} with an error raised if the lengths of \code{x} and \code{y} are unequal. Distances involving empty geometries are \code{NA}.
#' @details great circle distance calculations use by default spherical distances (\link[s2]{s2_distance} or \link[s2]{s2_distance_matrix}); if \code{sf_use_s2()} is \code{FALSE}, ellipsoidal distances are computed using \link[lwgeom]{st_geod_distance} which uses function \code{geod_inverse} from GeographicLib (part of PROJ); see Karney, Charles FF, 2013, Algorithms for geodesics, Journal of Geodesy 87(1), 43--55
#' @examples
#' p = st_sfc(st_point(c(0,0)), st_point(c(0,1)), st_point(c(0,2)))
#' st_distance(p, p)
#' st_distance(p, p, by_element = TRUE)
#' @export
st_distance = function(x, y, ..., dist_fun, by_element = FALSE, 
		which = ifelse(isTRUE(st_is_longlat(x)), "Great Circle", "Euclidean"), 
		par = 0.0, tolerance = 0.0) {

	missing_y = FALSE
	if (missing(y)) {
		y = x
		missing_y = TRUE
	} else
		stopifnot(st_crs(x) == st_crs(y))

	if (! missing(dist_fun))
		stop("dist_fun is deprecated: lwgeom is used for distance calculation")

	x = st_geometry(x)
	y = st_geometry(y)
	if (by_element)
		stopifnot(!missing_y, length(x) == length(y))

	if (isTRUE(st_is_longlat(x)) && which == "Great Circle") {
		if (sf_use_s2()) {
			ret = if (by_element)
					s2::s2_distance(x, y, ...)
				else
					s2::s2_distance_matrix(x, y, ...)
			set_units(ret, "m", mode = "standard")
		} else { # lwgeom:
			if (which != "Great Circle")
				stop("for non-great circle distances, data should be projected; see st_transform()")
			units(tolerance) = as_units("m")
			if (by_element) {
				crs = st_crs(x)
				dist_ll = function(x, y, tolerance)
					lwgeom::st_geod_distance(st_sfc(x, crs = crs), st_sfc(y, crs = crs),
						tolerance = tolerance)
				d = mapply(dist_ll, x, y, tolerance = tolerance)
				units(d) = units(st_crs(x)$SemiMajor)
				d
			} else
				lwgeom::st_geod_distance(x, y, tolerance)
		}
	} else {
		d = if (by_element) {
				if (inherits(x, "sfc_POINT") && inherits(y, "sfc_POINT") && which == "Euclidean") {
					xc = st_coordinates(x)
					yc = st_coordinates(y)
					sqrt((xc[,1] - yc[,1])^2 + (xc[,2] - yc[,2])^2)
				} else
					mapply(st_distance, x, y, by_element = FALSE, which = which, par = par)
			} else {
				if (missing_y && inherits(x, "sfc_POINT") && which == "Euclidean")
					as.matrix(stats::dist(st_coordinates(x)))
				else
					CPL_geos_dist(x, y, which, par)
			}
		d[is.nan(d)] = NA_real_
		if (!is.null(u <- st_crs(x)$ud_unit))
			units(d) = u
		d
	}
}

check_lengths = function (dots) {
	lengths <- vapply(dots, length, integer(1))
	non_constant_lengths <- unique(lengths[lengths != 1])
	if (length(non_constant_lengths) == 0) {
		1
	}
	else if (length(non_constant_lengths) == 1) {
		non_constant_lengths
	}
	else {
		lengths_label <- paste0(non_constant_lengths, collapse = ", ")
		stop(sprintf("Incompatible lengths: %s", lengths_label), 
			call. = FALSE)
	}
}

recycle_common = function (dots) {
	final_length <- check_lengths(dots)
	lapply(dots, rep_len, final_length)
}


#' Project point on linestring, interpolate along a linestring
#'
#' Project point on linestring, interpolate along a linestring
#' @param line object of class `sfc` with `LINESTRING` geometry
#' @param point object of class `sfc` with `POINT` geometry
#' @param normalized logical; if `TRUE`, use or return distance normalised to 0-1
#' @name st_line_project_point
#' @returns `st_line_project` returns the distance(s) of point(s) along line(s), when projected on the line(s)
#' @export
#' @details
#' arguments `line`, `point` and `dist` are recycled to common length when needed
#' @examples
#' st_line_project(st_as_sfc("LINESTRING (0 0, 10 10)"), st_as_sfc(c("POINT (0 0)", "POINT (5 5)")))
#' st_line_project(st_as_sfc("LINESTRING (0 0, 10 10)"), st_as_sfc("POINT (5 5)"), TRUE)
st_line_project = function(line, point, normalized = FALSE) {
	stopifnot(inherits(line, "sfc"), inherits(point, "sfc"),
		all(st_dimension(line) == 1), all(st_dimension(point) == 0),
		is.logical(normalized), length(normalized) == 1,
		st_crs(line) == st_crs(point))
	line = st_cast(line, "LINESTRING")
	point = st_cast(point, "POINT")
	if (isTRUE(st_is_longlat(line)))
		message_longlat("st_project_point")
	recycled = recycle_common(list(line, point))
	CPL_line_project(recycled[[1]], recycled[[2]], normalized)
}
