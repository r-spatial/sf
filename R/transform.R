chk_pol = function(x, dim = class(x)[1]) {
	PolClose = function(y) {
		if (any(head(y[[1]], 1) != tail(y[[1]], 1))) # close
			y[[1]] = rbind(y[[1]], head(y[[1]], 1))
		else if (nrow(y[[1]]) == 3) # closed, but line
			return(st_polygon(dim = dim))
		y
	}
	if (length(x) > 0 && nrow(x[[1]]) > 2)
		PolClose(x)
	else
		st_polygon(dim = dim)
}

chk_mpol = function(x) {
	cln = lapply(x, function(y) unclass(chk_pol(y, class(x)[1])))
	empty = if (length(cln))
			lengths(cln) == 0
		else
			TRUE
	# print(empty)
	st_multipolygon(cln[!empty], dim = class(x)[1])
}

sanity_check = function(x) {
	d = st_dimension(x) # flags empty geoms as NA
	if (any(d == 2, na.rm = TRUE)) { # the polygon stuff
		if (inherits(x, "sfc_POLYGON"))
			st_sfc(lapply(x, chk_pol), crs = st_crs(x))
		else if (inherits(x, "sfc_MULTIPOLYGON"))
			st_sfc(lapply(x, chk_mpol), crs = st_crs(x))
		else
			stop(paste("no check implemented for", class(x)[1]))
	} else
		x # nocov
}

#' Transform or convert coordinates of simple feature
#'
#' Transform or convert coordinates of simple feature
#'
#' @param x object of class sf, sfc or sfg
#' @param crs target coordinate reference system: object of class `crs`, or input string for \link{st_crs}
#' @param ... ignored
#' @param aoi area of interest, in degrees: 
#' WestLongitude, SouthLatitude, EastLongitude, NorthLatitude
#' @param pipeline character; coordinate operation pipeline, for overriding the default operation
#' @param reverse boolean; has only an effect when \code{pipeline} is defined:
#' if \code{TRUE}, the inverse operation of the pipeline is applied
#' @param desired_accuracy numeric; Only coordinate operations that offer an accuracy of 
#' at least the one specified will be considered; a negative value disables this feature
#' (requires GDAL >= 3.3)
#' @param allow_ballpark logical; are ballpark (low accuracy) transformations allowed? 
#' (requires GDAL >= 3.3)
#' @param partial logical; allow for partial projection, if not all points of a geometry can be projected (corresponds to setting environment variable \code{OGR_ENABLE_PARTIAL_REPROJECTION} to \code{TRUE})
#' @param check logical; if \code{TRUE}, perform a sanity check on resulting polygons
#' @details Transforms coordinates of object to new projection. 
#' Features that cannot be transformed are returned as empty geometries.
#' Transforms using the \code{pipeline=} argument may fail if there is
#' ambiguity in the axis order of the specified coordinate reference system;
#' if you need the traditional GIS order, use \code{"OGC:CRS84"}, not
#' \code{"EPSG:4326"}. Extra care is needed with the ESRI Shapefile format,
#' because WKT1 does not store axis order unambigiously.
#'
#' @seealso Projecting simple feature geometries 
#' to projections not supported by GDAL may be done by 
#' \link[lwgeom]{st_transform_proj}, part of package lwgeom.
#' 
#' \link{sf_project} projects a matrix of coordinates, bypassing GDAL altogether
#' @examples
#' p1 = st_point(c(7,52))
#' p2 = st_point(c(-30,20))
#' sfc = st_sfc(p1, p2, crs = 4326)
#' sfc
#' st_transform(sfc, 3857)
#' @export
st_transform = function(x, crs, ...) UseMethod("st_transform")

#' @name st_transform
#' @export
#' @examples
#' st_transform(st_sf(a=2:1, geom=sfc), "+init=epsg:3857")
#' try(st_transform(sfc, 3857, aoi = c(-280,-90,180,90)))
#' if (sf_extSoftVersion()["GDAL"] >= "3.0.0") {
#'   st_transform(sfc, pipeline =
#' 	  "+proj=pipeline +step +proj=axisswap +order=2,1") # reverse axes
#'   st_transform(sfc, pipeline =
#' 	  "+proj=pipeline +step +proj=axisswap +order=2,1", reverse = TRUE) # also reverse axes
#' }
st_transform.sfc = function(x, crs = st_crs(x), ..., 
		aoi = numeric(0), pipeline = character(0), reverse = FALSE,
		desired_accuracy = -1.0, allow_ballpark = TRUE,
		partial = TRUE, check = FALSE) {

	crs_missing = missing(crs)
	if (length(pipeline) == 0) {
		if (is.na(st_crs(x)))
			stop("cannot transform sfc object with missing crs")
		if (missing(crs))
			stop("argument crs cannot be missing")
	} else 
		stopifnot(length(pipeline) == 1)

	crs_input = crs
	crs = make_crs(crs)

	# FIXME: check for wkt here too WKT TODO:
	if (grepl("+proj=geocent", crs$proj4string) && length(x) && Dimension(x[[1]]) == "XY") # add z:
		x = st_zm(x, drop = FALSE, what = "Z")

	if (partial) {
		orig = Sys.getenv("OGR_ENABLE_PARTIAL_REPROJECTION")
		if (orig != "")
			on.exit(Sys.setenv(OGR_ENABLE_PARTIAL_REPROJECTION = orig))
		Sys.setenv(OGR_ENABLE_PARTIAL_REPROJECTION = "TRUE")
	}

	if (length(pipeline)) {
		if (!crs_missing && !is.na(crs)) { # verify pipeline is a legitimate option:
			if (!pipeline[1] %in% sf_proj_pipelines(st_crs(x), crs_input)$definition)
				warning("pipeline not found in PROJ-suggested candidate transformations")
		} else
			crs = NA_crs_  # to avoid st_crs(x) is crs of the returned object
	}
	ret = st_sfc(structure(CPL_transform(x, crs, aoi, pipeline, reverse, desired_accuracy,
			allow_ballpark), single_type = NULL, crs = crs)) # hard-sets crs to new crs
	if (check)
		sanity_check(ret)
	else
		ret
}

#' @name st_transform
#' @export
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' st_area(nc[1,]) # area from long/lat
#' st_area(st_transform(nc[1,], 32119)) # NC state plane, m
#' st_area(st_transform(nc[1,], 2264)) # NC state plane, US foot
#' library(units)
#' set_units(st_area(st_transform(nc[1,], 2264)), m^2)
st_transform.sf = function(x, crs = st_crs(x), ...) {
	x[[ attr(x, "sf_column") ]] = st_transform(st_geometry(x), crs, ...)
	x
}

#' @name st_transform
#' @export
#' @details The \code{st_transform} method for \code{sfg} objects assumes that the CRS of the object is available as an attribute of that name.
#' @examples
#' st_transform(structure(p1, proj4string = "+init=epsg:4326"), "+init=epsg:3857")
st_transform.sfg = function(x, crs = st_crs(x), ...) {
	x = st_sfc(x, crs = attr(x, "proj4string"))
	if (missing(crs))
		stop("argument crs cannot be missing")
	crs = make_crs(crs)
	structure(st_transform(x, crs, ...)[[1]], crs = crs)
}

#' @name st_transform
#' @export
st_wrap_dateline = function(x, options, quiet) UseMethod("st_wrap_dateline")

#' @name st_transform
#' @param options character; should have "WRAPDATELINE=YES" to function; another parameter that is used is "DATELINEOFFSET=10" (where 10 is the default value)
#' @param quiet logical; print options after they have been parsed?
#' @export
#' @examples
#' st_wrap_dateline(st_sfc(st_linestring(rbind(c(-179,0),c(179,0))), crs = 4326))
#' library(maps)
#' wrld <- st_as_sf(maps::map("world", fill = TRUE, plot = FALSE))
#' wrld_wrap <- st_wrap_dateline(wrld, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"),
#'    quiet = TRUE)
#' wrld_moll <- st_transform(wrld_wrap, "+proj=moll")
#' plot(st_geometry(wrld_moll), col = "transparent")
#' @details For a discussion of using \code{options}, see \url{https://github.com/r-spatial/sf/issues/280} and \url{https://github.com/r-spatial/sf/issues/541}
st_wrap_dateline.sfc = function(x, options = "WRAPDATELINE=YES", quiet = TRUE) {
	if (is.na(st_crs(x)))
		warning("crs not set: assuming geographic coordinates")
	else
		stopifnot(st_is_longlat(x))
	stopifnot(is.character(options))
	stopifnot(is.logical(quiet) && length(quiet) == 1)
	st_sfc(CPL_wrap_dateline(x, options, quiet), crs = st_crs(x))
}

#' @name st_transform
#' @export
st_wrap_dateline.sf = function(x, options = "WRAPDATELINE=YES", quiet = TRUE) {
	st_set_geometry(x, st_sfc(CPL_wrap_dateline(st_geometry(x), options, quiet), crs = st_crs(x)))
}

#' @name st_transform
#' @export
st_wrap_dateline.sfg = function(x, options = "WRAPDATELINE=YES", quiet = TRUE) {
	st_sfc(CPL_wrap_dateline(st_geometry(x), options, quiet), crs = st_crs(x))[[1]]
}
