#' Transform or convert coordinates of simple feature
#'
#' Transform or convert coordinates of simple feature
#'
#' @param x object of class sf, sfc or sfg
#' @param crs coordinate reference system: integer with the EPSG code, or character with proj4string
#' @param ... ignored
#' @param partial logical; allow for partial projection, if not all points of a geometry can be projected (corresponds to setting environment variable \code{OGR_ENABLE_PARTIAL_REPROJECTION} to \code{TRUE})
#' @param check logical; perform a sanity check on resulting polygons?
#' @param use_gdal logical; if \code{FALSE}, arguments \code{check} and \code{partial} get ignored, and projection is directly carried out by \code{lwgeom_transform} which ignores the GDAL api. This allows proj.4 parameters such as \code{+over} to act (https://github.com/r-spatial/sf/issues/511) and certain projections such as \code{wintri} which do not have an inverse, to work (https://github.com/r-spatial/sf/issues/509). This requires that sf is built by linking to \code{liblwgeom}.
#' @details Transforms coordinates of object to new projection. Features that cannot be transformed are returned as empty geometries.
#' @examples
#' p1 = st_point(c(7,52))
#' p2 = st_point(c(-30,20))
#' sfc = st_sfc(p1, p2, crs = 4326)
#' sfc
#' st_transform(sfc, 3857)
#' @export
st_transform = function(x, crs, ...) UseMethod("st_transform")

chk_pol = function(x, dim = class(x)[1]) {
	PolClose = function(y) {
		if (any(head(y[[1]], 1) != tail(y[[1]], 1)))
			y[[1]] = rbind(y[[1]], head(y[[1]], 1))
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
        x
}

#' @name st_transform
#' @export
#' @examples
#' st_transform(st_sf(a=2:1, geom=sfc), "+init=epsg:3857")
st_transform.sfc = function(x, crs, ..., partial = TRUE, check = FALSE, use_gdal = TRUE) {
	if (is.na(st_crs(x)))
		stop("sfc object should have crs set")
	if (missing(crs))
		stop("argument crs cannot be missing")

	if (! use_gdal) {
		if (inherits(crs, "crs"))
			crs = crs$proj4string
		return(st_sfc(CPL_lwgeom_transform(x, c(st_crs(x)$proj4string, crs))))
	}

	crs = make_crs(crs)

	if (grepl("+proj=geocent", crs$proj4string) && length(x) && Dimension(x[[1]]) == "XY") # add z:
		x = st_zm(x, drop = FALSE, what = "Z")

	if (partial) {
		orig = Sys.getenv("OGR_ENABLE_PARTIAL_REPROJECTION")
		if (orig != "")
			on.exit(Sys.setenv(OGR_ENABLE_PARTIAL_REPROJECTION = orig))
		Sys.setenv(OGR_ENABLE_PARTIAL_REPROJECTION = "TRUE")
	}

	if (crs != st_crs(x)) { # transform:
		ret = structure(CPL_transform(x, crs$proj4string),
			single_type = NULL, crs = crs)
		ret = st_sfc(ret)
		if (check)
			sanity_check(ret)
		else
			ret
	} else
		x
}

#' @name st_transform
#' @export
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' st_area(nc[1,]) # area, using geosphere::areaPolygon
#' st_area(st_transform(nc[1,], 32119)) # NC state plane, m
#' st_area(st_transform(nc[1,], 2264)) # NC state plane, US foot
#' library(units)
#' set_units(st_area(st_transform(nc[1,], 2264)), m^2)
st_transform.sf = function(x, crs, ...) {
	x[[ attr(x, "sf_column") ]] = st_transform(st_geometry(x), crs, ...)
	x
}

#' @name st_transform
#' @export
#' @details The \code{st_transform} method for \code{sfg} objects assumes that the CRS of the object is available as an attribute of that name.
#' @examples
#' st_transform(structure(p1, proj4string = "+init=epsg:4326"), "+init=epsg:3857")
st_transform.sfg = function(x, crs , ...) {
	x = st_sfc(x, crs = attr(x, "proj4string"))
	if (missing(crs))
		stop("argument crs cannot be missing")
	crs = make_crs(crs)
	structure(st_transform(x, crs, ...)[[1]], crs = crs)
}

#' @name st_transform
#' @param type character; one of \code{proj}, \code{ellps}, \code{datum} or \code{units}
#' @export
#' @details \code{st_proj_info} lists the available projections, ellipses, datums or units supported by the Proj.4 library
#' @examples
#' st_proj_info("datum")
st_proj_info = function(type = "proj") {
    opts <- c("proj", "ellps", "datum", "units")
    if (!(type %in% opts)) stop("unknown type")
    t <- as.integer(match(type[1], opts) - 1)
	res = CPL_proj_info(as.integer(t))
    if (type == "proj")
		res$description <- sapply(strsplit(as.character(res$description), "\n"),
			function(x) x[1])
    data.frame(res)
}

#' @name st_transform
#' @param options character; should have "WRAPDATELINE=YES" to function; another parameter that is used is "DATELINEOFFSET=10" (where 10 is the default value)
#' @param quiet logical; print options after they have been parsed?
#' @export
#' @examples
#' st_wrap_dateline(st_sfc(st_linestring(rbind(c(-179,0),c(179,0))), crs = 4326))
#' @details For a discussion of using \code{options}, see \url{https://github.com/r-spatial/sf/issues/280}
st_wrap_dateline = function(x, options = "WRAPDATELINE=YES", quiet = TRUE) {
	stopifnot(st_is_longlat(x))
	stopifnot(is.character(options))
	stopifnot(is.logical(quiet) && length(quiet) == 1)
	st_sfc(CPL_wrap_dateline(x, options, quiet), crs = st_crs(x))
}

st_to_s2 = function(x) {
	# geocentric, spherical:
	st_transform(x, 
		st_crs("+proj=geocent +a=1 +b=1 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
}
