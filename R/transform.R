
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
#' @param crs coordinate reference system: integer with the EPSG code, or character with proj4string
#' @param ... ignored
#' @param aoi area of interest, in degrees: 
#' WestLongitude, SouthLatitude, EastLongitude, NorthLatitude
#' @param pipeline character; proj4 or WKT coordinate operation, to override the default operation
#' @param reverse boolean; if \code{TRUE}, the inverse operation of the pipeline is applied
#' @param partial logical; allow for partial projection, if not all points of a geometry can be projected (corresponds to setting environment variable \code{OGR_ENABLE_PARTIAL_REPROJECTION} to \code{TRUE})
#' @param check logical; perform a sanity check on resulting polygons?
#' @details Transforms coordinates of object to new projection. Features that cannot be transformed are returned as empty geometries.
#'
#' Projecting to projections not supported by GDAL may be done by \link[lwgeom]{st_transform_proj}, part of package lwgeom.
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
		partial = TRUE, check = FALSE) {

	if (length(pipeline) == 0) {
		if (is.na(st_crs(x)))
			stop("cannot transform sfc object with missing crs")
		if (missing(crs))
			stop("argument crs cannot be missing")
	}

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

	if (crs != st_crs(x)) {
		ret = structure(CPL_transform(x, crs, aoi, pipeline, reverse),
			single_type = NULL, crs = crs)
		ret = st_sfc(ret)
		if (check)
			sanity_check(ret)
		else
			ret
	} else if (length(pipeline))
		st_sfc(CPL_transform(x, crs, aoi, pipeline, reverse))
	else
		x
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
#' @param type character; one of \code{have_datum_files}, \code{proj}, \code{ellps}, \code{datum}, \code{units} or \code{prime_meridians}; see Details.
#' @param path character; PROJ search path to be set
#' @export
#' @details \code{sf_proj_info} lists the available projections, ellipses, datums, units, or data search path of the PROJ library when \code{type} is equal to proj, ellps, datum, units or path; when \code{type} equals \code{have_datum_files} a boolean is returned indicating whether datum files are installed and accessible (checking for \code{conus}).
#'
#' PROJ >= 6 does not provide option \code{type = "datums"}. PROJ < 6 does not provide the option \code{type = "prime_meridians"}.
#' @examples
#' sf_proj_info("datum")
sf_proj_info = function(type = "proj", path) {

	if (type == "have_datum_files")
		return(CPL_have_datum_files(0))

	if (type == "path")
		return(CPL_get_data_dir(FALSE))
	
	if (!missing(path) && is.character(path))
		return(invisible(CPL_set_data_dir(path)))

	if (type == "network")
		return(CPL_is_network_enabled(TRUE))

    opts <- c("proj", "ellps", "datum", "units", "prime_meridians")
    if (!(type %in% opts))
		stop("unknown type") # nocov
    t <- as.integer(match(type[1], opts) - 1)
	res = CPL_proj_info(as.integer(t))
    if (type == "proj")
		res$description <- sapply(strsplit(as.character(res$description), "\n"),
			function(x) x[1])
    data.frame(res)
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

st_to_s2 = function(x) {
	# to geocentric, spherical, unit sphere:
	st_transform(x, st_crs("+proj=geocent +a=1 +b=1 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")) # nocov
}

#' directly transform a set of coordinates
#'
#' directly transform a set of coordinates
#' @param from character description of source CRS, or object of class \code{crs} 
#' @param to character description of target CRS, or object of class \code{crs} 
#' @param pts two-column numeric matrix, or object that can be coerced into a matrix
#' @param keep logical value controlling the handling of unprojectable points. If
#' `keep` is `TRUE`, then such points will yield `Inf` or `-Inf` in the
#' return value; otherwise an error is reported and nothing is returned.
#' @param warn logical; if \code{TRUE}, warn when non-finite values are generated
#' @param authority_compliant logical; \code{TRUE} means handle axis order authority compliant (e.g. EPSG:4326 implying x=lat, y=lon), \code{FALSE} means use visualisation order (i.e. always x=lon, y=lat)
#' @return two-column numeric matrix with transformed/converted coordinates, returning invalid values as \code{Inf}
#' @export
sf_project = function(from, to, pts, keep = FALSE, warn = TRUE, authority_compliant = st_axis_order()) {
	if (inherits(from, "crs"))
		from = from$wkt
	if (inherits(to, "crs"))
		to = to$wkt
	if (!is.logical(keep) || 1 != length(keep))
		stop("'keep' must be single-length logical value")
	v = CPL_proj_is_valid(from)
	if (!v[[1]])
		stop(paste0(v[[2]], ": ", from))
	v = CPL_proj_is_valid(to)
	if (!v[[1]])
		stop(paste0(v[[2]], ": ", to))
	CPL_proj_direct(as.character(c(from[1], to[1])), as.matrix(pts), 
			if (keep) 1 else 0, warn, authority_compliant)
}
