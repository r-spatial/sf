
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

	crs_missing = missing(crs)
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

	if (!crs_missing && crs != st_crs(x)) {
		ret = structure(CPL_transform(x, crs, aoi, pipeline, reverse),
			single_type = NULL, crs = crs)
		ret = st_sfc(ret)
		if (check)
			sanity_check(ret)
		else
			ret
	} else {
		if (length(pipeline)) # crs_missing: don't set CRS to st_crs(x):
			st_sfc(CPL_transform(x, crs, aoi, pipeline, reverse), crs = NA_crs_)
		else # crs_missing, no pipeline, do nothing:
			x 
	}
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
#' for PROJ >= 6, \code{sf_proj_info} does not provide option \code{type = "datums"}. 
#' PROJ < 6 does not provide the option \code{type = "prime_meridians"}.
#'
#' for PROJ >= 7.1.0, the "units" query of \code{sf_proj_info} returns the \code{to_meter} 
#' variable as numeric, previous versions return a character vector containing a numeric expression.
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

st_to_s2 = function(x) {
	# to geocentric, spherical, unit sphere:
	st_transform(x, st_crs("+proj=geocent +a=1 +b=1 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")) # nocov
}

#' directly transform a set of coordinates
#'
#' directly transform a set of coordinates
#' @param from character description of source CRS, or object of class \code{crs}, 
#' or pipeline describing a transformation
#' @param to character description of target CRS, or object of class \code{crs} 
#' @param pts two-column numeric matrix, or object that can be coerced into a matrix
#' @param keep logical value controlling the handling of unprojectable points. If
#' `keep` is `TRUE`, then such points will yield `Inf` or `-Inf` in the
#' return value; otherwise an error is reported and nothing is returned.
#' @param warn logical; if \code{TRUE}, warn when non-finite values are generated
#' @param authority_compliant logical; \code{TRUE} means handle axis order authority compliant (e.g. EPSG:4326 implying x=lat, y=lon), \code{FALSE} means use visualisation order (i.e. always x=lon, y=lat)
#' @return two-column numeric matrix with transformed/converted coordinates, returning invalid values as \code{Inf}
#' @export
sf_project = function(from, to = character(0), pts, keep = FALSE, warn = TRUE, 
		authority_compliant = st_axis_order()) {

	if (!is.logical(keep) || length(keep) != 1 || is.na(keep))
		stop("'keep' must be single-length non-NA logical value")
	proj_from_crs = function(x) {
		if (inherits(x, "crs")) {
			x = if (sf_extSoftVersion()["proj.4"] >= "6.0.0")
				x$wkt
			else
				x$proj4string
		}
		if (length(x)) {
			v = CPL_proj_is_valid(x)
			if (!v[[1]])
				stop(paste0(v[[2]], ": ", x))
			x[1]
		} else
			x # empty
	}

	from_to = c(proj_from_crs(from), proj_from_crs(to))
	if ((length(from_to) == 1) && !missing(authority_compliant))
		stop("when setting a projection pipeline, setting authority_compliant has no effect")

	CPL_proj_direct(from_to, as.matrix(pts), keep, warn, authority_compliant)
}

#' Manage PROJ settings
#' 
#' Manage PROJ search path and network settings
#' @param paths the search path to be set; omit if no paths need to be set
#' @return `sf_proj_search_paths()` returns the search path (possibly after setting it)
#' @name proj_tools
#' @export
sf_proj_search_paths = function(paths = character(0)) {
	if (length(paths) == 0)
		CPL_get_proj_search_paths(paths) # get
	else
		CPL_set_proj_search_paths(as.character(paths)) # set
}

#' @param enable logical; set this to enable (TRUE) or disable (FALSE) the proj network search facility
#' @param url character; use this to specify and override the default proj network CDN 
#' @return `sf_proj_network` when called without arguments returns a logical indicating whether
#' network search of datum grids is enabled, when called with arguments it returns a character
#' vector with the URL of the CDN used (or specified with `url`).
#' @name proj_tools
#' @export
sf_proj_network = function(enable = FALSE, url = character(0)) {
	if (missing(enable) && missing(url))
		CPL_is_network_enabled()
	else 
		CPL_enable_network(url, enable)
}

#' @param source_crs object of class `crs` or character
#' @param target_crs object of class `crs` or character
#' @param authority character; constrain output pipelines to those of authority
#' @param AOI length four numeric; desired area of interest for the resulting 
#' coordinate transformations (west, south, east, north, in degrees).
#' For an area of interest crossing the anti-meridian, west will be greater than east.
#' @param Use one of "NONE", "BOTH", "INTERSECTION", "SMALLEST", indicating how AOI's
#' of source_crs and target_crs are being used
#' @param grid_availability character; one of "USED" (Grid availability is only used for sorting 
#' results. Operations where some grids are missing will be sorted last), "DISCARD"
#' (Completely discard an operation if a required grid is missing)
#' , "IGNORED" (Ignore grid availability at all. Results will be presented as if all grids were 
#' available.), or "AVAILABLE" (Results will be presented as if grids known to PROJ (that is 
#' registered in the grid_alternatives table of its database) were available. Used typically when 
#' networking is enabled.)
#' @param accuracy numeric; only return pipelines with at least this accuracy
#' @param strict_containment logical; default FALSE; permit partial matching of the area
#' of interest; if TRUE strictly contain the area of interest.
#' The area of interest is either as given in AOI, or as implied by the
#' source/target coordinate reference systems 
#' @param axis_order_authority_compliant logical; if FALSE always 
#' choose ‘x’ or longitude for the first 
#' axis; if TRUE, follow the axis orders given by the coordinate reference systems when 
#' constructing the for the first axis; if FALSE, follow the axis orders given by
#' @return `sf_proj_pipelines` returns a table with candidate coordinate transformation
#' pipelines along with their accuracy; `NA` accuracy indicates ballpark accuracy.
#' @name proj_tools
#' @export
sf_proj_pipelines = function(source_crs, target_crs, authority = character(0), AOI = numeric(0),
		Use = "NONE", grid_availability = "USED", accuracy = -1.0, 
		strict_containment = FALSE, axis_order_authority_compliant = st_axis_order()) {
	stopifnot(!missing(source_crs), !missing(target_crs))
	if (inherits(source_crs, "crs"))
		source_crs = source_crs$wkt
	if (inherits(target_crs, "crs"))
		target_crs = target_crs$wkt
	stopifnot(is.character(source_crs), is.character(target_crs))

	ret = CPL_get_pipelines(c(source_crs, target_crs), as.character(authority), 
		as.numeric(AOI), as.character(Use), as.character(grid_availability),
		as.numeric(accuracy), as.logical(strict_containment), 
		as.logical(axis_order_authority_compliant))
	if (nrow(ret)) {
		if (substr(ret$definition[1], 1, 1) != "+") # paste + to every word
			ret$definition = 
				sapply(strsplit(ret$definition, " "), 
					function(x) paste0(paste0("+", x), collapse=" "))
		ret$containment = strict_containment
		structure(ret, class = c("proj_pipelines", "data.frame"),
			source_crs = source_crs, target_crs = target_crs)
	} else
		invisible(NULL)
}

#' @export
print.proj_pipelines = function(x, ...) {
	cat("Candidate coordinate operations found: ", nrow(x), "\n")
	nos <- which(!x$instantiable)
	if (length(nos) > 0L)
		xx <- x[-nos,]
	else
		xx <- x
	xx <- xx[order(xx$accuracy),]
	y = xx[1,]
	cat("Strict containment:    ", y$containment, "\n")
	cat("Axis order auth compl: ", y$axis_order, "\n")
	cat("Source: ", attr(x, "source_crs"), "\n")
	cat("Target: ", attr(x, "target_crs"), "\n")
	if (is.na(y$accuracy))
		cat("Best instantiable operation has only ballpark accuracy", "\n")
	else
		cat("Best instantiable operation has accuracy:", y$accuracy, "m\n")
	cat("Description: ")
	desc <- strwrap(y$description, exdent=13, width=0.8*getOption("width"))
	if (length(desc) == 1L)
		cat(desc, "\n")
	else
		cat(desc, sep="\n")
	cat("Definition:  ")
	def <- strwrap(y$definition, exdent=13, width=0.8*getOption("width"))
	if (length(def) == 1L)
		cat(def, "\n")
	else
		cat(def, sep="\n")
	# nos:
	if (length(nos) > 0L) {
		grds <- attr(x, "grids")
		for (i in seq(along=nos)) {
			grd <- grds[[nos[i]]]
			ii <- length(grd)
			if (ii > 0L) {
				cat("Operation", nos[i], "is lacking", ii,
				ifelse(ii == 1L, "grid", "grids"),
					"with accuracy", x$accuracy[nos[i]], "m\n")
				for (j in 1:ii) {
					cat("Missing grid:", grd[[j]][[1]], "\n")
					if (nzchar(grd[[j]][[2]])) cat("Name:", grd[[j]][[2]], "\n")
					if (nzchar(grd[[j]][[4]])) cat("URL:", grd[[j]][[4]], "\n")
				}
			}
		}
	}
	invisible(x)
}
