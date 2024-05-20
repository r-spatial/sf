
#' @name st_transform
#' @param type character; one of `have_datum_files`, `proj`, `ellps`, `datum`, `units`, `path`, or `prime_meridians`; see Details.
#' @param path character; PROJ search path to be set
#' @export
#' @details \code{sf_proj_info} lists the available projections, ellipses, datums, units, or data search path of the PROJ library when \code{type} is equal to proj, ellps, datum, units or path; when \code{type} equals \code{have_datum_files} a boolean is returned indicating whether datum files are installed and accessible (checking for \code{conus}). `path` returns the `PROJ_INFO.searchpath` field directly, as a single string with path separaters (`:`  or `;`).
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
		return(CPL_get_data_dir(TRUE))
	
	if (!missing(path) && is.character(path))
		return(invisible(unique(CPL_set_data_dir(path, TRUE))))

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

#' directly transform a set of coordinates
#'
#' directly transform a set of coordinates
#' @param from character description of source CRS, or object of class \code{crs}, 
#' or pipeline describing a transformation
#' @param to character description of target CRS, or object of class \code{crs} 
#' @param pts two-, three- or four-column numeric matrix, or object that can be coerced into a matrix; columns 3 and 4 contain z and t values.
#' @param keep logical value controlling the handling of unprojectable points. If
#' `keep` is `TRUE`, then such points will yield `Inf` or `-Inf` in the
#' return value; otherwise an error is reported and nothing is returned.
#' @param warn logical; if \code{TRUE}, warn when non-finite values are generated
#' @param authority_compliant logical; \code{TRUE} means handle axis order authority compliant (e.g. EPSG:4326 implying x=lat, y=lon), \code{FALSE} means use visualisation order (i.e. always x=lon, y=lat)
#' @return two-column numeric matrix with transformed/converted coordinates, returning invalid values as \code{Inf}
#' @export
sf_project = function(from = character(0), to = character(0), pts, keep = FALSE, warn = TRUE, 
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
			x # empty: character(0)
	}

	from_to = c(proj_from_crs(from), proj_from_crs(to))
	if ((length(from_to) == 1) && !missing(authority_compliant))
		stop("when specifying a projection pipeline, setting authority_compliant has no effect")

	CPL_proj_direct(from_to, as.matrix(pts), keep, warn, authority_compliant)
}

#' Manage PROJ settings
#' 
#' Query or manage PROJ search path and network settings
#' @param paths the search path to be set; omit if paths need to be queried
#' @param with_proj logical; if `NA` set for both GDAL and PROJ, otherwise set either for PROJ (`TRUE`) or GDAL (`FALSE`)
#' @return `sf_proj_search_paths()` returns the search path (possibly after setting it)
#' @name proj_tools
#' @export
sf_proj_search_paths = function(paths = character(0), with_proj = NA) {
	if (length(paths) == 0)
		CPL_get_data_dir(FALSE)
	else {
		if (is.na(with_proj) || !isTRUE(with_proj))
			CPL_set_data_dir(as.character(paths), FALSE) # set GDAL
		if (is.na(with_proj) || isTRUE(with_proj)) { # set for PROJ
			if (length(paths) > 1) {
				paths = paste0(paths, collapse = .Platform$path.sep)
				message(paste("setting proj path(s) to", paths))
			}
			CPL_set_data_dir(as.character(paths), TRUE)
		}
	}
}

#' @param enable logical; set this to enable (`TRUE`) or disable (`FALSE`) the proj network search facility
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

#' @param source_crs,target_crs object of class `crs` or character
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
#' @param desired_accuracy numeric; only return pipelines with at least this accuracy
#' @param strict_containment logical; default `FALSE`; permit partial matching of the area
#' of interest; if `TRUE` strictly contain the area of interest.
#' The area of interest is either as given in AOI, or as implied by the
#' source/target coordinate reference systems 
#' @param axis_order_authority_compliant logical; if `FALSE` always 
#' choose ‘x’ or longitude for the first 
#' axis; if TRUE, follow the axis orders given by the coordinate reference systems when 
#' constructing the for the first axis; if `FALSE`, follow the axis orders given by
#' @return `sf_proj_pipelines()` returns a table with candidate coordinate transformation
#' pipelines along with their accuracy; `NA` accuracy indicates ballpark accuracy.
#' @name proj_tools
#' @export
sf_proj_pipelines = function(source_crs, target_crs, authority = character(0), AOI = numeric(0),
		Use = "NONE", grid_availability = "USED", desired_accuracy = -1.0, 
		strict_containment = FALSE, axis_order_authority_compliant = st_axis_order()) {
	stopifnot(!missing(source_crs), !missing(target_crs))
	if (inherits(source_crs, "crs"))
		source_crs = source_crs$wkt
	if (inherits(target_crs, "crs"))
		target_crs = target_crs$wkt
	stopifnot(is.character(source_crs), is.character(target_crs))

	ret = CPL_get_pipelines(c(source_crs, target_crs), as.character(authority), 
		as.numeric(AOI), as.character(Use), as.character(grid_availability),
		as.numeric(desired_accuracy), as.logical(strict_containment), 
		as.logical(axis_order_authority_compliant))
	if (nrow(ret)) {
		if (!startsWith(ret$definition[1], "+")) # paste + to every word
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
		for (i in seq(along.with = nos)) {
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
