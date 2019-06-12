#' @importFrom utils head tail object.size str
#' @importFrom stats runif aggregate na.omit setNames
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom methods as slotNames new slot
#' @importFrom grid convertUnit current.viewport linesGrob pathGrob pointsGrob polylineGrob unit viewport nullGrob convertHeight convertWidth
#' @import graphics
#' @importFrom grDevices rgb dev.size
#' @importFrom Rcpp evalCpp
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbGetQuery dbSendQuery dbReadTable dbExecute
#' @importFrom units as_units set_units make_unit_label drop_units
#' @importFrom classInt classIntervals
#' @useDynLib sf
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

setOldClass("sf")
setOldClass(c("sfc_POINT", "sfc"))
setOldClass(c("sfc_MULTIPOINT", "sfc"))
setOldClass(c("sfc_LINESTRING", "sfc"))
setOldClass(c("sfc_MULTILINESTRING", "sfc"))
setOldClass(c("sfc_POLYGON", "sfc"))
setOldClass(c("sfc_MULTIPOLYGON", "sfc"))
setOldClass(c("sfc_GEOMETRY", "sfc"))
setOldClass("sfg")

.sf_cache <- new.env(FALSE, parent=globalenv())

pathGrob <- NULL
.onLoad = function(libname, pkgname) {
	if (getRversion() < as.numeric_version("3.6")) { # nocov start
		pathGrob <<- function(..., pathId.lengths) {
			grid::pathGrob(...)
		}
	}
	load_gdal() # nocov end
}

.onUnload = function(libname, pkgname) {
	unload_gdal() # nocov
}

.onAttach = function(libname, pkgname) {
	m = paste0("Linking to GEOS ", strsplit(CPL_geos_version(TRUE), "-")[[1]][1],
		", GDAL ", CPL_gdal_version(), ", PROJ ", CPL_proj_version())
	packageStartupMessage(m)
	if (length(grep(CPL_geos_version(FALSE, TRUE), CPL_geos_version(TRUE))) != 1) { # nocov start
		packageStartupMessage("WARNING: different compile-time and runtime versions for GEOS found:")
		packageStartupMessage(paste(
			"Linked against:", CPL_geos_version(TRUE, TRUE),
			"compiled against:", CPL_geos_version(FALSE, TRUE)))
		packageStartupMessage("It is probably a good idea to reinstall sf, and maybe rgeos and rgdal too")
	} # nocov end
}

#' Provide the external dependencies versions of the libraries linked to sf
#'
#' Provide the external dependencies versions of the libraries linked to sf
#' @export
sf_extSoftVersion = function() {
	structure(c(CPL_geos_version(), CPL_gdal_version(), CPL_proj_version(),
		ifelse(CPL_gdal_with_geos(), "true", "false")),
		names = c("GEOS", "GDAL", "proj.4", "GDAL_with_GEOS"))
}

load_gdal <- function() {
	if (file.exists(system.file("proj/nad.lst", package = "sf")[1])) {
		# nocov start
		prj = system.file("proj", package = "sf")[1]
		if (! CPL_set_data_dir(prj)) { # if TRUE, uses C API to set path, leaving PROJ_LIB alone
			assign(".sf.PROJ_LIB", Sys.getenv("PROJ_LIB"), envir=.sf_cache)
			Sys.setenv("PROJ_LIB" = prj)
		}
		assign(".sf.GDAL_DATA", Sys.getenv("GDAL_DATA"), envir=.sf_cache)
		gdl = system.file("gdal", package = "sf")[1]
		Sys.setenv("GDAL_DATA" = gdl)
		# nocov end
	}
	CPL_gdal_init()
	register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse)
	if (inherits(try(units::as_units("link"), silent = TRUE), "try-error"))
		units::install_conversion_constant("m", "link", 0.201168)
	if (inherits(try(units::as_units("us_in"), silent = TRUE), "try-error"))
		units::install_conversion_constant("m", "us_in", 1./39.37)
	if (inherits(try(units::as_units("ind_yd"), silent = TRUE), "try-error"))
		units::install_conversion_constant("m", "ind_yd", 0.91439523)
	if (inherits(try(units::as_units("ind_ft"), silent = TRUE), "try-error"))
		units::install_conversion_constant("m", "ind_ft", 0.30479841)
	if (inherits(try(units::as_units("ind_ch"), silent = TRUE), "try-error"))
		units::install_conversion_constant("m", "ind_ch", 20.11669506)
}

unload_gdal <- function() {
	CPL_gdal_cleanup_all()
	if (file.exists(system.file("proj/nad.lst", package = "sf")[1])) {
		# nocov start
		if (! CPL_set_data_dir(system.file("proj", package = "sf")[1])) # set back:
			Sys.setenv("PROJ_LIB"=get(".sf.PROJ_LIB", envir=.sf_cache))

		Sys.setenv("GDAL_DATA"=get(".sf.GDAL_DATA", envir=.sf_cache))
		# nocov end
	}
	units::remove_symbolic_unit("link")
	units::remove_symbolic_unit("us_in")
	units::remove_symbolic_unit("ind_yd")
	units::remove_symbolic_unit("ind_ft")
	units::remove_symbolic_unit("ind_ch")
}
