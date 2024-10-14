#' @importFrom utils head tail object.size str packageVersion compareVersion globalVariables assignInMyNamespace
#' @importFrom stats aggregate dist na.omit rbinom runif setNames
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom methods as new slot slotNames slot<-
#' @importFrom grid convertHeight convertUnit convertWidth current.viewport linesGrob nullGrob pathGrob pointsGrob polylineGrob unit viewport
#' @import graphics
#' @importFrom grDevices dev.size rgb cm
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery dbReadTable dbSendQuery dbWriteTable
#' @importFrom units as_units drop_units make_unit_label set_units
#' @importFrom classInt classIntervals
#' @useDynLib sf, .registration=TRUE
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
setOldClass(c("sfc_GEOMETRYCOLLECTION", "sfc"))
setOldClass("sfg")
setOldClass("crs")
setOldClass("bbox")

.sf_cache <- new.env(FALSE, parent=globalenv())

pathGrob <- NULL
.onLoad = function(libname, pkgname) {
	if (getRversion() < as.numeric_version("3.6")) { # nocov start
		pathGrob <<- function(..., pathId.lengths) {
			grid::pathGrob(...)
		}
	} # nocov end
	load_gdal() 
	if ((s2 <- Sys.getenv("_SF_USE_S2")) != "")
		options(sf_use_s2 = s2 != "false")
}

.onUnload = function(libname, pkgname) {
	unload_gdal() # nocov
}

.onAttach = function(libname, pkgname) {
	m = paste0("Linking to GEOS ", strsplit(CPL_geos_version(TRUE), "-")[[1]][1],
		", GDAL ", CPL_gdal_version(), ", PROJ ", CPL_proj_version(),
		"; sf_use_s2() is ", sf_use_s2())
	m = strwrap(m, width = getOption("width"))
	packageStartupMessage(paste0(m, collapse = "\n"))
	if (length(grep(CPL_geos_version(FALSE, TRUE), CPL_geos_version(TRUE))) != 1) { # nocov start
		packageStartupMessage("WARNING: different compile-time and runtime versions for GEOS found:")
		packageStartupMessage(paste(
			"Linked against:", CPL_geos_version(TRUE, TRUE),
			"compiled against:", CPL_geos_version(FALSE, TRUE)))
		packageStartupMessage("It is probably a good idea to reinstall sf (and maybe lwgeom too)")
	} # nocov end
}

#' Provide the external dependencies versions of the libraries linked to sf
#'
#' Provide the external dependencies versions of the libraries linked to sf
#' @export
sf_extSoftVersion = function() {
	structure(c(CPL_geos_version(), CPL_gdal_version(), CPL_proj_version(), 
		ifelse(CPL_gdal_with_geos(), "true", "false"),
		ifelse(CPL_proj_h(), "true", "false"), CPL_proj_version()),
		names = c("GEOS", "GDAL", "proj.4", "GDAL_with_GEOS", "USE_PROJ_H", "PROJ"))
}

save_and_replace = function(var, value, where) {
	if (Sys.getenv(var) != "")
		assign(paste0(".sf.", var), Sys.getenv(var), envir = where)
	# Sys.setenv(var = value) uses NSE and will set var, not the variable var points to:
	do.call(Sys.setenv, setNames(list(value), var))
}

if_exists_restore = function(vars, where) {
	fn = function(var, where) {
		lname = paste0(".sf.", var)
		if (!is.null(get0(lname, envir = where)))
			do.call(Sys.setenv, setNames(list(get(lname, envir = where)), var)) # see above
	}
	lapply(vars, fn, where = where)
}

load_gdal <- function() {
	if (!identical(Sys.getenv("R_SF_USE_PROJ_DATA"), "true")) {
		if (file.exists(prj <- system.file("proj", package = "sf")[1])) {
			# nocov start
			if (! sf_proj_search_paths(prj)) { # if TRUE, uses C API to set path, leaving PROJ_LIB / PROJ_DATA alone
				save_and_replace("PROJ_LIB", prj, .sf_cache)
				save_and_replace("PROJ_DATA", prj, .sf_cache)
			}
			# CPL_use_proj4_init_rules(1L)
			# nocov end
		}
		if (file.exists(gdl <- system.file("gdal", package = "sf")[1]))
			save_and_replace("GDAL_DATA", gdl, .sf_cache)
	}
	CPL_gdal_init()
	register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse)
}

unload_gdal <- function() {
	CPL_gdal_cleanup_all()
	if_exists_restore(c("PROJ_LIB", "PROJ_DATA", "GDAL_DATA"), .sf_cache)
}
 

#' @export
#' @name sf_project
#' @details \code{sf_add_proj_units} loads the PROJ units `link`, `us_in`, `ind_yd`, `ind_ft`, and `ind_ch` into the udunits database, and returns \code{TRUE} invisibly on success.
#' @examples
#' sf_add_proj_units()
sf_add_proj_units = function() {
	#nocov start
	units::install_unit("link", "0.201168 m")
	units::install_unit("us_in", "1./39.37 m")
	units::install_unit("ind_yd", "0.91439523 m")
	units::install_unit("ind_ft", "0.30479841 m")
	units::install_unit("ind_ch", "20.11669506 m")
	invisible(TRUE)
	#nocov end
}
