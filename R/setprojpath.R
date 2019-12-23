#' Set the PROJ_LIB path to the 'proj' subfolder of `sf` installation
#' folder on windows () and the GDAL_DATA folder to the 'GDAL_DATA' folder
#' of `sf` and autoatically restore the previous (system) value upon
#' exiting from the caller function. This avoids the need of modifying
#' values of the two variables on load.
#' @name setprojpath
setprojpath <- function() {
	prj = system.file("proj", package = "sf")[1]
	curprojpath <- Sys.getenv("PROJ_LIB")
	if (! CPL_set_data_dir(prj)) {
		Sys.setenv("PROJ_LIB" = prj)
	}
	curgdalpath <- Sys.getenv("GDAL_DATA")
	Sys.setenv("GDAL_DATA" = system.file("gdal", package = "sf")[1])
	do.call(on.exit,
			list(substitute(Sys.setenv("PROJ_LIB"  = curprojpath)),
				 substitute(Sys.setenv("GDAL_DATA" = curgdalpath))),
			envir = parent.frame()
	)
}
