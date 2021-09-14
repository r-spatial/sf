# see https://github.com/r-spatial/sf/issues/1567
#' @export
st_as_sf.SpatVector = function(x, ..., hex = TRUE) {
	if(!requireNamespace("terra", quietly = TRUE))
		stop("package terra required, please install it first")
	if (!utils::packageVersion("terra") >= "1.1-5")
		stop("package terra version 1.1-5 required")
	d <- terra::as.data.frame(x, geom = "hex")
	d$geometry <- structure(as.list(d$geometry), class = "WKB")
	st_as_sf(d, crs = st_crs(x))
}

#' @export
st_crs.SpatRaster = function(x, ...) {
	if (!requireNamespace("terra", quietly = TRUE))
		stop("package terra required, please install it first") # nocov
	string = terra::crs(x)
	if (string == "") 
		NA_crs_
	else
		st_crs(string)
}

#' @export
st_crs.SpatVector = st_crs.SpatRaster

#' @export
st_bbox.SpatExtent = function(obj, ..., crs = NA_crs_) {
	if (!requireNamespace("terra", quietly = TRUE))
		stop("package terra required, please install it first") # nocov
	bb = as.vector(obj)[c(1,3,2,4)]
	names(bb) = c("xmin", "ymin", "xmax", "ymax")
	st_bbox(bb, crs = crs)
}

#' @export
st_bbox.SpatRaster = function(obj, ...) {
	if (!requireNamespace("terra", quietly = TRUE))
		stop("package terra required, please install it first") # nocov
	st_bbox(terra::ext(obj), crs = st_crs(obj))
}

#' @export
st_bbox.SpatVector = function(obj, ...) {
	if (!requireNamespace("terra", quietly = TRUE))
		stop("package terra required, please install it first") # nocov
    bb = as.vector(terra::ext(obj))[c(1,3,2,4)]
    names(bb) = c("xmin", "ymin", "xmax", "ymax")
    st_bbox(bb, crs = st_crs(obj))
}
