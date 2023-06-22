
#' Break antimeridian for plotting not centred on Greenwich
#'
#' Longitudes can be broken at the antimeridian of a target central longitude 
#' to permit plotting of (usually world) line or polygon objects centred 
#' on the chosen central longitude. The method may only be used with 
#' non-projected, geographical coordinates and linestring or polygon objects.
#' s2 is turned off internally to permit the use of a rectangular bounding 
#' box. If the input geometries go outside [-180, 180] degrees longitude,
#' the protruding geometries will also be split using the same \code{tol=} 
#' values; in this case empty geometries will be dropped first.
#'
#' @param x object of class sf or sfc
#' @param lon_0 target central longitude (degrees)
#' @param tol half of break width (degrees, default 0.0001)
#' @param ... ingnored here
#' @export
#' @name st_break_antimeridian
#' @examples
#' \donttest{
#' if (require("maps", quietly=TRUE)) {
#'  opar = par(mfrow=c(3, 2))
#'  wld = st_as_sf(map(fill=FALSE, interior=FALSE, plot=FALSE), fill=FALSE)
#'  for (lon_0 in c(-170, -90, -10, 10, 90, 170)) {
#'   wld |> st_break_antimeridian(lon_0=lon_0) |>
#'    st_transform(paste0("+proj=natearth +lon_0=", lon_0)) |>
#'    st_geometry() |> plot(main=lon_0)
#'  }
#'  par(opar)
#' }
#' }
st_break_antimeridian = function(x, lon_0=0, tol=0.0001, ...) {
	ll = st_is_longlat(x)
	if (!ll | is.na(ll))
		stop("'st_break_antimeridian' requires non-projected geographic coordinates",
			 call. = FALSE)

	UseMethod("st_break_antimeridian")
}

#' @export
#' @name st_break_antimeridian
st_break_antimeridian.sf = function(x, lon_0=0, tol=0.0001, ...) {
	type = st_geometry_type(x)
        if (all(grepl("CURVE", grep("LINESTRING|POLYGON", type, value=TRUE),
            fixed=TRUE))) stop("'st_break_antimeridian' requires linestring or polygon objects", call. = FALSE)
	bb0 = st_bbox(x)
        low = bb0[1] < -(180+tol)
	high = bb0[3] > (180+tol)
        if (low || high) {
	    warning("st_break_antimeridian: longitude coordinates outside [-180, 180]")
#	     x_c = st_geometry(x)
#            g = ((x_c + c(360, 90)) %% c(360) - c(0, 90))  - c(180, 0)
#	     st_crs(g) <- st_crs(x)
#            st_geometry(x) = g
            x <- st_within_pm180(x, tol=tol)
            bb0 = st_bbox(x)
        }
        if (lon_0 < 0) {
	    am = lon_0 + 180
            am_w = am - tol
	    am_e = am + tol
            if (am_w > 180) am_w = am_w - 360
        } else {
	    am = lon_0 - 180
            am_w = am - tol
	    am_e = am + tol
            if (am_w < -180) am_w = am_w + 360
	}
        if (lon_0 == 0) {
	    bb1 = bb0
            bb1[1] = am_e
	    bb1[3] = am_w
            bb1 = st_as_sfc(bb1)
        } else {
            bb1w = bb1e = bb0
	    bb1w[3] = am_w # antimeridian of target minus fuzz
	    bb1e[1] = am_e # plus fuzz
	    bb1 = c(st_as_sfc(bb1w), st_as_sfc(bb1e))
	}
        s2_status = sf_use_s2()
	sf_use_s2(FALSE) # avoid s2 because we need a planar bounding box
	res = st_intersection(x, bb1)
	sf_use_s2(s2_status)
	st_crs(res) = st_crs(x)
        res
}
#' @export
#' @name st_break_antimeridian
st_break_antimeridian.sfc = function(x, lon_0=0, tol=0.0001, ...) {
# cannot reduce sf to sfc because the length of the returned object is
# not restricted to the row count of the input object
        st_geometry(st_break_antimeridian(st_as_sf(x), lon_0=lon_0, tol=tol))
}

st_within_pm180 <- function(x, tol=0.0001) {
	stopifnot(inherits(x, "sf"))
        xempt = st_is_empty(x)
	if (any(xempt)) x = x[!xempt,]
        xcrs = st_crs(x)
        xnames = names(x)
	xnames = xnames[grep(attr(x, "sf_column"), xnames, invert=TRUE)]
	x$st_within_pm180_ID = as.character(1:nrow(x))
        s2_status = sf_use_s2()
	sf_use_s2(FALSE) # avoid s2 because we need a planar bounding box
	bb0 = st_bbox(x)
        low = bb0[1] < -(180+tol)
	high = bb0[3] > (180+tol)
        bb1w = bb1e = bb0
	if (low) {
	    am = -180
            am_w = am - tol
	    am_e = am + tol
            if (am_w > 180) am_w = am_w - 360
	    bb1w[3] = am_w # 180 minus fuzz
	    bb1e[1] = am_e # 180 plus fuzz
            xw = st_intersection(x, st_as_sfc(bb1w))
	    xe = st_intersection(x, st_as_sfc(bb1e))
            gw = st_geometry(xw)
            gw = gw + c(360, 0)
	    st_crs(gw) <- xcrs
	    st_geometry(xw) <- gw
	    o = rbind(xw, xe)
	    x = aggregate(o, list(o$st_within_pm180_ID), head, 1)
	    x = x[, xnames]
	    st_crs(x) = xcrs
	}
	bb0 = st_bbox(x)
        bb1w = bb1e = bb0
	if (high) {
	    am = 180
            am_w = am - tol
	    am_e = am + tol
            if (am_w < -180) am_w = am_w + 360
	    bb1w[3] = am_w # 180 minus fuzz
	    bb1e[1] = am_e # 180 plus fuzz
            xw = st_intersection(x, st_as_sfc(bb1w))
	    xe = st_intersection(x, st_as_sfc(bb1e))
            ge = st_geometry(xe)
            ge = ge - c(360, 0)
	    st_crs(ge) <- xcrs
	    st_geometry(xe) <- ge
	    o = rbind(xw, xe)
	    x = aggregate(o, list(o$st_within_pm180_ID), head, 1)
	    x = x[, xnames]
	    st_crs(x) = xcrs
	}
	sf_use_s2(s2_status)
	st_crs(x) = xcrs
        x

}
