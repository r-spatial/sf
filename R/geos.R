#' Compute distance between pairs of simple feature geometries
#' 
#' Compute distance between pairs of simple feature geometries
#' @param sf1 first simple feature (sf) or simple feature geometry (sfc) collection
#' @param sf2 second simple feature (sf) or simple feature geometry (sfc) collection
#' @name geos
#' @returns numeric matrix of dimension length(sf1) x length(sf2)
#' @export
st_distance = function(sf1, sf2 = sf1) {
	if (inherits(sf1, "sf"))
		sf1 = st_geometry(sf1)
	if (inherits(sf2, "sf"))
		sf2 = st_geometry(sf2)
	stopifnot(inherits(sf1, "sfc")) 
	stopifnot(inherits(sf1, "sfc")) 
	CPL_geos_dist(sf1, sf2)
}

#' @name geos
#' @param sf simple feautures (sf) or simple feature geometries (sfc) object
#' @returns logical vector
#' @export
st_is_valid = function(sf) {
	if (inherits(sf, "sf"))
		sf = st_geometry(sf)
	stopifnot(inherits(sf, "sfc")) 
	CPL_geos_is_valid(sf)
}

#' @name geos
#' @export
#' @param op character; one of "intersects", "disjoint", "touches", "crosses", "within", "contains", "overlaps", "equals", "covers", "coveredBy", "equalsExact", or "isWithinDistance", "relate", "distance"
#' @param sparse logical; should a sparse matrix be returned (TRUE) or a dense matrix?
#' @param par numeric; parameter used for "equalsExact" (margin) and "isWithinDistance"
#' @returns matrix (sparse or dense) of type \code{character} for op \code{relate}, \code{numeric} for \code{distance}, and \code{logical} for all others; matrix has dimension \code{sf1} x \code{sf2}.
st_geos_binop = function(op = "intersects", sf1, sf2 = sf1, par = 0.0, sparse = TRUE) {
	if (inherits(sf1, "sf"))
		sf1 = st_geometry(sf1)
	if (inherits(sf2, "sf"))
		sf2 = st_geometry(sf2)
	stopifnot(inherits(sf1, "sfc")) 
	stopifnot(inherits(sf1, "sfc")) 
	CPL_geos_binop(sf1, sf2, op, par, sparse)
}
