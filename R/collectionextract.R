#' Given a (multi)geometry, return a (multi)geometry consisting only of elements of the specified type.
#'
#' Similar to ST_CollectionExtract in postgis. If there are no sub-geometries of the specified type, an empty geometry is returned.
#'
#' @param x an \code{sf(c/g)} object that has mixed geometry
#' @param type One of "POLYGON", "POINT", "LINESTRING"
#' @param warn logical; if \code{TRUE}, warn if attributes are assigned to sub-geometries when casting (see \code{\link{st_cast}})
#'
#' @return An sf(c/g) object with (multi)geometries consisting only of elements of the specified type. For sfg objects, an sfg object is returned if there is only one geometry of the specified type, otherwise the geometries are combined into an sfc object of the relevant type. If any subgeometries in the input are MULTI, then all of the subgeometries in the output will be MULTI.
#' @export
#'
#' @examples
#' pt <- st_point(c(1, 0))
#' ls <- st_linestring(matrix(c(4, 3, 0, 0), ncol = 2))
#' poly1 <- st_polygon(list(matrix(c(5.5, 7, 7, 6, 5.5, 0, 0, -0.5, -0.5, 0), ncol = 2)))
#' poly2 <- st_polygon(list(matrix(c(6.6, 8, 8, 7, 6.6, 1, 1, 1.5, 1.5, 1), ncol = 2)))
#' multipoly <- st_multipolygon(list(poly1, poly2))
#'
#' i <- st_geometrycollection(list(pt, ls, poly1, poly2))
#' j <- st_geometrycollection(list(pt, ls, poly1, poly2, multipoly))
#'
#' st_collectionextract(i, "POLYGON")
#' st_collectionextract(i, "POINT")
#' st_collectionextract(i, "LINESTRING")
#'
#' ## A GEOMETRYCOLLECTION
#' aa <- rbind(st_sf(a=1, geom = st_sfc(i)),
#' 			st_sf(a=2, geom = st_sfc(j)))
#'
#' ## With sf objects
#' st_collectionextract(aa, "POLYGON")
#' st_collectionextract(aa, "LINESTRING")
#' st_collectionextract(aa, "POINT")
#'
#' ## With sfc objects
#' st_collectionextract(st_geometry(aa), "POLYGON")
#' st_collectionextract(st_geometry(aa), "LINESTRING")
#' st_collectionextract(st_geometry(aa), "POINT")
#'
#' ## A GEOMETRY of single types
#' bb <- rbind(
#' 	st_sf(a = 1, geom = st_sfc(pt)),
#' 	st_sf(a = 2, geom = st_sfc(ls)),
#' 	st_sf(a = 3, geom = st_sfc(poly1)),
#' 	st_sf(a = 4, geom = st_sfc(multipoly))
#' )
#'
#' st_collectionextract(bb, "POLYGON")
#'
#' ## A GEOMETRY of mixed single types and GEOMETRYCOLLECTIONS
#' cc <- rbind(aa, bb)
#'
#' st_collectionextract(cc, "POLYGON")
#'
st_collectionextract = function(x, type = c("POLYGON", "POINT", "LINESTRING"), warn = FALSE) {
	UseMethod("st_collectionextract")
}

#' @name st_collectionextract
#'
#' @export
st_collectionextract.sfg = function(x, type = c("POLYGON", "POINT", "LINESTRING"), warn = FALSE) {
	type = match.arg(type)
	types = c(type, paste0("MULTI", type))

	if (inherits(x, types)) {
		warning("x is already of type ", type, ".")
		return(x)
	}
	# Find the geometries of the specified type and extract into a list
	matches = vapply(x, st_is, types, FUN.VALUE = logical(1))
	x_types = x[which(matches)]
	if (length(x_types) == 0L) {
		## Not sure if this should be a warning and return an empty sfg (current)
		## or error?
		warning("x contains no geometries of specified type")
		return(x_types)
	} else if (length(x_types) == 1L) {
		# Get the contents of the first (only) list element which is an sfg
		return(x_types[[1]])
	} else {
		# turn list into an sfc, and cast it to single type. Will be multi
		# if any are multi
		return(st_cast(st_sfc(x_types), warn = warn))
	}
}

#' @name st_collectionextract
#'
#' @export
st_collectionextract.sfc = function(x, type = c("POLYGON", "POINT", "LINESTRING"), warn = FALSE) {
	type = match.arg(type)
	types = c(type, paste0("MULTI", type))

	if (inherits(st_geometry(x), paste0("sfc_", types))) {
		warning("x is already of type ", type, ".")
		return(x)
	}

	# Cast to GEOMETRYCOLLECTION if not already (e.g., if it is sfc_GEOMETRY)
	if (!inherits(st_geometry(x), "sfc_GEOMETRYCOLLECTION")) {
		x = st_cast(x, "GEOMETRYCOLLECTION")
	}

	## Cast GEOMETRYCOLLECTION into all components
	gc_casted = st_cast(x, warn = warn)

	## Keep only components that match input type
	if (inherits(gc_casted, "sf")) {
		gc_types = gc_casted[st_is(gc_casted, types), ]
	} else {
		gc_types = gc_casted[st_is(gc_casted, types)]
	}

	## Cast to specified (MULTI) type
	st_cast(gc_types, warn = warn)
}

#' @name st_collectionextract
#'
#' @export
st_collectionextract.sf = st_collectionextract.sfc
