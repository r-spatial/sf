#' Given an object with geometries of type \code{GEOMETRY} or \code{GEOMETRYCOLLECTION},
#' return an object consisting only of elements of the specified type.
#'
#' Similar to ST_CollectionExtract in PostGIS. If there are no sub-geometries
#' of the specified type, an empty geometry is returned.
#'
#' @param x an object of class \code{sf}, \code{sfc} or \code{sfg} that has
#' mixed geometry (\code{GEOMETRY} or \code{GEOMETRYCOLLECTION}).
#' @param type character; one of "POLYGON", "POINT", "LINESTRING"
#' @param warn logical; if \code{TRUE}, warn if attributes are assigned to
#' sub-geometries when casting (see \code{\link{st_cast}})
#'
#' @return An object having the same class as \code{x}, with geometries
#' consisting only of elements of the specified type.
#' For \code{sfg} objects, an \code{sfg} object is returned if there is only
#' one geometry of the specified type, otherwise the geometries are combined
#' into an \code{sfc} object of the relevant type. If any subgeometries in the
#' input are MULTI, then all of the subgeometries in the output will be MULTI.
#'
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
#' st_collection_extract(i, "POLYGON")
#' st_collection_extract(i, "POINT")
#' st_collection_extract(i, "LINESTRING")
#'
#' ## A GEOMETRYCOLLECTION
#' aa <- rbind(st_sf(a=1, geom = st_sfc(i)),
#' 			st_sf(a=2, geom = st_sfc(j)))
#'
#' ## With sf objects
#' st_collection_extract(aa, "POLYGON")
#' st_collection_extract(aa, "LINESTRING")
#' st_collection_extract(aa, "POINT")
#'
#' ## With sfc objects
#' st_collection_extract(st_geometry(aa), "POLYGON")
#' st_collection_extract(st_geometry(aa), "LINESTRING")
#' st_collection_extract(st_geometry(aa), "POINT")
#'
#' ## A GEOMETRY of single types
#' bb <- rbind(
#' 	st_sf(a = 1, geom = st_sfc(pt)),
#' 	st_sf(a = 2, geom = st_sfc(ls)),
#' 	st_sf(a = 3, geom = st_sfc(poly1)),
#' 	st_sf(a = 4, geom = st_sfc(multipoly))
#' )
#'
#' st_collection_extract(bb, "POLYGON")
#'
#' ## A GEOMETRY of mixed single types and GEOMETRYCOLLECTIONS
#' cc <- rbind(aa, bb)
#'
#' st_collection_extract(cc, "POLYGON")
#'
st_collection_extract = function(x, type = c("POLYGON", "POINT", "LINESTRING"), warn = FALSE) {
	UseMethod("st_collection_extract")
}

#' @name st_collection_extract
#'
#' @export
st_collection_extract.sfg = function(x, type = c("POLYGON", "POINT", "LINESTRING"), warn = FALSE) {
	type = match.arg(type)
	types = c(type, paste0("MULTI", type))

	if (inherits(x, types)) {
		warning("x is already of type ", type, ".")
		return(x)
	}

	if (!inherits(x, "GEOMETRYCOLLECTION")) {
		stop("x is of singular geometry type that is different to supplied type: ", type) # nocov
	}

	# Find the geometries of the specified type and extract into a list
	matches = vapply(x, st_is, types, FUN.VALUE = logical(1))
	x_types = x[which(matches)]
	if (length(x_types) == 0L) {
		## return an empty sfg of the specified type
		warning("x contains no geometries of specified type")
		return(typed_empty(paste0("sfc_", type)))
	} else if (length(x_types) == 1L) {
		# Get the contents of the first (only) list element which is an sfg
		return(x_types[[1]])
	} else {
		# turn list into an sfc, and cast it to single type. Will be multi
		# if any are multi
		return(st_cast(st_sfc(x_types), warn = warn))
	}
}

#' @name st_collection_extract
#'
#' @export
st_collection_extract.sfc = function(x, type = c("POLYGON", "POINT", "LINESTRING"), warn = FALSE) {
	type = match.arg(type)
	types = c(type, paste0("MULTI", type))

	if (length(x) == 0)
		return(x)

	# Check it's not already what user is asking for
	if (inherits(st_geometry(x), paste0("sfc_", types))) {
		warning("x is already of type ", type, ".") # nocov
		return(x)                                   # nocov
	}

	if (!inherits(st_geometry(x), c("sfc_GEOMETRY", "sfc_GEOMETRYCOLLECTION"))) {
		stop("x is of singular geometry type that is different to supplied type: ", type)
	}

	# Cast to GEOMETRYCOLLECTION if is GEOMETRY)
	if (inherits(st_geometry(x), "sfc_GEOMETRY")) {
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

	if (length(st_geometry(gc_types)) == 0L) {
		warning("x contains no geometries of specified type")
		return(gc_types)
	}

	st_cast(gc_types, warn = warn)
}

#' @name st_collection_extract
#'
#' @export
st_collection_extract.sf = st_collection_extract.sfc
