#' Exctract only elements of specified type from a GEOMETRYCOLLECTION
#'
#' @param x an \code{sf(c/g)} object that has mixed geometry
#' @param type One of "POLYGON", "POINT", "LINESTRING"
#'
#' @return a (multi)geometry consisting only of elements of the specified type
#' @export
#'
#' @examples
st_collectionextract = function(x, type = c("POLYGON", "POINT", "LINESTRING")) {
	UseMethod("st_collectionextract")
}

#' @name st_collectionextract
#'
#' @export
st_collectionextract.sfg = function(x, type = c("POLYGON", "POINT", "LINESTRING")) {
	type = match.arg(type)
	types = c(type, paste0("MULTI", type))

	if (is(x, types)) {
		warning("x is already of type ", type, ".")
		return(x)
	}
	matches = vapply(x, st_is, types, FUN.VALUE = logical(1))
	x_types = x[which(matches)]
	if (length(x_types) == 1) {
		return(x_types[[1]])
	} else {
		return(st_sfc(x_types))
	}
}

#' @name st_collectionextract
#'
#' @export
st_collectionextract.sfc = function(x, type = c("POLYGON", "POINT", "LINESTRING")) {
	type = match.arg(type)
	types = c(type, paste0("MULTI", type))

	if (is(st_geometry(x), paste0("sfc_", types))) {
		warning("x is already of type ", type, ".")
		return(x)
	}

	# Cast to GEOMETRYCOLLECTION if not already (e.g., if it is sfc_GEOMETRY)
	if (!is(st_geometry(x), "sfc_GEOMETRYCOLLECTION")) {
		x = st_cast(x, "GEOMETRYCOLLECTION")
	}

	## Cast GEOMETRYCOLLECTION into all components
	gc_casted = st_cast(x)

	## Keep only components that match input type
	if (is(gc_casted, "sf")) {
		gc_types = gc_casted[st_is(gc_casted, types), ]
	} else {
		gc_types = gc_casted[st_is(gc_casted, types)]
	}

	## Cast to specified (MULTI) type
	st_cast(gc_types)
}

#' @name st_collectionextract
#'
#' @export
st_collectionextract.sf = st_collectionextract.sfc
