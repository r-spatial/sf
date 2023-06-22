agr_levels = c("constant", "aggregate", "identity")

#' @name st_agr
#' @details
#' \code{NA_agr_} is the \code{agr} object with a missing value.
#' @export
NA_agr_ = factor(NA, levels = agr_levels)

#' get or set relation_to_geometry attribute of an \code{sf} object
#'
#' get or set relation_to_geometry attribute of an \code{sf} object
#' @name st_agr
#' @param x object of class \code{sf}
#' @param ... ignored
#' @export
st_agr = function(x, ...) UseMethod("st_agr")

#' @export
st_agr.sf = function(x, ...) {
	nm = setdiff(names(x), attr(x, "sf_column"))
	ret = attr(x, "agr")[nm]
	if (is.null(names(ret)) || all(is.na(names(ret))))
		structure(rep(NA_agr_, length(nm)), names = nm)
	else
		ret
}

#' @export
st_agr.character = function(x, ...) {
	structure(factor(x, levels = agr_levels), names = names(x))
}

#' @export
st_agr.factor = function(x, ...) {
	stopifnot(all(levels(x) == agr_levels))
	x
}

#' @export
st_agr.default = function(x = NA_character_, ...) { 
	stopifnot(all(is.na(x)))
	structure(st_agr(as.character(x)), names = names(x))
}

#' @name st_agr
#' @param value character, or factor with appropriate levels; if named, names should correspond to the non-geometry list-column columns of \code{x}
#' @export
`st_agr<-` = function(x, value) UseMethod("st_agr<-")

#' @export
`st_agr<-.sf` = function(x, value) {
	stopifnot(is.character(value) || is.factor(value))
	nv = setdiff(names(x), attr(x, "sf_column"))
	if (length(value) == 0)
		attr(x, "agr") = setNames(NA_agr_[0], character())
	else if (! is.null(names(value)) && length(value) == 1) { 
		# as in: st_agr(x) = c(Group.1 = "identity"): replace one particular named
		if (!is.null(attr(x, "agr")))
			attr(x, "agr")[names(value)] = st_agr(value)
		else
			attr(x, "agr") = st_agr(value)
	} else {
		value = rep(st_agr(value), length.out = ncol(x) - 1)
		if (! is.null(names(value)))
			value = value[match(nv, names(value))]
		else
			names(value) = nv
		attr(x, "agr") <- value
	}
#	a = st_agr(x)
#	absent = setdiff(names(x), c(na.omit(names(a)), attr(x, "sf_column")))
#	if (length(absent)) { # repair:
#		a[absent] = NA_agr_
#		names(a[absent]) = absent
#		attr(x, "agr") = a[nv]
#	}
	x
}

#' @name st_agr
#' @export
st_set_agr = function(x, value) { 
	st_agr(x) = value
	x
}

summarize_agr = function(x) {
	su = summary(st_agr(x))
	su = su[su > 0]
	paste(paste0(names(su), " (", su, ")"), collapse = ", ")
}

all_constant = function(x) {
	x = attr(x, "agr")
	!anyNA(x) && all(x %in% c("identity", "constant"))
}
