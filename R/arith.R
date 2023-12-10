#' S3 Ops Group Generic Functions for simple feature geometries
#' @name Ops
#'
#' @param e1 object of class \code{sfg} or \code{sfc}
#' @param e2 numeric, or object of class \code{sfg}; in case \code{e1} is of class \code{sfc} also an object of class \code{sfc} is allowed
#'
#' @details in case \code{e2} is numeric, +, -, *, /, %% and %/% add, subtract, multiply, divide, modulo, or integer-divide by \code{e2}. In case \code{e2} is an n x n matrix, * matrix-multiplies and / multiplies by its inverse. If \code{e2} is an \code{sfg} object, |, /, & and %/% result in the geometric union, difference, intersection and symmetric difference respectively, and \code{==} and \code{!=} return geometric (in)equality, using \link{st_equals}.
#'
#' If \code{e1} is of class \code{sfc}, and \code{e2} is a length 2 numeric, then it is considered a two-dimensional point (and if needed repeated as such) only for operations \code{+} and \code{-}, in other cases the individual numbers are repeated; see commented examples.
#'
#' @details
#' It has been reported (https://github.com/r-spatial/sf/issues/2067) that
#' certain ATLAS versions result in invalid polygons, where the final point
#' in a ring is no longer equal to the first point. In that case, setting
#' the precisions with \link{st_set_precision} may help.
#' @return object of class \code{sfg}
#' @export
#'
#' @examples
#' st_point(c(1,2,3)) + 4
#' st_point(c(1,2,3)) * 3 + 4
#' m = matrix(0, 2, 2)
#' diag(m) = c(1, 3)
#' # affine:
#' st_point(c(1,2)) * m + c(2,5)
#' # world in 0-360 range:
#' if (require(maps, quietly = TRUE)) {
#'  w = st_as_sf(map('world', plot = FALSE, fill = TRUE))
#'  w2 = (st_geometry(w) + c(360,90)) %% c(360) - c(0,90)
#'  w3 = st_wrap_dateline(st_set_crs(w2 - c(180,0), 4326)) + c(180,0)
#'  plot(st_set_crs(w3, 4326), axes = TRUE)
#' }
#' (mp <- st_point(c(1,2)) + st_point(c(3,4))) # MULTIPOINT (1 2, 3 4)
#' mp - st_point(c(3,4)) # POINT (1 2)
#' opar = par(mfrow = c(2,2), mar = c(0, 0, 1, 0))
#' a = st_buffer(st_point(c(0,0)), 2)
#' b = a + c(2, 0)
#' p = function(m) { plot(c(a,b)); plot(eval(parse(text=m)), col=grey(.9), add = TRUE); title(m) }
#' o = lapply(c('a | b', 'a / b', 'a & b', 'a %/% b'), p)
#' par(opar)
#' sfc = st_sfc(st_point(0:1), st_point(2:3))
#' sfc + c(2,3) # added to EACH geometry
#' sfc * c(2,3) # first geometry multiplied by 2, second by 3
Ops.sfg <- function(e1, e2) {

	if (nargs() == 1) {
		switch (.Generic,
			"-" = return(e1 * -1.0),
			"+" = return(e1),
			stop(paste("unary", .Generic, 'not defined for "sfg" objects'))
		)
	}

	prd <- switch(.Generic, "*" = TRUE, "/" = TRUE, FALSE)
	pm  <- switch(.Generic, "+" = , "-" = TRUE, FALSE)
	mod <- switch(.Generic, "%%" = TRUE, "%/%" = TRUE, FALSE)
	set <- switch(.Generic, "&" = TRUE, "|" = TRUE, FALSE)
	lgcl <- switch(.Generic, "==" = TRUE, "!=" = TRUE, FALSE)

	if (!(prd || pm || mod || set || lgcl))
		stop(paste("operation", .Generic, "not supported for sfg objects"))

	e1_empty = st_is_empty(e1)
	e2_empty = inherits(e2, "sfg") && st_is_empty(e2)
	if (lgcl && (e1_empty || e2_empty))
		return(NA)

	if (e1_empty && (.Generic %in% c("*", "+", "-", "%%")))
		return(e1)

	if (inherits(e2, "sfg")) {
		e2 = switch(.Generic,
			"|" = st_union(e1, e2),
			"/" = st_difference(e1, e2),
			"&" = st_intersection(e1, e2),
			"%/%" = st_sym_difference(e1, e2),
			"==" = length(st_equals(e1, e2)[[1]]) != 0,
			"!=" = length(st_equals(e1, e2)[[1]]) == 0,
			unclass(e2))
		if (inherits(e2, "sfg") || is.logical(e2))
			return(e2)
	}

	dims = nchar(class(e1)[1])
	Vec = rep(0, dims)
	Mat = matrix(0, dims, dims)
	diag(Mat) = 1
	if (pm || mod) {
		if (length(e2) == 1)
			Vec = rep(e2, length.out = dims)
		else
			Vec = e2
		if (.Generic == "-")
			Vec = -Vec
	} else if (prd) {
		if (length(e2) == 1 || length(e2) == dims)
			diag(Mat) = e2
		else
			Mat = e2
		if (.Generic == "/")
			Mat = solve(Mat) # inverse
	}

	if_pt = function(x, y) { if(inherits(x, "POINT")) as.vector(y) else y }
	fn = if (prd)
			function(x, Mat, Vec) structure(if_pt(x, x %*% Mat), class = class(x))
		else if (pm)
			function(x, Mat, Vec) structure(if_pt(x, unclass(x) + conform(Vec, x)), class = class(x))
		else # mod:
			function(x, Mat, Vec) structure(if_pt(x, unclass(x) %% conform(Vec, x)), class = class(x))

	if (is.list(e1))
		rapply(e1, fn, how = "replace", Mat = Mat, Vec = Vec)
	else
		fn(e1, Mat, Vec)
}

conform = function(vec, m) {
	if (is.matrix(m))
		t(matrix(vec, ncol(m), nrow(m)))
	else
		vec
}

#' @export
#' @name Ops
#' @examples
#' nc = st_transform(st_read(system.file("gpkg/nc.gpkg", package="sf")), 32119) # nc state plane, m
#' b = st_buffer(st_centroid(st_union(nc)), units::set_units(50, km)) # shoot a hole in nc:
#' plot(st_geometry(nc) / b, col = grey(.9))
Ops.sfc <- function(e1, e2) {

	if (length(e1) == 0) # empty set
		return(e1)

	if (is.numeric(e2) && !is.matrix(e2) && length(e2) <= 2 && .Generic %in% c("+", "-")) {
		if (.Generic == "-")
			e2 <- -e2
		return(opp_sfc(e1, as.numeric(e2), 0L, NA_crs_))
	} else if (.Generic %in% c("*", "/") && is.numeric(e2) && (length(e2) == 1 || is_only_diag(e2))) {
		if (is.matrix(e2)) e2 <- diag(e2)
		if (.Generic == "/")
			e2 <- 1 / e2
		return(opp_sfc(e1, as.numeric(e2), 1L, NA_crs_))
	}
	if ((is.matrix(e2) && ncol(e2) == 2) || (is.numeric(e2) && length(e2) == 2))
		e1 = st_zm(e1) # drop z and/or m

	if (!is.list(e2) && ((.Generic %in% c("+", "-") && length(e2) == 2) || is.matrix(e2)))
		e2 = list(e2)

	ret = switch(
		.Generic,
		"&"   =  mapply(function(x, y) { x  & y }, e1, e2, SIMPLIFY = FALSE),
		"|"   =  mapply(function(x, y) { x  | y }, e1, e2, SIMPLIFY = FALSE),
		"%/%" =  mapply(function(x, y) { x %/% y}, e1, e2, SIMPLIFY = FALSE),
		"/"   =  mapply(function(x, y) { x  / y }, e1, e2, SIMPLIFY = FALSE),
		"!="  =  mapply(function(x, y) { x != y }, e1, e2, SIMPLIFY = TRUE),
		"=="  =  mapply(function(x, y) { x == y }, e1, e2, SIMPLIFY = TRUE),
		"*"   =  mapply(function(x, y) { x  * y }, e1, e2, SIMPLIFY = FALSE),
		"+"   =  mapply(function(x, y) { x  + y }, e1, e2, SIMPLIFY = FALSE),
		"-"   =  mapply(function(x, y) { x  - y }, e1, e2, SIMPLIFY = FALSE),
		"%%"  =  mapply(function(x, y) { x %% y }, e1, e2, SIMPLIFY = FALSE),
		stop(paste("operation", .Generic, "not supported")))

	if (!(.Generic %in% c("!=", "=="))) {
		crs = if (.Generic %in% c("&", "|", "%/%", "/") && inherits(e2, c("sfc", "sfg"))) # retain:
			st_crs(e1)
		else # geometry got displaced:
			NA_crs_
		st_sfc(ret, crs = crs, precision = attr(e1, "precision"))
	} else
		ret
}
is_only_diag <- function(x) {
	is.matrix(x) && all(`diag<-`(x, 0) == 0) # nocov
}
