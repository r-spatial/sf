#' S3 Ops Group Generic Functions (multiply and add/subtract) for affine transformation
#'
#' Ops functions for simple feature geometry objects (constrained to multiplication and addition)
#' @name ops
#'
#' @param e1 object of class \code{sfg}
#' @param e2 numeric of object of class \code{sfg}; in case of multiplication an n x n matrix, in case of addition or subtraction a vector of length n, with n the number of dimensions of the geometry; if \code{e2} is an \code{sfg} object, \code{+}, \code{-}, \code{*} and \code{/} will result in the geometric union, difference, intersection and symmetric difference respectively; for other operators will lead to \code{e2} being converted to a numeric.
#'
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
#' library(maps)
#' w = st_as_sf(map('world', plot = FALSE, fill = TRUE))
#' w2 = (st_geometry(w) + c(360,90)) %% c(360) - c(0,90)
#' w3 = st_wrap_dateline(st_set_crs(w2 - c(180,0), 4326)) + c(180,0)
#' plot(st_set_crs(w3, 4326), axes = TRUE)
#' (mp <- st_point(c(1,2)) + st_point(c(3,4))) # MULTIPOINT (1 2, 3 4)
#' mp - st_point(c(3,4)) # POINT (1 2)
#' opar = par(mfrow = c(2,2), mar = c(0, 0, 1, 0))
#' a = st_buffer(st_point(c(0,0)), 2)
#' b = a + c(2, 0) 
#' p = function(m) { plot(c(a,b)); plot(eval(parse(text=m)), col=grey(.9), add = TRUE); title(m) }
#' lapply(c('a + b', 'a - b', 'a * b', 'a / b'), p)
#' par(opar)
Ops.sfg <- function(e1, e2) {
	if (nargs() == 1)
		stop(paste("unary", .Generic, "not defined for \"sfg\" objects"))

	prd <- switch(.Generic, "*" = TRUE, "/" = TRUE, FALSE)
	pm  <- switch(.Generic, "+" = , "-" = TRUE, FALSE)
	mod <- switch(.Generic, "%%" = TRUE, FALSE)

	if (!(prd || pm || mod))
		stop(paste("operation", .Generic, "not supported for sfg objects"))
	
	if (st_is_empty(e1))
		return(e1)

	if (inherits(e2, "sfg")) {
		ret = switch(.Generic,
			"+" = st_union(e1, e2),
			"-" = st_difference(e1, e2),
			"*" = st_intersection(e1, e2),
			"/" = st_sym_difference(e1, e2),
			unclass(e2))
		if (inherits(ret, "sfg"))
			return(ret)
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
#' @name ops
#' @examples
#' nc = st_transform(st_read(system.file("gpkg/nc.gpkg", package="sf")), 32119) # nc state plane, m
#' b = st_buffer(st_centroid(st_union(nc)), units::set_units(50, km)) # shoot a hole in nc:
#' plot(st_geometry(nc) - b, col = grey(.9))
Ops.sfc <- function(e1, e2) {

	if (length(e1) == 0) # empty set
		return(e1)

	if ((is.matrix(e2) && ncol(e2) == 2) || (is.numeric(e2) && length(e2) == 2))
		e1 = st_zm(e1) # drop z and/or m

	if (!is.list(e2))
		e2 = list(e2)

	ret = switch(.Generic,
		"*" =  mapply(function(x, y) { x  * y }, e1, e2, SIMPLIFY = FALSE),
		"+" =  mapply(function(x, y) { x  + y }, e1, e2, SIMPLIFY = FALSE),
		"-" =  mapply(function(x, y) { x  - y }, e1, e2, SIMPLIFY = FALSE),
		"%%" = mapply(function(x, y) { x %% y }, e1, e2, SIMPLIFY = FALSE),
		"/"  = mapply(function(x, y) { x  / y }, e1, e2, SIMPLIFY = FALSE),
		stop(paste("operation", .Generic, "not supported")))

	crs = if (.Generic %in% c("+", "-") && inherits(e2, c("sfc", "sfg"))) # retain:
			st_crs(e1)
		else # geometry got displaced:
			NA_crs_

	st_sfc(ret, crs = crs, precision = attr(e1, "precision"))
}
