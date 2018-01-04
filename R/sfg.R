# dim: what does the third dimension, if present, refer to? (XYZ or XYM)
getClassDim = function(x, d, dim = "XYZ", type) {
	stopifnot(d > 1 && d < 5)
	type = toupper(type)
	if (d == 2)
		c("XY", type, "sfg")
	else if (d == 3) {
		stopifnot(dim %in% c("XYZ", "XYM"))
		c(dim, type, "sfg")
	} else if (d == 4)
		c("XYZM", type, "sfg")
	else stop(paste(d, "is an illegal number of columns for a", type))
}

is_numeric_matrix = function(x)
	stopifnot(is.numeric(x) && is.matrix(x))

Mtrx = function(x, dim = "XYZ", type) {
	is_numeric_matrix(x)
	structure(x, class = getClassDim(x, ncol(x), dim, type))
}

# creates object of class c(dim, type, "sfg") from list x, possibly checking rings are closed
MtrxSet = function(x, dim = "XYZ", type, needClosed = FALSE) {
	stopifnot(is.list(x))
	if (length(x) > 0) { # list()
		nc = unique(vapply(x, ncol, 0L))
		if (length(nc) != 1)
			stop("matrices having unequal number of columns")
		lapply(x, is_numeric_matrix)
		NotClosed = function(y) any(head(y, 1) != tail(y, 1))
		if (needClosed && any(vapply(x, NotClosed, TRUE)))
			stop("polygons not (all) closed")
		structure(x, class = getClassDim(x, nc, dim, type))
	} else
		structure(x, class = getClassDim(x, nchar(dim), dim, type))
}

# creates object of class c(dim, type, "sfg") from list x, d, possibly checking rings are closed
MtrxSetSet = function(x, dim = "XYZ", type, needClosed = FALSE) {
	stopifnot(is.list(x) && all(vapply(x, is.list, TRUE)))
	if (length(x)) {
		nc = unique(unlist(lapply(x, function(y) vapply(y, ncol, 0L))))
		if (length(nc) != 1)
			stop("matrices having unequal number of columns")
		lapply(x, function(y) lapply(y, is_numeric_matrix))
		NotClosed = function(y) any(head(y, 1) != tail(y, 1))
		if (needClosed && any(unlist(lapply(x, function(y) vapply(y, NotClosed, TRUE)))))
			stop("polygons not (all) closed")
		structure(x, class = getClassDim(x, nc, dim, type))
	} else
		structure(x, class = getClassDim(x, nchar(dim), dim, type))
}

#return "XY", "XYZ", "XYM", or "XYZM"
Dimension = function(x) {
	stopifnot(inherits(x, "sfg"))
	class(x)[1]
}

#' Create simple feature from a numeric vector, matrix or list
#'
#' Create simple feature from a numeric vector, matrix or list
#' @param x for \code{st_point}, numeric vector (or one-row-matrix) of length 2, 3 or 4; for \code{st_linestring} and \code{st_multipoint}, numeric matrix with points in rows; for \code{st_polygon} and \code{st_multilinestring}, list with numeric matrices with points in rows; for \code{st_multipolygon}, list of lists with numeric matrices; for \code{st_geometrycollection} list with (non-geometrycollection) simple feature objects
#' @param dim character, indicating dimensions: "XY", "XYZ", "XYM", or "XYZM"; only really needed for three-dimensional points (which can be either XYZ or XYM) or empty geometries; see details
#' @name st
#' @details "XYZ" refers to coordinates where the third dimension represents altitude, "XYM" refers to three-dimensional coordinates where the third dimension refers to something else ("M" for measure); checking of the sanity of \code{x} may be only partial.
#' @return object of the same nature as \code{x}, but with appropriate class attribute set
#' @examples
#' (p1 = st_point(c(1,2)))
#' class(p1)
#' st_bbox(p1)
#' (p2 = st_point(c(1,2,3)))
#' class(p2)
#' (p3 = st_point(c(1,2,3), "XYM"))
#' pts = matrix(1:10, , 2)
#' (mp1 = st_multipoint(pts))
#' pts = matrix(1:15, , 3)
#' (mp2 = st_multipoint(pts))
#' (mp3 = st_multipoint(pts, "XYM"))
#' pts = matrix(1:20, , 4)
#' (mp4 = st_multipoint(pts))
#' pts = matrix(1:10, , 2)
#' (ls1 = st_linestring(pts))
#' pts = matrix(1:15, , 3)
#' (ls2 = st_linestring(pts))
#' (ls3 = st_linestring(pts, "XYM"))
#' pts = matrix(1:20, , 4)
#' (ls4 = st_linestring(pts))
#' outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#' hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#' hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#' pts = list(outer, hole1, hole2)
#' (ml1 = st_multilinestring(pts))
#' pts3 = lapply(pts, function(x) cbind(x, 0))
#' (ml2 = st_multilinestring(pts3))
#' (ml3 = st_multilinestring(pts3, "XYM"))
#' pts4 = lapply(pts3, function(x) cbind(x, 0))
#' (ml4 = st_multilinestring(pts4))
#' outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#' hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#' hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#' pts = list(outer, hole1, hole2)
#' (pl1 = st_polygon(pts))
#' pts3 = lapply(pts, function(x) cbind(x, 0))
#' (pl2 = st_polygon(pts3))
#' (pl3 = st_polygon(pts3, "XYM"))
#' pts4 = lapply(pts3, function(x) cbind(x, 0))
#' (pl4 = st_polygon(pts4))
#' pol1 = list(outer, hole1, hole2)
#' pol2 = list(outer + 12, hole1 + 12)
#' pol3 = list(outer + 24)
#' mp = list(pol1,pol2,pol3)
#' (mp1 = st_multipolygon(mp))
#' pts3 = lapply(mp, function(x) lapply(x, function(y) cbind(y, 0)))
#' (mp2 = st_multipolygon(pts3))
#' (mp3 = st_multipolygon(pts3, "XYM"))
#' pts4 = lapply(mp2, function(x) lapply(x, function(y) cbind(y, 0)))
#' (mp4 = st_multipolygon(pts4))
#' (gc = st_geometrycollection(list(p1, ls1, pl1, mp1)))
#' st_geometrycollection() # empty geometry
#' @export
st_point = function(x = c(NA_real_, NA_real_), dim = "XYZ") {
	stopifnot(is.numeric(x))
	if (is.matrix(x))
		stopifnot(nrow(x) == 1) # because we want to be able to call rbind on points
	structure(x, class = getClassDim(x, length(x), dim, "POINT"))
}
#' @name st
#' @export
st_multipoint = function(x = matrix(numeric(0), 0, 2), dim = "XYZ") Mtrx(x, dim, type = "MULTIPOINT")
#' @name st
#' @export
st_linestring = function(x = matrix(numeric(0), 0, 2), dim = "XYZ") Mtrx(x, dim, type = "LINESTRING")
#' @name st
#' @export
st_polygon = function(x = list(), dim = if(length(x)) "XYZ" else "XY") {
	if (identical(x, 1))
		st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0))))
	else MtrxSet(x, dim, type = "POLYGON", needClosed = TRUE)
}
#' @name st
#' @export
st_multilinestring = function(x = list(), dim = if (length(x)) "XYZ" else "XY")
	MtrxSet(x, dim, type = "MULTILINESTRING", needClosed = FALSE)
#' @name st
#' @export
st_multipolygon = function(x = list(), dim = if (length(x)) "XYZ" else "XY")
	MtrxSetSet(x, dim, type = "MULTIPOLYGON", needClosed = TRUE)
#' @name st
#' @param dims character; specify dimensionality in case of an empty (NULL) geometrycollection, in which case \code{x} is the empty \code{list()}.
#' @export
st_geometrycollection = function(x = list(), dims = "XY") {
	cls = vapply(x, class, rep("", 3))
	if (length(cls)) {
		if (!is.matrix(cls) || !is.character(cls) || nrow(cls) != 3)
			stop("st_geometrycollection parameter x error: list elements should be simple features")
		stopifnot(all(cls[3,] == "sfg"))
		stopifnot(all(cls[2,] != "GEOMETRYCOLLECTION")) # can't recurse!
		# check all dimensions are equal:
		dims = unique(cls[1,])
		if (length(dims) > 1)
			stop(paste("multiple dimensions found:", paste(dims, collapse = ", ")))
	}
	structure(x, class = c(dims, "GEOMETRYCOLLECTION", "sfg")) # TODO: no Z/M/ZM modifier here??
}

POINT2MULTIPOINT = function(x, dim = "XYZ") {
	if (length(x) == 3) # disambiguate Z/M:
		dim = class(x)[1]
	st_multipoint(matrix(unclass(x), 1), dim = dim)
}
LINESTRING2MULTILINESTRING = function(x, dim = "XYZ") {
	if (ncol(x) == 3) # disambiguate Z/M:
		dim = class(x)[1]
	st_multilinestring(list(unclass(x)), dim = dim)
}
POLYGON2MULTIPOLYGON = function(x, dim = "XYZ") {
	if (ncol(x[[1]]) == 3) # disambiguate Z/M:
		dim = class(x)[1]
	st_multipolygon(list(unclass(x)), dim = dim)
}

#' @name st
#' @param width integer; number of characters to be printed (max 30; 0 means print everything)
#' @export
print.sfg = function(x, ..., width = 0) { # avoids having to write print methods for 68 classes:
	f = format(x, ..., width = width)
	message(f)
	invisible(f)
}

#' @name st
#' @param n integer; number of elements to be selected
#' @export
head.sfg = function(x, n = 10L, ...) {
	structure(head(unclass(x), n = n, ...), class = class(x))
}

#' @name st
#' @export
format.sfg = function(x, ..., width = 30) {
	if (is.null(width))
		width = 30
#	if (object.size(x) > 1000)
#		x = head(x, 10)
	pr = st_as_text(x, ...)
	if (width > 0 && nchar(pr) > width)
		paste0(substr(pr, 1, width - 3), "...")
	else
		pr
}

#' @export
#' @name st
#' @param ... objects to be pasted together into a single simple feature
#' @param recursive logical; ignored
#' @param flatten logical; if TRUE, try to simplify results; if FALSE, return geometrycollection containing all objects
#' @examples
#' c(st_point(1:2), st_point(5:6))
#' c(st_point(1:2), st_multipoint(matrix(5:8,2)))
#' c(st_multipoint(matrix(1:4,2)), st_multipoint(matrix(5:8,2)))
#' c(st_linestring(matrix(1:6,3)), st_linestring(matrix(11:16,3)))
#' c(st_multilinestring(list(matrix(1:6,3))), st_multilinestring(list(matrix(11:16,3))))
#' pl = list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))
#' c(st_polygon(pl), st_polygon(pl))
#' c(st_polygon(pl), st_multipolygon(list(pl)))
#' c(st_linestring(matrix(1:6,3)), st_point(1:2))
#' c(st_geometrycollection(list(st_point(1:2), st_linestring(matrix(1:6,3)))),
#'   st_geometrycollection(list(st_multilinestring(list(matrix(11:16,3))))))
#' c(st_geometrycollection(list(st_point(1:2), st_linestring(matrix(1:6,3)))),
#'   st_multilinestring(list(matrix(11:16,3))), st_point(5:6),
#'   st_geometrycollection(list(st_point(10:11))))
#' @details When \code{flatten=TRUE}, this method may merge points into a multipoint structure, and may not preserve order, and hence cannot be reverted. When given fish, it returns fish soup. 
c.sfg = function(..., recursive = FALSE, flatten = TRUE) {

	stopifnot(! recursive)
	Paste0 = function(lst) lapply(lst, unclass)
	Paste1 = function(lst) do.call(c, lapply(lst, unclass))
	lst = list(...)
	if (flatten) {
		cls = vapply(lst, function(x) class(x)[2], "")
		ucls = unique(cls)
		if (length(ucls) == 1) {
			switch(ucls,
				POINT = st_multipoint(do.call(rbind, lst)),
				# CURVE = st_multicurve(Paste0(lst))
				# CIRCULARSTRING = st_geometrycollection(lst), # FIXME??
				LINESTRING = st_multilinestring(Paste0(lst)),
				# SURFACE = st_multisurface(Paste0(lst)),
				POLYGON = st_multipolygon(Paste0(lst)),
				# TRIANGLE = st_geometrycollection(lst),
				MULTIPOINT = st_multipoint(do.call(rbind, lst)),
				MULTILINESTRING = st_multilinestring(Paste1(lst)),
				# MULTICURVE = st_multicurve(Paste1(lst)),
				MULTIPOLYGON = st_multipolygon(Paste1(lst)),
				# MULTISURFACE = st_multisurface(Paste1(lst)),
				# POLYHEDRALSURFACE = st_polyhedralsurface(Paste1(lst)),
				# TIN = st_tin(Paste1(lst)),
				GEOMETRYCOLLECTION = st_geometrycollection(Paste1(lst)),
				stop(paste("type", cls, "not supported"))
			)
		} else if (all(ucls %in% c("POINT", "MULTIPOINT")))
			st_multipoint(do.call(rbind, lst))
		else if (all(cls %in% c("LINESTRING", "MULTILINESTRING"))) {
			ls = which(cls == "LINESTRING")
			mls = st_multilinestring(lst[ls])
			st_multilinestring(c(unlist(lst[-ls], FALSE), unclass(mls)))
		} else if (all(cls %in% c("POLYGON", "MULTIPOLYGON"))) {
			po = which(cls == "POLYGON")
			mpo = st_multipolygon(lst[po])
			st_multipolygon(c(unlist(lst[-po], FALSE), unclass(mpo)))
		} else {
			# unfold GC objects first, then
			gc = (cls == "GEOMETRYCOLLECTION")
			ret = lst[!gc]
			if (any(gc)) { # append the _contents_ of GC's to the non-GC elements:
				wgc = which(gc)
				for (i in seq_len(length(wgc)))
					ret = append(ret, lst[[wgc[i]]])
			}
			st_geometrycollection(ret)
		}
	} else # !flatten:
		st_geometrycollection(lst) # breaks if one of them is a GC
}

#' @name st
#' @method as.matrix sfg
#' @export
#' @return as.matrix returns the set of points that form a geometry as a single matrix, where each point is a row; use \code{unlist(x, recursive = FALSE)} to get sets of matrices.
as.matrix.sfg = function(x, ...) {
	switch(class(x)[2],
		POINT = matrix(x, 1),
		MULTIPOINT = as.matrix(unclass(x)),
		LINESTRING = as.matrix(unclass(x)),
		POLYGON = do.call(rbind, x),
		MULTILINESTRING = do.call(rbind, x),
		MULTIPOLYGON = do.call(rbind, lapply(x, function(y) do.call(rbind, y))),
		GEOMETRYCOLLECTION = do.call(rbind, lapply(x, as.matrix)),
		NextMethod()
	)
}
