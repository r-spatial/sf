NAmat2xyList <- function(xy) {
	NAs <- unclass(attr(na.omit(xy), "na.action"))
	if ((length(NAs) == 1L) && (NAs == nrow(xy))) {
		xy <- xy[-nrow(xy)] # nocov
		NAs <- NULL # nocov
	}
# NA problem found by Edzer Pebesma, 24/8-06
	diffNAs <- diff(NAs)
	if (any(diffNAs == 1)) {
		xy <- xy[-(NAs[which(diffNAs == 1)] + 1), ] # nocov
		NAs <- unclass(attr(na.omit(xy), "na.action")) # nocov
	}
	nParts <- length(NAs) + 1L
# two NAs at end of file 070905 RSB
# no NAs at all RSB 080814
	if (!is.null(NAs) && nrow(xy) == NAs[length(NAs)])
            nParts <- nParts - 1
	res <- vector(mode="list", length=nParts)
	from <- integer(nParts)
	to <- integer(nParts)
	from[1] <- 1
	to[nParts] <- nrow(xy)
# two NAs at end of file 070905 RSB
# no NAs at all RSB 080814
	if (!is.null(NAs) && nrow(xy) == NAs[length(NAs)])
		to[nParts] <- to[nParts] - 1
	if (nParts > 1) {
		for (i in 2:nParts) {
			to[(i-1)] <- NAs[(i-1)]-1
			from[i] <- NAs[(i-1)]+1
		}
	}
	for (i in seq_len(nParts))
		res[[i]] <- xy[from[i]:to[i],, drop = FALSE]
	res
}

map2pol = function(xyList, ID) {
	# close rings:
	xyList = lapply(xyList, ClosePol)
	# group into MULTIPOLYGON:
	uID = unique(ID)
	ret = vector("list", length(uID))
	for (g in seq_along(uID))
		ret[[g]] = st_multipolygon(lapply(xyList[ uID[g] == ID ], function(x) list(x)))
	st_sfc(ret)
}

map2lin = function(xyList, ID) {
	# group into MULTIPOLYGON:
	uID = unique(ID)
	ret = vector("list", length(uID))
	for (g in seq_along(uID))
		ret[[g]] = st_multilinestring(xyList[ uID[g] == ID ])
	st_sfc(ret)
}


#' @export
#' @name st_as_sf
#' @param fill logical; the value for \code{fill} that was used in the call to \link[maps]{map}.
#' @param group logical; if \code{TRUE}, group id labels from \link[maps]{map} by their prefix before \code{:}
st_as_sf.map = function(x, ..., fill = TRUE, group = TRUE) {
	ID = if (group)
			vapply(strsplit(x$names, ":"), function(y) y[1], "")
		else 
			x$names
	xyList <- NAmat2xyList(cbind(x$x, x$y))
	geom = if (fill)
			map2pol(xyList, ID)
		else
			map2lin(xyList, ID)
	st_sf(ID = unique(ID), geom = geom, crs = 4326)
}


#' @export
#' @name st_as_sfc
st_as_sfc.map = function(x, ...) {
	st_geometry(st_as_sf(x, ...))
}
