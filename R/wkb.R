# convert character string, as typically PostgreSQL returned blobs, to raw vector;
# skips a leading "0x", as this is created by PostGIS when using ST_asBinary() 
#
# most wkb read/write stuff was modified & extended from Ian Cook's wkb package, 
# https://cran.r-project.org/web/packages/wkb/index.html
#
charToWKB = function(y) {
	stopifnot((nchar(y) %% 2) == 0)
	if (substr(y, 1, 2) == "0x")
		y = substr(y, 3, nchar(y))
	as.raw(as.numeric(paste0("0x", sapply(1:(nchar(y)/2), function(x) substr(y, (x-1)*2+1, x*2)))))
}

#' @name st_as_sfc
#' @param EWKB logical; if TRUE, parse as EWKB (PostGIS: ST_AsEWKB), otherwise as ISO WKB (PostGIS: ST_AsBinary)
#' @details when converting from WKB, the object \code{x} is either a character vector such as typically obtained from PostGIS (either with leading "0x" or without), or a list with raw vectors representing the features in binary form.
#' @examples
#' wkb = structure(list("01010000204071000000000000801A064100000000AC5C1441"), class = "WKB")
#' st_as_sfc(wkb, EWKB = TRUE)
#' wkb = structure(list("0x01010000204071000000000000801A064100000000AC5C1441"), class = "WKB")
#' st_as_sfc(wkb, EWKB = TRUE)
#' @export
st_as_sfc.WKB = function(x, ..., EWKB = FALSE) {
	skip0x = function(x) {
		if (substr(x, 1, 2) == "0x")
			substr(x, 3, nchar(x))
		else
			x
	}
    if (all(sapply(x, is.character)))
		#x = structure(lapply(x, charToWKB), class = "WKB")
		x = structure(lapply(x, function(y) HexToRaw(skip0x(y))), class = "WKB")
	ret = lapply(x, readWKB, EWKB = EWKB)
	if (EWKB) {
		epsg = sapply(ret, function(x) attr(x, "epsg"))
		epsg = if (is.list(epsg)) # they were all NULL -> missing
				NA_integer_
			else
				unique(epsg)
		if (length(epsg) > 1)
			stop(paste("more than one unique SRID found:", paste(epsg, collapse = ", ")))
	} else
		epsg = NA_integer_
	st_sfc(ret, epsg = epsg, ...)
}

sf.tp = toupper(c(
	# "Geometry",          # 0
	"Point",               # 1
	"LineString",          # 2
	"Polygon",             # 3
	"MultiPoint",          # 4
	"MultiLineString",     # 5
	"MultiPolygon",        # 6
	"GeometryCollection",  # 7
	"CircularString",      # 8 x
	"CompoundCurve",       # 9 x
	"CurvePolygon",        # 10 x
	"MultiCurve",          # 11 x
	"MultiSurface",        # 12 x
	"Curve",               # 13 x *
	"Surface",             # 14 x *
	"PolyhedralSurface",   # 15
	"TIN",                 # 16
	"Triangle"             # 17
	)) # "Geometry" = 0, should not be matched, is a superclass only
	   # x: not described in ISO document
	   # *: GDAL support described in https://trac.osgeo.org/gdal/ticket/6401

readWKB = function(x, EWKB = FALSE) {
	stopifnot(inherits(x, "raw"))
	rc <- rawConnection(x, "r")
	on.exit(close(rc))
	seek(rc, 0L)
	# read data:
	readData(rc, EWKB = EWKB)
}

parseTypeEWKB = function(wkbType, endian) {
	# following the OGC doc, 3001 is POINT with ZM; turns out, PostGIS does sth else - 
	# read WKB, as well as EWKB; this post is more inormative of what is going on:
	# https://lists.osgeo.org/pipermail/postgis-devel/2004-December/000710.html
	# (without SRID, Z, M and ZM this all doesn't matter)
	# comparison ISO WKB and EWKB:
	# https://lists.osgeo.org/pipermail/postgis-devel/2004-December/000695.html
	stopifnot(length(wkbType) == 4)
	if (endian == "little") {
		sf_type = as.numeric(wkbType[1])
		info = as.raw(as.integer(wkbType[4]) %/% 2^4)
	} else {
		sf_type = as.numeric(wkbType[4])
		info = as.raw(as.integer(wkbType[1]) %/% 2^4)
	}
	tp = sf.tp[sf_type] 
	has_srid = as.logical(info & as.raw(2)) # 2-bit is "on"?
	zm = if ((info & as.raw(12)) == as.raw(12))
		"XYZM"
	else if (info & as.raw(8))
		"XYZ"
	else if (info & as.raw(4))
		"XYM"
	else if (info == as.raw(0) || info == as.raw(2))
		"XY"
	else 
		stop(paste("unknown value for info:", info))
	list(dims = nchar(zm), zm = zm, tp = tp, has_srid = has_srid)
}

parseTypeISO = function(wkbType) {
	tp = sf.tp[wkbType %% 1000] 
	dd = wkbType %/% 1000
	zm = if (dd == 0)
		"XY"
	else if (dd == 1)
		"XYZ"
	else if (dd == 2)
		"XYM"
	else if (dd == 3)
		"XYZM"
	else
		stop(paste("unknown value for wkbType:", wkbType))
	list(dims = nchar(zm), zm = zm, tp = tp, has_srid = FALSE)
}

readData = function(rc, EWKB = FALSE) {
	# read byte order:
	byteOrder <- readBin(rc, what = "raw", size = 1L)
	stopifnot(byteOrder %in% c(as.raw(0L), as.raw(1L)))
	endian = ifelse(byteOrder == as.raw(1L), "little", "big")
	# read wkbType:
	srid = NA_integer_
	if (EWKB) {
		wkbType <- readBin(rc, what = "raw", n = 4L, size = 1L, endian = endian)
		pt <- parseTypeEWKB(wkbType, endian)
		if (pt$has_srid)
			srid <- readBin(rc, what = "integer", size = 4L, endian = endian)
	} else {
		wkbType <- readBin(rc, what = "integer", n = 1L, size = 4L, endian = endian)
		pt <- parseTypeISO(wkbType)
	}
	# read data part:
	ret = switch(pt$tp,
		POINT = readPoint(rc, pt$dims, endian),
		MULTIPOINT = readMPoints(rc, pt$dims, endian, EWKB),
		LINESTRING = readMatrix(rc, pt$dims, endian),
		POLYGON = , TRIANGLE = readMatrixList(rc, pt$dims, endian),
		MULTILINESTRING = , MULTIPOLYGON = , POLYHEDRALSURFACE = , 
		TIN = lapply(readGC(rc, pt$dims, endian, EWKB), unclass),
		GEOMETRYCOLLECTION = readGC(rc, pt$dims, endian, EWKB),
			stop(paste("type", pt$tp, "unsupported")))
	class(ret) <- c(pt$zm, pt$tp, "sfi")
	if (!is.na(srid))
		attr(ret, "epsg") <- srid
	ret
}

readPoint = function(rc, dims, endian) {
	readBin(rc, what = "double", n = as.integer(dims), size = 8L, endian = endian)
}
readMPoints = function(rc, dims, endian, EWKB) {
	npts = readBin(rc, what = "integer", n = 1L, size = 4L, endian = endian)
	do.call(rbind, lapply(1:npts, function(x) readData(rc, EWKB)))
}
readMatrix = function(rc, dims, endian) {
	npts = readBin(rc, what = "integer", n = 1L, size = 4L, endian = endian)
	m = readBin(rc, what = "double", n = as.integer(npts * dims), size = 8L, endian = endian)
	t(matrix(m, nrow = dims)) # x1 y1, x2 y2 etc -> t()
}
readMatrixList = function(rc, dims, endian) {
	nmtrx = readBin(rc, what = "integer", n = 1L, size = 4L, endian = endian)
	lapply(1:nmtrx, function(x) readMatrix(rc, dims, endian))
}
readMatrixListList = function(rc, dims, endian) {
	nmtrxl = readBin(rc, what = "integer", n = 1L, size = 4L, endian = endian)
	lapply(1:nmtrxl, function(x) readMatrixList(rc, dims, endian))
}
readGC = function(rc, dims, endian, EWKB) {
	ngc = readBin(rc, what = "integer", n = 1L, size = 4L, endian = endian)
	lapply(1:ngc, function(x) readData(rc, EWKB))
}

#' convert sfc object to an WKB object
#'
#' convert sfc object to an WKB object
#' @param x object to convert
#' @param ... further arguments
#' @name st_as_wkb
#' @export
st_as_wkb = function(x, ...) UseMethod("st_as_wkb")

#' @name st_as_wkb
#' @param endian character; either "big" or "little"; default: use that of platform
#' @export
st_as_wkb.sfc = function(x, ..., endian = .Platform$endian) {
	stopifnot(endian %in% c("big", "little"))
	ret = lapply(x, st_as_wkb, ..., endian = endian)
	class(ret) = "WKB"
	ret
}

createType = function(x, endian, EWKB = FALSE) {
	dims = x[1]  # "XY", "XYZ", "XYM", or "XYZM"
	cl = x[2]
	m = match(cl, sf.tp)
	if (is.na(m))
		stop(paste("Class", cl, "not matched"))
	# return:
	if (! EWKB) # ISO: add 1000s
		as.integer(m + switch(dims, "XYZ" = 1000, "XYM" = 2000, "XYZM" = 3000, 0))
	else { # EWKB: set higher bits
		ret = raw(4)
		ret[1] = as.raw(m) # set up little-endian
		ret[4] = as.raw(switch(dims, "XYZ" = 0x80, "XYM" = 0x40, "XYZM" = 0xC0, 0))
		if (endian == "big")
			rev(ret)
		else
			ret
	}
}

#' @name st_as_wkb
#' @export
st_as_wkb.sfi = function(x, ..., endian = .Platform$endian) {
	stopifnot(endian %in% c("big", "little"))
	# preamble:
	rc <- rawConnection(raw(0), "r+")
	on.exit(close(rc))
	# data:
	writeData(x, rc, endian)
	rawConnectionValue(rc)
}

writeData = function(x, rc, endian, EWKB = FALSE) {
	if (endian == "big")
		writeBin(as.raw(0L), rc)
	else
		writeBin(as.raw(1L), rc)
	if (EWKB)
		writeBin(createType(class(x), endian, TRUE), rc, size = 1L, endian = endian)
	else
		writeBin(createType(class(x)), rc, size = 4L, endian = endian)
	# TODO (?): write SRID in case of EWKB?
	# write out x:
	switch(class(x)[2],
		POINT = writeBin(as.vector(as.double(x)), rc, size = 8L, endian = endian),
		MULTIPOINT = writeMPoints(x, rc, endian, EWKB),
		LINESTRING = writeMatrix(x, rc, endian),
		POLYGON = , TRIANGLE = writeMatrixList(x, rc, endian),
		POLYHEDRALSURFACE = , TIN = , 
		MULTILINESTRING = , MULTIPOLYGON = writeMulti(x, rc, endian, EWKB),
		GEOMETRYCOLLECTION = writeGC(x, rc, endian, EWKB),
		stop(paste("unexpected possibility:", class(x)))
	)
}

writeMulti = function(x, rc, endian, EWKB) {
	unMulti = if (inherits(x, "MULTILINESTRING"))
		st_linestring
	else # MULTIPOLYGON, POLYHEDRALSURFACE, TIN:
		st_polygon
	writeBin(as.integer(length(x)), rc, size = 4L, endian = endian)
	lapply(lapply(x, unMulti, class(x)[1]), writeData, rc = rc, endian = endian, EWKB = EWKB)
}
writeGC = function(x, rc, endian, EWKB) {
	writeBin(as.integer(length(x)), rc, size = 4L, endian = endian)
	lapply(x, writeData, rc = rc, endian = endian, EWKB = EWKB) 
}
writeMatrix = function(x, rc, endian) {
	writeBin(as.integer(nrow(x)), rc, size = 4L, endian = endian)
	writeBin(as.double(as.vector(t(x))), rc, size = 8L, endian = endian)
}
writeMatrixList = function(x, rc, endian) {
	writeBin(as.integer(length(x)), rc, size = 4L, endian = endian)
	lapply(x, function(y) writeMatrix(y, rc, endian))
}
writeMPoints = function(x, rc, endian, EWKB) {
	writeBin(as.integer(nrow(x)), rc, size = 4L, endian = endian)
	apply(x, 1, function(y) writeData(st_point(y, class(x)[1]), rc, endian, EWKB))
}
