# convert character string, as typically PostgreSQL returned blobs, to raw vector;
# skips a leading "0x", as this is created by PostGIS when using ST_asBinary() 
#
# most wkb read/write stuff was modified & extended from Ian Cook's wkb package, 
# https://cran.r-project.org/web/packages/wkb/index.html
#
hex_to_raw = function(y) {
	stopifnot((nchar(y) %% 2) == 0)
	if (substr(y, 1, 2) == "0x")
		y = substr(y, 3, nchar(y))
	as.raw(as.numeric(paste0("0x", vapply(seq_len(nchar(y)/2), 
		function(x) substr(y, (x-1)*2+1, x*2), "")))) # SLOW, hence the Rcpp implementation
}

skip0x = function(x) {
	if (is.na(x))
		"010700000000000000" # empty GeometryCollection, st_as_binary(st_geometrycollection())
	else if (substr(x, 1, 2) == "0x")
		substr(x, 3, nchar(x))
	else
		x
}

#' @name st_as_sfc
#' @param EWKB logical; if TRUE, parse as EWKB (PostGIS: ST_AsEWKB), otherwise as ISO WKB (PostGIS: ST_AsBinary)
#' @param pureR logical; if TRUE, use only R code, if FALSE, use compiled (C++) code; use TRUE when the endian-ness of the binary differs from the host machine (\code{.Platform$endian}).
#' @details when converting from WKB, the object \code{x} is either a character vector such as typically obtained from PostGIS (either with leading "0x" or without), or a list with raw vectors representing the features in binary (raw) form.
#' @examples
#' wkb = structure(list("01010000204071000000000000801A064100000000AC5C1441"), class = "WKB")
#' st_as_sfc(wkb, EWKB = TRUE)
#' wkb = structure(list("0x01010000204071000000000000801A064100000000AC5C1441"), class = "WKB")
#' st_as_sfc(wkb, EWKB = TRUE)
#' @export
st_as_sfc.WKB = function(x, ..., EWKB = FALSE, pureR = FALSE, crs = NA_crs_) {
    if (all(vapply(x, is.character, TRUE))) {
		x <- if (pureR)
				structure(lapply(x, hex_to_raw), class = "WKB")
			else 
				structure(CPL_hex_to_raw(vapply(x, skip0x, USE.NAMES = FALSE, "")), class = "WKB")
	} else # direct call with raw:
		stopifnot(inherits(x, "WKB") && all(vapply(x, is.raw, TRUE))) # WKB as raw
	if (any(lengths(x) == 0))
		stop("cannot read WKB object from zero-length raw vector")
	ret = if (pureR)
			R_read_wkb(x, readWKB, EWKB = EWKB)
		else
			CPL_read_wkb(x, EWKB = EWKB, endian = as.integer(.Platform$endian == "little"))
	if (is.na(crs) && EWKB && !is.null(attr(ret, "epsg")) && attr(ret, "epsg") != 0)
		crs = attr(ret, "epsg")
	attr(ret, "epsg") = NULL
	st_sfc(ret, crs = crs)
}

R_read_wkb = function(x, readWKB, EWKB = EWKB) {
	ret = lapply(x, readWKB, EWKB = EWKB)
	epsg = attr(ret[[1]], "epsg")
	ret = lapply(ret, function(x) { attr(x, "epsg") = NULL; x })
	attr(ret, "epsg") = epsg
	ret
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
	   # *: GDAL support see https://trac.osgeo.org/gdal/ticket/6401

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
	stopifnot(!is.na(tp))
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
	stopifnot(!is.na(tp))
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
		CURVE = ,
		CIRCULARSTRING = ,
		LINESTRING = readMatrix(rc, pt$dims, endian),
		SURFACE = ,
		POLYGON = , 
		TRIANGLE = readMatrixList(rc, pt$dims, endian),
		MULTIPOINT = readMPoints(rc, pt$dims, endian, EWKB),
		MULTILINESTRING = , 
		MULTICURVE = ,
		MULTIPOLYGON = , 
		MULTISURFACE = ,
		POLYHEDRALSURFACE = , 
		TIN = lapply(readGC(rc, pt$dims, endian, EWKB), unclass),
		GEOMETRYCOLLECTION = readGC(rc, pt$dims, endian, EWKB),
		CURVEPOLYGON = readGC(rc, pt$dims, endian, EWKB),
		stop(paste("type", pt$tp, "unsupported")))
	class(ret) <- c(pt$zm, pt$tp, "sfg")
	if (!is.na(srid))
		attr(ret, "epsg") <- srid
	ret
}

readPoint = function(rc, dims, endian) {
	readBin(rc, what = "double", n = as.integer(dims), size = 8L, endian = endian)
}
readMPoints = function(rc, dims, endian, EWKB) {
	npts = readBin(rc, what = "integer", n = 1L, size = 4L, endian = endian)
	do.call(rbind, lapply(seq_len(npts), function(x) readData(rc, EWKB)))
}
readMatrix = function(rc, dims, endian) {
	npts = readBin(rc, what = "integer", n = 1L, size = 4L, endian = endian)
	m = readBin(rc, what = "double", n = as.integer(npts * dims), size = 8L, endian = endian)
	t(matrix(m, nrow = dims)) # x1 y1, x2 y2 etc -> t()
}
readMatrixList = function(rc, dims, endian) {
	nmtrx = readBin(rc, what = "integer", n = 1L, size = 4L, endian = endian)
	lapply(seq_len(nmtrx), function(x) readMatrix(rc, dims, endian))
}
#readMatrixListList = function(rc, dims, endian) {
#	nmtrxl = readBin(rc, what = "integer", n = 1L, size = 4L, endian = endian)
#	lapply(seq_len(nmtrxl), function(x) readMatrixList(rc, dims, endian))
#}
readGC = function(rc, dims, endian, EWKB) {
	ngc = readBin(rc, what = "integer", n = 1L, size = 4L, endian = endian)
	lapply(seq_len(ngc), function(x) readData(rc, EWKB))
}

#' Convert sfc object to an WKB object
#'
#' Convert sfc object to an WKB object
#' @param x object to convert
#' @param ... ignored
#' @name st_as_binary
#' @export
st_as_binary = function(x, ...) UseMethod("st_as_binary")

#' @name st_as_binary
#' @param endian character; either "big" or "little"; default: use that of platform
#' @param EWKB logical; use EWKB (PostGIS), or (default) ISO-WKB?
#' @param pureR logical; use pure R solution, or C++?
#' @param precision numeric; if zero, do not modify; to reduce precision: negative values convert to float (4-byte real); positive values convert to round(x*precision)/precision. See details.
#' @param hex logical; return hexadecimal encoded (character)?
#' @details for the precion model, see also \url{http://tsusiatsoftware.net/jts/javadoc/com/vividsolutions/jts/geom/PrecisionModel.html}. There, it is written that: ``... to specify 3 decimal places of precision, use a scale factor of 1000. To specify -3 decimal places of precision (i.e. rounding to the nearest 1000), use a scale factor of 0.001.''. Note that ALL coordinates, so also Z or M values (if present) are affected.
#' @export
st_as_binary.sfc = function(x, ..., EWKB = FALSE, endian = .Platform$endian, pureR = FALSE,
		precision = attr(x, "precision"), hex = FALSE) {
	stopifnot(endian %in% c("big", "little"))
	if (pureR && precision != 0.0)
		stop("for non-zero precision values, use pureR = FALSE")
	if (pureR) 
		structure(lapply(x, st_as_binary, EWKB = EWKB, pureR = pureR, endian = endian), class = "WKB")
	else {
		stopifnot(endian == .Platform$endian)
		structure(CPL_write_wkb(x, EWKB, endian == "little", Dimension(x[[1]]), precision), 
				class = "WKB")
	}
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

#' @name st_as_binary
#' @export
st_as_binary.sfg = function(x, ..., endian = .Platform$endian, EWKB = FALSE, pureR = FALSE, 
		hex = FALSE) {
	stopifnot(endian %in% c("big", "little"))
	if (! pureR) {
		stopifnot(endian == .Platform$endian)
		CPL_write_wkb(st_sfc(x), EWKB, endian == "little", Dimension(x), 0.0)[[1]]
	} else {
		rc <- rawConnection(raw(0), "r+")
		on.exit(close(rc))
		writeData(x, rc, endian, EWKB)
		rawConnectionValue(rc)
	}
}

#' Convert raw vector(s) into hexadecimal character string(s)
#'
#' Convert raw vector(s) into hexadecimal character string(s)
#' @param x raw vector, or list with raw vectors 
#' @export
rawToHex = function(x) {
	if (is.raw(x))
		CPL_raw_to_hex(x)
	else if (is.list(x) && all(vapply(x, is.raw, TRUE)))
		vapply(x, function(rw) CPL_raw_to_hex(rw), "")
	else
		stop(paste("not implemented for objects of class", class(x)))
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
		LINESTRING = writeMatrix(x, rc, endian),
		POLYGON = , 
		TRIANGLE = writeMatrixList(x, rc, endian),
		MULTIPOINT = writeMPoints(x, rc, endian, EWKB),
		POLYHEDRALSURFACE = , 
		TIN = , 
		MULTILINESTRING = , 
		MULTIPOLYGON = writeMulti(x, rc, endian, EWKB),
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
	if (nrow(x))
		apply(x, 1, function(y) writeData(st_point(y, class(x)[1]), rc, endian, EWKB))
}
