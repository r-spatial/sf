bbox.Mtrx = function(obj) {
	mn = apply(obj, 2, min)
	mx = apply(obj, 2, max)
	c(xmin = mn[1], xmax = mx[1], ymin = mn[2], ymax = mx[2])
}
bbox.MtrxSet = function(obj) {
	s = sapply(obj, bbox.Mtrx)
	c(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
}
bbox.MtrxSetSet = function(obj) {
	s = sapply(obj, bbox.MtrxSet)
	c(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
}
bbox.MtrxSetSetSet = function(obj) {
	s = sapply(obj, bbox.MtrxSetSet)
	c(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
}
#' Return bounding of a simple feature or simple feature set
#'
#' Return bounding of a simple feature or simple feature set
#' @param obj object to compute the bounding box from
#' @export
bbox = function(obj) UseMethod("bbox")

#' @export
bbox.POINT = function(obj) c(xmin = obj[1], xmax = obj[1], ymin = obj[2], ymax = obj[2])
#' @export
bbox.MULTIPOINT = bbox.Mtrx
#' @export
bbox.LINESTRING = bbox.Mtrx
#' @export
bbox.POLYGON = bbox.MtrxSet
#' @export
bbox.MULTILINESTRING = bbox.MtrxSet
#' @export
bbox.MULTIPOLYGON = bbox.MtrxSetSet
#' @export
bbox.GEOMETRYCOLLECTION = function(obj) {
	s = sapply(obj, bbox) # dispatch on class
	c(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
}

#' @export
bbox.sfc = function(obj) {
	switch(attr(obj, "type"),
		"POINT" = , "POINT Z" = , "POINT M" = , "POINT ZM" = bbox.Mtrx(do.call(rbind, obj)),
		"MULTIPOINT" = , "MULTIPOINT Z" = , "MULTIPOINT M" = , "MULTIPOINT ZM" = bbox.MtrxSet(obj),
		"LINESTRING" = , "LINESTRING Z" = , "LINESTRING M" = , "LINESTRING ZM" = bbox.MtrxSet(obj),
		"POLYGON" = , "POLYGON Z" = , "POLYGON M" = , "POLYGON ZM" = bbox.MtrxSetSet(obj),
		"MULTILINESTRING" = , "MULTILINESTRING Z" = , "MULTILINESTRING M" = , 
			"MULTILINESTRING ZM" = bbox.MtrxSetSet(obj),
		"MULTIPOLYGON" = , "MULTIPOLYGON Z" = , "MULTIPOLYGON M" = , 
			"MULTIPOLYGON ZM" = bbox.MtrxSetSetSet(obj),
		"GEOMETRYCOLLECTION" = , "GEOMETRYCOLLECTION Z" = , "GEOMETRYCOLLECTION M" = , 
			"GEOMETRYCOLLECTION ZM" = { 
				s = sapply(obj, bbox)
				c(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
			},
		stop("simple feature type not supported")
	)
}
