#' functions to interact with gdal not meant to be called directly by users (but e.g. by stars::read_stars)
#'
#' @param x character vector, possibly of length larger than 1 when more than one raster is read
#' @param ... ignored
#' @param options character; raster layer read options
#' @param driver character; when empty vector, driver is auto-detected.
#' @param read_data logical; if \code{FALSE}, only the imagery metadata is returned
#' @param NA_value (double) non-NA value to use for missing values; if \code{NA}, when writing missing values are not specially flagged in output dataset, when reading the default (dataset) missing values are used (if present / set).
#' @param RasterIO_parameters list with named parameters to GDAL's RasterIO; see the stars::read_stars documentation.
#' @details These functions are exported for the single purpose of being used by package stars, they are not meant to be used directly and may change or disappear without prior notice or deprecation warnings.
#' @name gdal
#' @export
gdal_read = function(x, ..., options = character(0), driver = character(0), read_data = TRUE, NA_value = NA_real_,
		RasterIO_parameters = list())
	CPL_read_gdal(as.character(x), as.character(options), as.character(driver), 
		as.logical(read_data), as.double(NA_value), RasterIO_parameters)

#' @name gdal
#' @export
#' @param type gdal write type
#' @param geotransform length 6 numeric vector with GDAL geotransform parameters.
#' @param update logical; \code{TRUE} if in an existing raster file pixel values shall be updated.
gdal_write = function(x, ..., file, driver = "GTiff", options = character(0), type = "Float32", 
		NA_value = NA_real_, geotransform, update = FALSE) {

	if (!requireNamespace("stars", quietly = TRUE))
		stop("stars required: install that first") # nocov

	d = stars::st_dimensions(x)
	xydims = attr(d, "raster")$dimensions
	if (!all.equal(match(xydims, names(d)), 1:2))
		stop("x and y raster dimensions need to be in place 1 and 2")
	from = c(d[[1]]$from, d[[2]]$from) - 1
	dims = c(d[[1]]$to, d[[2]]$to)
	if (length(d) == 3)
		dims = c(dims, d[[3]]$to - d[[3]]$from + 1)

	if (inherits(x, "stars_proxy")) {
		mat = matrix(0, 0, 0) # nocov start
		only_create = TRUE # don't write any pixel data
		if (!all(from == 0))
			warning("writing raster to original size") # otherwise, geotransform needs to be modified
		from = c(0, 0) # nocov end
	} else {
		mat = x[[1]]
		dm = dim(mat)
		if (is.factor(mat)) {
			rgba = NULL
			ex = attr(mat, "exclude")
			if (is.null(ex))
				lev = c("", levels(mat)) # add "" for value 0: R factors start at 1
			else {
				if (any(ex)) {
					lev = vector("character", length(ex)) # fills with ""
					lev[!ex] = levels(mat)
					rgba = if (!is.null(co <- attr(mat, "rgba"))) {
						n = length(ex)
						coltab = cbind(rep(0., n), rep(0, n), rep(0, n), rep(255, n))
						coltab[!ex,] = co
						coltab 
					}
					values = which(!ex) - 1
					mat = values[as.numeric(mat)]
				} else
					lev = levels(mat)
			}
			mat = structure(mat, class = NULL, levels = lev, dim = dm, rgba = rgba)
		}
		only_create = FALSE # write x too
		if (! update) {
			if (!all(from == 0))
				stop("cannot write sub-rasters only")
			if (!all(dims == dm))
				stop("dimensions don't match")
		}
		dim(mat) = c(dm[1], prod(dm[-1])) # flatten to 2-D matrix
	}
	if (length(dims) == 2)
		dims = c(dims, 1) # one band
	else if (is.character(d[[3]]$values)) # add band descriptions?
		attr(mat, "descriptions") = d[[3]]$values

	CPL_write_gdal(mat, file, driver, options, type, dims, from, geotransform,
		st_crs(x)[[2]], as.double(NA_value), create = !update, only_create = only_create)
}

#' @param gt double vector of length 6
#' @name gdal
#' @details gdal_inv_geotransform returns the inverse geotransform
#' @export
gdal_inv_geotransform = function(gt)
	CPL_inv_geotransform(as.double(gt))

## @param x two-column matrix with columns and rows, as understood by GDAL; 0.5 refers to the first cell's center; 
## FIXME: this is now duplicate in sf and stars
xy_from_colrow = function(x, geotransform, inverse = FALSE) {
# http://www.gdal.org/classGDALDataset.html , search for geotransform:
# 0-based indices:
# Xp = geotransform[0] + P*geotransform[1] + L*geotransform[2];
# Yp = geotransform[3] + P*geotransform[4] + L*geotransform[5];
	if (inverse) {
		geotransform = gdal_inv_geotransform(geotransform) # nocov start
		if (any(is.na(geotransform)))
			stop("geotransform not invertible") # nocov end
	}
	stopifnot(ncol(x) == 2)
	matrix(geotransform[c(1, 4)], nrow(x), 2, byrow = TRUE) + 
		x %*% matrix(geotransform[c(2, 3, 5, 6)], nrow = 2, ncol = 2)
}

# convert x/y gdal dimensions into a list of points, or a list of square polygons
#' @export
st_as_sfc.dimensions = function(x, ..., as_points = NA, use_cpp = TRUE, which = seq_len(prod(dim(x))),
		geotransform) {

	if (is.na(as_points))
		stop("as_points should be set to TRUE (`points') or FALSE (`polygons')")

	xy2sfc = function(cc, dm, as_points) { # form points or polygons from a matrix with corner points
		if (as_points)
			unlist(apply(cc, 1, function(x) list(st_point(x))), recursive = FALSE)[which]
		else {
			stopifnot(prod(dm) == nrow(cc))
			lst = vector("list", length = prod(dm - 1))
			for (y in 1:(dm[2]-1)) {
				for (x in 1:(dm[1]-1)) {
					i1 = (y - 1) * dm[1] + x      # top-left
					i2 = (y - 1) * dm[1] + x + 1  # top-right
					i3 = (y - 0) * dm[1] + x + 1  # bottom-right
					i4 = (y - 0) * dm[1] + x      # bottlom-left
					lst[[ (y-1)*(dm[1]-1) + x ]] = st_polygon(list(cc[c(i1,i2,i3,i4,i1),]))
				}
			}
			lst[which]
		}
	}

	raster = attr(x, "raster")
	xy_names = raster$dimensions
	xd = x[[ xy_names[1] ]]
	yd = x[[ xy_names[2] ]]
	cc = if (!is.na(xd$offset) && !is.na(yd$offset)) {
		xy = if (as_points) # grid cell centres:
				expand.grid(x = seq(xd$from, xd$to) - 0.5, y = seq(yd$from, yd$to) - 0.5)
			else # grid corners: from 0 to n
				expand.grid(x = seq(xd$from - 1, xd$to), y = seq(yd$from - 1, yd$to))
		xy_from_colrow(as.matrix(xy), geotransform)
	} else if (is.null(xd$values) || is.null(yd$values)) { # only one of [xd|yd] has $values:
		if (!requireNamespace("stars", quietly = TRUE)) # nocov start
			stop("stars required: install that first")
		if (! as_points)
			stop("st_as_sfc(): mixed regular and rectilinear dimensions only supported if as_points = TRUE")
		as.matrix(st_coordinates(x)) # nocov end
	} else { # both xd and yd have $values:
		expand = function(x) { # might fail on the poles or dateline
			d = diff(x)
			c(x[1] - 0.5 * d[1], x + 0.5 * c(d, tail(d, 1)))
		}
		if (raster$curvilinear) { # expand jointly:
			if (!as_points && all(dim(xd$values) == dim(x)[xy_names])) { # expand from points to cells/polygons: 
				xd$values = apply((apply(xd$values, 1, expand)), 1, expand)
				yd$values = apply((apply(yd$values, 1, expand)), 1, expand)
			}
			cbind(as.vector(xd$values), as.vector(yd$values))
		} else { # rectlinear: expand independently
			if (! as_points) {
				xd$values = if (inherits(xd$values, "intervals"))
						c(xd$values$start, tail(xd$values$end, 1))
					else
						expand(xd$values)
				yd$values = if (inherits(yd$values, "intervals"))
						c(yd$values$start, tail(yd$values$end, 1))
					else
						expand(yd$values)
			} else {
				if (inherits(xd$values, "intervals"))
					xd$values = 0.5 * (xd$values$start + xd$values$end)
				if (inherits(yd$values, "intervals"))
					yd$values = 0.5 * (yd$values$start + yd$values$end)
			}
			as.matrix(expand.grid(x = xd$values, y = yd$values))
		}
	}
	dims = dim(x) + !as_points
	if (use_cpp)
		structure(CPL_xy2sfc(cc, as.integer(dims), as_points, as.integer(which)), 
			crs = st_crs(xd$refsys), n_empty = 0L, bbox = bbox.Mtrx(cc))
	else
		st_sfc(xy2sfc(cc, dims, as_points), crs = xd$refsys)
}


#' @details gdal_crs reads coordinate reference system from GDAL data set
#' @param file character; file name
#' @return object of class \code{crs}, see \link{st_crs}.
#' @name gdal
#' @export
gdal_crs = function(file, options = character(0)) {
	st_crs(CPL_get_crs(file, options)$crs)
}

#' @details get_metadata gets metadata of a raster layer
#' @name gdal
#' @export
#' @param domain_item character vector of length 0, 1 (with domain), or 2 (with domain and item); use \code{""} for the default domain, use \code{NA_character_} to query the domain names.
#' @param parse logical; should metadata be parsed into a named list (\code{TRUE}) or returned as character data?
#' @return named list with metadata items
#' @examples
#' \dontrun{
#'   f = system.file("tif/L7_ETMs.tif", package="stars")
#'   f = system.file("nc/avhrr-only-v2.19810901.nc", package = "stars")
#'   gdal_metadata(f)
#'   gdal_metadata(f, NA_character_)
#'   try(gdal_metadata(f, "wrongDomain"))
#'   gdal_metadata(f, c("", "AREA_OR_POINT"))
#' }
gdal_metadata = function(file, domain_item = character(0), options = character(0), parse = TRUE) {
	stopifnot(is.character(file))
	stopifnot(is.character(domain_item))
	stopifnot(length(domain_item) <= 2)
	stopifnot(is.character(options))
	if (length(domain_item) >= 1 && !is.na(domain_item[1]) &&
			!(domain_item[1] %in% CPL_get_metadata(file, NA_character_, options)))
		stop("domain_item[1] not found in available metadata domains")
	p = CPL_get_metadata(file, domain_item, options)
	if (!is.na(domain_item[1]) && parse)
		split_strings(p)
	else
		p
}

split_strings = function(md, split = "=") {
	splt = strsplit(md, split)
	lst = lapply(splt, function(x) if (length(x) <= 1) NA_character_ else x[[2]])
	structure(lst, names = sapply(splt, function(x) x[[1]]), class = "gdal_metadata")
}

#' @param name logical; retrieve name of subdataset? If \code{FALSE}, retrieve description
#' @export
#' @return \code{gdal_subdatasets} returns a zero-length list if \code{file} does not have subdatasets, and else a named list with subdatasets.
#' @name gdal
#' @details gdal_subdatasets returns the subdatasets of a gdal dataset
gdal_subdatasets = function(file, options = character(0), name = TRUE) {
	if (!("SUBDATASETS" %in% CPL_get_metadata(file, NA_character_, options)))
		list()
	else {
		md = gdal_metadata(file, "SUBDATASETS", options, TRUE)
		if (name)
			md[seq(1, length(md), by = 2)]
		else
			md[seq(2, length(md), by = 2)]
	}
}

#' @param use_integer boolean; if \code{TRUE}, raster values are read as (and rounded to) unsigned 32-bit integers values; if \code{FALSE} they are read as 32-bit floating points numbers. The former is supposedly faster.
#' @param mask stars object with NA mask (0 where NA), or NULL
#' @param breaks numeric vector with break values for contour polygons (or lines)
#' @param use_contours logical;
#' @param contour_lines logical;
#' @param connect8 logical; if \code{TRUE} use 8 connection algorithm, rather than 4
#' @name gdal
#' @export
gdal_polygonize = function(x, mask = NULL, file = tempfile(), driver = "GTiff", use_integer = TRUE,
		geotransform, breaks = classInt::classIntervals(na.omit(as.vector(x[[1]])))$brks, 
		use_contours = FALSE, contour_lines = FALSE, connect8 = FALSE, ...) {
	gdal_write(x, file = file, driver = driver, geotransform = geotransform) # nocov start
	on.exit(unlink(file))
	mask_name = if (!is.null(mask)) {
			mask_name = tempfile()
			gdal_write(mask, file = mask_name, driver = driver, geotransform = geotransform)
			on.exit(unlink(mask_name))
			mask_name
		} else
			character(0)
	contour_options = if (use_contours) { # construct contour_options:
			nbreaks = breaks
			if (max(breaks) == max(x[[1]], na.rm = TRUE)) # expand, because GDAL will not include interval RHS
				nbreaks[length(nbreaks)] = breaks[length(breaks)] * 1.01
			c(paste0("FIXED_LEVELS=", paste0(nbreaks, collapse = ",")),
			"ELEV_FIELD=0", "ELEV_FIELD_MIN=1", "ELEV_FIELD_MAX=2",
			paste0("POLYGONIZE=", ifelse(contour_lines, "NO", "YES")))
		} else
			character(0)

	options = if (connect8)
			"8CONNECTED=8"
		else
			character(0)

	pol = CPL_polygonize(file, mask_name, "GTiff", "Memory", "foo", options, 0, 
		contour_options, use_contours, use_integer)
	out = process_cpl_read_ogr(pol, quiet = TRUE)
	names(out)[1] = names(x)[1]
	if (! contour_lines && use_contours) {
#		if (out$Min[1] == 0 && out$Min[1] > min(breaks))
#			out$Min[1] = -Inf
#
# https://github.com/r-spatial/sf/pull/1608 : 

		i <- match(out$Max[1], sort(breaks))
		out$Min[1] = if (!is.na(i) && i > 1)
				sort(breaks)[i - 1]
			else 
				-Inf

		out$Max[out$Max == 2^32 - 1] = Inf
		f = paste0("[", out$Min, ",", out$Max, ")")
		out[[1]] = factor(f, levels = f)
	} else
		out$Min = out$Max = NULL
	out # nocov end
}

#' @param sf object of class \code{sf}
#' @name gdal
#' @export
gdal_rasterize = function(sf, x, gt, file, driver = "GTiff", options = character()) {
	gdal_write(x, file = file, driver = driver, geotransform = gt)
	geoms = which(sapply(sf, inherits, "sfc"))
	values = as.double(t(as.matrix(as.data.frame(sf)[-geoms])))
	CPL_rasterize(file, driver, st_geometry(sf), values, options, NA_real_);
}

#' @export
#' @name gdal
#' @param f gdal raster data source filename
#' @param pts points matrix
#' @param bilinear logical; use bilinear interpolation, rather than nearest neighbor?
gdal_extract = function(f, pts, bilinear = FALSE) {
	CPL_extract(f, pts, as.logical(bilinear))
}

#' @name gdal
#' @param file file name
#' @param array_name array name
#' @param options open options
#' @export
gdal_read_mdim = function(file, array_name = character(0), options = character(0)) {
	read_mdim(file, array_name, options)
}

#' @name gdal
#' @param dimension_values list with dimension values
#' @param units character; units names (udunits conform) corresponding to dimension_values
#' @export
gdal_write_mdim = function(x, file, dimension_values, units) {
	write_mdim(x, file, dimension_values, units)
}

#' @name gdal
#' @param f character; file name
#' @param nxy integer vector of length 2
#' @param values fill value
#' @param crs object of class \code{crs}
#' @param xlim numeric
#' @param ylim numeric
#' @export
gdal_create = function(f, nxy, values, crs, xlim, ylim) {
	CPL_create(as.character(f), as.integer(nxy), as.double(values), crs$wkt, as.double(xlim), as.double(ylim))
}

#' add or remove overviews to/from a raster image
#'
#' add or remove overviews to/from a raster image
#' @param file character; file name
#' @param overviews integer; overview levels
#' @param method character; method to create overview; one of: nearest, average, rms, gauss, cubic, cubicspline, lanczos, average_mp, average_magphase, mode
#' @param layers integer; layers to create overviews for (default: all)
#' @param options character; dataset opening options
#' @param clean logical; if \code{TRUE} only remove overviews, do not add
#' @param read_only logical; if \code{TRUE}, add overviews to another file with extension \code{.ovr} added to \code{file}
#' @return \code{TRUE}, invisibly, on success
#' @seealso \link{gdal_utils} for access to other gdal utilities that have a C API
#' @export
gdal_addo = function(file, overviews = c(2,4,8,16), method = "NEAREST", layers = integer(0), 
					 options = character(0), clean = FALSE, read_only = FALSE) {
	stopifnot(length(method) == 1, is.character(method), is.numeric(overviews))
	invisible(CPL_gdaladdo(file, method, as.integer(overviews), as.integer(layers), as.character(options), 
				 as.logical(clean)[1], as.logical(read_only)[1]))
}
