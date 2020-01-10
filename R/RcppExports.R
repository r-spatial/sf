# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

CPL_get_bbox <- function(sf, depth = 0L) {
    .Call('_sf_CPL_get_bbox', PACKAGE = 'sf', sf, depth)
}

CPL_gdal_init <- function() {
    invisible(.Call('_sf_CPL_gdal_init', PACKAGE = 'sf'))
}

CPL_gdal_cleanup_all <- function() {
    invisible(.Call('_sf_CPL_gdal_cleanup_all', PACKAGE = 'sf'))
}

CPL_gdal_version <- function(what = "RELEASE_NAME") {
    .Call('_sf_CPL_gdal_version', PACKAGE = 'sf', what)
}

CPL_crs_parameters <- function(crs) {
    .Call('_sf_CPL_crs_parameters', PACKAGE = 'sf', crs)
}

CPL_wkt_from_user_input <- function(input) {
    .Call('_sf_CPL_wkt_from_user_input', PACKAGE = 'sf', input)
}

CPL_crs_equivalent <- function(crs1, crs2) {
    .Call('_sf_CPL_crs_equivalent', PACKAGE = 'sf', crs1, crs2)
}

CPL_crs_from_input <- function(input) {
    .Call('_sf_CPL_crs_from_input', PACKAGE = 'sf', input)
}

CPL_roundtrip <- function(sfc) {
    .Call('_sf_CPL_roundtrip', PACKAGE = 'sf', sfc)
}

CPL_circularstring_to_linestring <- function(sfc) {
    .Call('_sf_CPL_circularstring_to_linestring', PACKAGE = 'sf', sfc)
}

CPL_multisurface_to_multipolygon <- function(sfc) {
    .Call('_sf_CPL_multisurface_to_multipolygon', PACKAGE = 'sf', sfc)
}

CPL_compoundcurve_to_linear <- function(sfc) {
    .Call('_sf_CPL_compoundcurve_to_linear', PACKAGE = 'sf', sfc)
}

CPL_curve_to_linestring <- function(sfc) {
    .Call('_sf_CPL_curve_to_linestring', PACKAGE = 'sf', sfc)
}

CPL_transform <- function(sfc, crs, AOI, pipeline, reverse = FALSE) {
    .Call('_sf_CPL_transform', PACKAGE = 'sf', sfc, crs, AOI, pipeline, reverse)
}

CPL_wrap_dateline <- function(sfc, opt, quiet = TRUE) {
    .Call('_sf_CPL_wrap_dateline', PACKAGE = 'sf', sfc, opt, quiet)
}

CPL_get_rgdal_drivers <- function(dummy) {
    .Call('_sf_CPL_get_rgdal_drivers', PACKAGE = 'sf', dummy)
}

CPL_sfc_from_wkt <- function(wkt) {
    .Call('_sf_CPL_sfc_from_wkt', PACKAGE = 'sf', wkt)
}

CPL_gdal_with_geos <- function() {
    .Call('_sf_CPL_gdal_with_geos', PACKAGE = 'sf')
}

CPL_axis_order_authority_compliant <- function(authority_compliant) {
    .Call('_sf_CPL_axis_order_authority_compliant', PACKAGE = 'sf', authority_compliant)
}

CPL_area <- function(sfc) {
    .Call('_sf_CPL_area', PACKAGE = 'sf', sfc)
}

CPL_gdal_dimension <- function(sfc, NA_if_empty = TRUE) {
    .Call('_sf_CPL_gdal_dimension', PACKAGE = 'sf', sfc, NA_if_empty)
}

CPL_length <- function(sfc) {
    .Call('_sf_CPL_length', PACKAGE = 'sf', sfc)
}

CPL_gdal_segmentize <- function(sfc, dfMaxLength = 0.0) {
    .Call('_sf_CPL_gdal_segmentize', PACKAGE = 'sf', sfc, dfMaxLength)
}

CPL_gdal_linestring_sample <- function(sfc, distLst) {
    .Call('_sf_CPL_gdal_linestring_sample', PACKAGE = 'sf', sfc, distLst)
}

CPL_get_layers <- function(datasource, options, do_count = FALSE) {
    .Call('_sf_CPL_get_layers', PACKAGE = 'sf', datasource, options, do_count)
}

CPL_read_ogr <- function(datasource, layer, query, options, quiet, toTypeUser, fid_column_name, drivers, wkt_filter, promote_to_multi = TRUE, int64_as_string = FALSE, dsn_exists = TRUE, dsn_isdb = FALSE) {
    .Call('_sf_CPL_read_ogr', PACKAGE = 'sf', datasource, layer, query, options, quiet, toTypeUser, fid_column_name, drivers, wkt_filter, promote_to_multi, int64_as_string, dsn_exists, dsn_isdb)
}

CPL_gdalinfo <- function(obj, options, oo) {
    .Call('_sf_CPL_gdalinfo', PACKAGE = 'sf', obj, options, oo)
}

CPL_gdalwarp <- function(src, dst, options, oo, doo) {
    .Call('_sf_CPL_gdalwarp', PACKAGE = 'sf', src, dst, options, oo, doo)
}

CPL_gdalrasterize <- function(src, dst, options, oo, doo, overwrite = FALSE) {
    .Call('_sf_CPL_gdalrasterize', PACKAGE = 'sf', src, dst, options, oo, doo, overwrite)
}

CPL_gdaltranslate <- function(src, dst, options, oo) {
    .Call('_sf_CPL_gdaltranslate', PACKAGE = 'sf', src, dst, options, oo)
}

CPL_gdalvectortranslate <- function(src, dst, options, oo, doo) {
    .Call('_sf_CPL_gdalvectortranslate', PACKAGE = 'sf', src, dst, options, oo, doo)
}

CPL_gdalbuildvrt <- function(src, dst, options, oo) {
    .Call('_sf_CPL_gdalbuildvrt', PACKAGE = 'sf', src, dst, options, oo)
}

CPL_gdaldemprocessing <- function(src, dst, options, processing, colorfilename, oo) {
    .Call('_sf_CPL_gdaldemprocessing', PACKAGE = 'sf', src, dst, options, processing, colorfilename, oo)
}

CPL_gdalnearblack <- function(src, dst, options, oo, doo) {
    .Call('_sf_CPL_gdalnearblack', PACKAGE = 'sf', src, dst, options, oo, doo)
}

CPL_gdalgrid <- function(src, dst, options, oo) {
    .Call('_sf_CPL_gdalgrid', PACKAGE = 'sf', src, dst, options, oo)
}

CPL_gdal_warper <- function(infile, outfile, options, oo, doo) {
    .Call('_sf_CPL_gdal_warper', PACKAGE = 'sf', infile, outfile, options, oo, doo)
}

CPL_write_ogr <- function(obj, dsn, layer, driver, dco, lco, geom, dim, fids, quiet = FALSE, update = FALSE, delete_dsn = FALSE, delete_layer = FALSE) {
    .Call('_sf_CPL_write_ogr', PACKAGE = 'sf', obj, dsn, layer, driver, dco, lco, geom, dim, fids, quiet, update, delete_dsn, delete_layer)
}

CPL_geos_binop <- function(sfc0, sfc1, op, par = 0.0, pattern = "", prepared = FALSE) {
    .Call('_sf_CPL_geos_binop', PACKAGE = 'sf', sfc0, sfc1, op, par, pattern, prepared)
}

CPL_geos_is_valid_reason <- function(sfc) {
    .Call('_sf_CPL_geos_is_valid_reason', PACKAGE = 'sf', sfc)
}

CPL_geos_is_valid <- function(sfc, NA_on_exception = TRUE) {
    .Call('_sf_CPL_geos_is_valid', PACKAGE = 'sf', sfc, NA_on_exception)
}

CPL_geos_is_simple <- function(sfc) {
    .Call('_sf_CPL_geos_is_simple', PACKAGE = 'sf', sfc)
}

CPL_geos_is_empty <- function(sfc) {
    .Call('_sf_CPL_geos_is_empty', PACKAGE = 'sf', sfc)
}

CPL_geos_normalize <- function(sfc) {
    .Call('_sf_CPL_geos_normalize', PACKAGE = 'sf', sfc)
}

CPL_geos_union <- function(sfc, by_feature = FALSE) {
    .Call('_sf_CPL_geos_union', PACKAGE = 'sf', sfc, by_feature)
}

CPL_geos_snap <- function(sfc0, sfc1, tolerance) {
    .Call('_sf_CPL_geos_snap', PACKAGE = 'sf', sfc0, sfc1, tolerance)
}

CPL_geos_op <- function(op, sfc, bufferDist, nQuadSegs, dTolerance, preserveTopology, bOnlyEdges = 1L, endCapStyle = 0L, joinStyle = 0L, mitreLimit = 1L) {
    .Call('_sf_CPL_geos_op', PACKAGE = 'sf', op, sfc, bufferDist, nQuadSegs, dTolerance, preserveTopology, bOnlyEdges, endCapStyle, joinStyle, mitreLimit)
}

CPL_geos_voronoi <- function(sfc, env, dTolerance = 0.0, bOnlyEdges = 1L) {
    .Call('_sf_CPL_geos_voronoi', PACKAGE = 'sf', sfc, env, dTolerance, bOnlyEdges)
}

CPL_geos_op2 <- function(op, sfcx, sfcy) {
    .Call('_sf_CPL_geos_op2', PACKAGE = 'sf', op, sfcx, sfcy)
}

CPL_geos_version <- function(runtime = FALSE, capi = FALSE) {
    .Call('_sf_CPL_geos_version', PACKAGE = 'sf', runtime, capi)
}

CPL_geos_dist <- function(sfc0, sfc1, which, par) {
    .Call('_sf_CPL_geos_dist', PACKAGE = 'sf', sfc0, sfc1, which, par)
}

CPL_geos_nearest_feature <- function(sfc0, sfc1) {
    .Call('_sf_CPL_geos_nearest_feature', PACKAGE = 'sf', sfc0, sfc1)
}

CPL_geos_nearest_points <- function(sfc0, sfc1, pairwise) {
    .Call('_sf_CPL_geos_nearest_points', PACKAGE = 'sf', sfc0, sfc1, pairwise)
}

CPL_transpose_sparse_incidence <- function(m, n) {
    .Call('_sf_CPL_transpose_sparse_incidence', PACKAGE = 'sf', m, n)
}

CPL_nary_difference <- function(sfc) {
    .Call('_sf_CPL_nary_difference', PACKAGE = 'sf', sfc)
}

CPL_nary_intersection <- function(sfc) {
    .Call('_sf_CPL_nary_intersection', PACKAGE = 'sf', sfc)
}

CPL_hex_to_raw <- function(cx) {
    .Call('_sf_CPL_hex_to_raw', PACKAGE = 'sf', cx)
}

CPL_raw_to_hex <- function(raw) {
    .Call('_sf_CPL_raw_to_hex', PACKAGE = 'sf', raw)
}

opp_sfc <- function(geom, value, mult, crs) {
    .Call('_sf_opp_sfc', PACKAGE = 'sf', geom, value, mult, crs)
}

normalize_sfc <- function(geom, min, range, crs) {
    .Call('_sf_normalize_sfc', PACKAGE = 'sf', geom, min, range, crs)
}

CPL_polygonize <- function(raster, mask_name, raster_driver, vector_driver, vector_dsn, options, iPixValField, contour_options, use_contours = FALSE, use_integer = TRUE) {
    .Call('_sf_CPL_polygonize', PACKAGE = 'sf', raster, mask_name, raster_driver, vector_driver, vector_dsn, options, iPixValField, contour_options, use_contours, use_integer)
}

CPL_rasterize <- function(raster, raster_driver, sfc, values, options, NA_value) {
    .Call('_sf_CPL_rasterize', PACKAGE = 'sf', raster, raster_driver, sfc, values, options, NA_value)
}

CPL_proj_h <- function(b = FALSE) {
    .Call('_sf_CPL_proj_h', PACKAGE = 'sf', b)
}

CPL_set_data_dir <- function(data_dir) {
    .Call('_sf_CPL_set_data_dir', PACKAGE = 'sf', data_dir)
}

CPL_use_proj4_init_rules <- function(v) {
    .Call('_sf_CPL_use_proj4_init_rules', PACKAGE = 'sf', v)
}

CPL_proj_version <- function(b = FALSE) {
    .Call('_sf_CPL_proj_version', PACKAGE = 'sf', b)
}

CPL_proj_is_valid <- function(proj4string) {
    .Call('_sf_CPL_proj_is_valid', PACKAGE = 'sf', proj4string)
}

CPL_have_datum_files <- function(foo) {
    .Call('_sf_CPL_have_datum_files', PACKAGE = 'sf', foo)
}

CPL_proj_direct <- function(from_to, pts, keep, warn = TRUE) {
    .Call('_sf_CPL_proj_direct', PACKAGE = 'sf', from_to, pts, keep, warn)
}

CPL_proj_info <- function(type) {
    .Call('_sf_CPL_proj_info', PACKAGE = 'sf', type)
}

CPL_xy2sfc <- function(cc, dim, to_points, which) {
    .Call('_sf_CPL_xy2sfc', PACKAGE = 'sf', cc, dim, to_points, which)
}

points_cpp <- function(pts, gdim = "XY") {
    .Call('_sf_points_cpp', PACKAGE = 'sf', pts, gdim)
}

CPL_signed_area <- function(pts) {
    .Call('_sf_CPL_signed_area', PACKAGE = 'sf', pts)
}

CPL_get_metadata <- function(obj, domain_item, options) {
    .Call('_sf_CPL_get_metadata', PACKAGE = 'sf', obj, domain_item, options)
}

CPL_get_crs <- function(obj, options) {
    .Call('_sf_CPL_get_crs', PACKAGE = 'sf', obj, options)
}

CPL_inv_geotransform <- function(gt_r) {
    .Call('_sf_CPL_inv_geotransform', PACKAGE = 'sf', gt_r)
}

CPL_read_gdal <- function(fname, options, driver, read_data, NA_value, RasterIO_parameters) {
    .Call('_sf_CPL_read_gdal', PACKAGE = 'sf', fname, options, driver, read_data, NA_value, RasterIO_parameters)
}

CPL_write_gdal <- function(x, fname, driver, options, Type, dims, from, gt, p4s, na_val, create = TRUE, only_create = FALSE) {
    invisible(.Call('_sf_CPL_write_gdal', PACKAGE = 'sf', x, fname, driver, options, Type, dims, from, gt, p4s, na_val, create, only_create))
}

CPL_read_wkb <- function(wkb_list, EWKB = FALSE, spatialite = FALSE) {
    .Call('_sf_CPL_read_wkb', PACKAGE = 'sf', wkb_list, EWKB, spatialite)
}

CPL_write_wkb <- function(sfc, EWKB = FALSE) {
    .Call('_sf_CPL_write_wkb', PACKAGE = 'sf', sfc, EWKB)
}

CPL_get_z_range <- function(sf, depth = 0L) {
    .Call('_sf_CPL_get_z_range', PACKAGE = 'sf', sf, depth)
}

CPL_get_m_range <- function(sf, depth = 0L) {
    .Call('_sf_CPL_get_m_range', PACKAGE = 'sf', sf, depth)
}

# Register entry points for exported C++ functions
methods::setLoadAction(function(ns) {
    .Call('_sf_RcppExport_registerCCallable', PACKAGE = 'sf')
})