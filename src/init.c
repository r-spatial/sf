#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP sf_CPL_area(SEXP);
extern SEXP sf_CPL_crs_from_epsg(SEXP);
extern SEXP sf_CPL_crs_from_proj4string(SEXP);
extern SEXP sf_CPL_crs_from_wkt(SEXP);
extern SEXP sf_CPL_crs_parameters(SEXP);
extern SEXP sf_CPL_gdal_cleanup_all();
extern SEXP sf_CPL_gdal_dimension(SEXP, SEXP);
extern SEXP sf_CPL_gdal_init();
extern SEXP sf_CPL_gdal_linestring_sample(SEXP, SEXP);
extern SEXP sf_CPL_gdal_segmentize(SEXP, SEXP);
extern SEXP sf_CPL_gdal_version(SEXP);
extern SEXP sf_CPL_geos_binop(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP sf_CPL_geos_dist(SEXP, SEXP);
extern SEXP sf_CPL_geos_is_simple(SEXP);
extern SEXP sf_CPL_geos_is_valid(SEXP, SEXP);
extern SEXP sf_CPL_geos_is_valid_reason(SEXP);
extern SEXP sf_CPL_geos_op(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP sf_CPL_geos_op2(SEXP, SEXP, SEXP);
extern SEXP sf_CPL_geos_relate(SEXP, SEXP);
extern SEXP sf_CPL_geos_union(SEXP, SEXP);
extern SEXP sf_CPL_geos_version(SEXP);
extern SEXP sf_CPL_geos_voronoi(SEXP, SEXP, SEXP, SEXP);
extern SEXP sf_CPL_get_bbox(SEXP, SEXP);
extern SEXP sf_CPL_get_layers(SEXP, SEXP, SEXP);
extern SEXP sf_CPL_get_rgdal_drivers(SEXP);
extern SEXP sf_CPL_hex_to_raw(SEXP);
extern SEXP sf_CPL_invert_sparse_incidence(SEXP, SEXP);
extern SEXP sf_CPL_length(SEXP);
extern SEXP sf_CPL_lwgeom_version(SEXP);
extern SEXP sf_CPL_make_valid(SEXP);
extern SEXP sf_CPL_proj_info(SEXP);
extern SEXP sf_CPL_proj_is_valid(SEXP);
extern SEXP sf_CPL_proj_version(SEXP);
extern SEXP sf_CPL_raw_to_hex(SEXP);
extern SEXP sf_CPL_read_ogr(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP sf_CPL_read_wkb(SEXP, SEXP, SEXP, SEXP);
extern SEXP sf_CPL_roundtrip(SEXP);
extern SEXP sf_CPL_sfc_from_wkt(SEXP);
extern SEXP sf_CPL_transform(SEXP, SEXP);
extern SEXP sf_CPL_wrap_dateline(SEXP, SEXP, SEXP);
extern SEXP sf_CPL_write_ogr(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP sf_CPL_write_wkb(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"sf_CPL_area",                    (DL_FUNC) &sf_CPL_area,                     1},
    {"sf_CPL_crs_from_epsg",           (DL_FUNC) &sf_CPL_crs_from_epsg,            1},
    {"sf_CPL_crs_from_proj4string",    (DL_FUNC) &sf_CPL_crs_from_proj4string,     1},
    {"sf_CPL_crs_from_wkt",            (DL_FUNC) &sf_CPL_crs_from_wkt,             1},
    {"sf_CPL_crs_parameters",          (DL_FUNC) &sf_CPL_crs_parameters,           1},
    {"sf_CPL_gdal_cleanup_all",        (DL_FUNC) &sf_CPL_gdal_cleanup_all,         0},
    {"sf_CPL_gdal_dimension",          (DL_FUNC) &sf_CPL_gdal_dimension,           2},
    {"sf_CPL_gdal_init",               (DL_FUNC) &sf_CPL_gdal_init,                0},
    {"sf_CPL_gdal_linestring_sample",  (DL_FUNC) &sf_CPL_gdal_linestring_sample,   2},
    {"sf_CPL_gdal_segmentize",         (DL_FUNC) &sf_CPL_gdal_segmentize,          2},
    {"sf_CPL_gdal_version",            (DL_FUNC) &sf_CPL_gdal_version,             1},
    {"sf_CPL_geos_binop",              (DL_FUNC) &sf_CPL_geos_binop,               7},
    {"sf_CPL_geos_dist",               (DL_FUNC) &sf_CPL_geos_dist,                2},
    {"sf_CPL_geos_is_simple",          (DL_FUNC) &sf_CPL_geos_is_simple,           1},
    {"sf_CPL_geos_is_valid",           (DL_FUNC) &sf_CPL_geos_is_valid,            2},
    {"sf_CPL_geos_is_valid_reason",    (DL_FUNC) &sf_CPL_geos_is_valid_reason,     1},
    {"sf_CPL_geos_op",                 (DL_FUNC) &sf_CPL_geos_op,                  8},
    {"sf_CPL_geos_op2",                (DL_FUNC) &sf_CPL_geos_op2,                 3},
    {"sf_CPL_geos_relate",             (DL_FUNC) &sf_CPL_geos_relate,              2},
    {"sf_CPL_geos_union",              (DL_FUNC) &sf_CPL_geos_union,               2},
    {"sf_CPL_geos_version",            (DL_FUNC) &sf_CPL_geos_version,             1},
    {"sf_CPL_geos_voronoi",            (DL_FUNC) &sf_CPL_geos_voronoi,             4},
    {"sf_CPL_get_bbox",                (DL_FUNC) &sf_CPL_get_bbox,                 2},
    {"sf_CPL_get_layers",              (DL_FUNC) &sf_CPL_get_layers,               3},
    {"sf_CPL_get_rgdal_drivers",       (DL_FUNC) &sf_CPL_get_rgdal_drivers,        1},
    {"sf_CPL_hex_to_raw",              (DL_FUNC) &sf_CPL_hex_to_raw,               1},
    {"sf_CPL_invert_sparse_incidence", (DL_FUNC) &sf_CPL_invert_sparse_incidence,  2},
    {"sf_CPL_length",                  (DL_FUNC) &sf_CPL_length,                   1},
    {"sf_CPL_lwgeom_version",          (DL_FUNC) &sf_CPL_lwgeom_version,           1},
    {"sf_CPL_make_valid",              (DL_FUNC) &sf_CPL_make_valid,               1},
    {"sf_CPL_proj_info",               (DL_FUNC) &sf_CPL_proj_info,                1},
    {"sf_CPL_proj_is_valid",           (DL_FUNC) &sf_CPL_proj_is_valid,            1},
    {"sf_CPL_proj_version",            (DL_FUNC) &sf_CPL_proj_version,             1},
    {"sf_CPL_raw_to_hex",              (DL_FUNC) &sf_CPL_raw_to_hex,               1},
    {"sf_CPL_read_ogr",                (DL_FUNC) &sf_CPL_read_ogr,                 7},
    {"sf_CPL_read_wkb",                (DL_FUNC) &sf_CPL_read_wkb,                 4},
    {"sf_CPL_roundtrip",               (DL_FUNC) &sf_CPL_roundtrip,                1},
    {"sf_CPL_sfc_from_wkt",            (DL_FUNC) &sf_CPL_sfc_from_wkt,             1},
    {"sf_CPL_transform",               (DL_FUNC) &sf_CPL_transform,                2},
    {"sf_CPL_wrap_dateline",           (DL_FUNC) &sf_CPL_wrap_dateline,            3},
    {"sf_CPL_write_ogr",               (DL_FUNC) &sf_CPL_write_ogr,               12},
    {"sf_CPL_write_wkb",               (DL_FUNC) &sf_CPL_write_wkb,                5},
    {NULL, NULL, 0}
};

void R_init_sf(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
