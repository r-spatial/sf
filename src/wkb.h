/*      NULL/EMPTY             0 */
#define SF_Unknown             0 /* sfc_GEOMETRY */
#define SF_Point               1
#define SF_LineString          2
#define SF_Polygon             3
#define SF_MultiPoint          4
#define SF_MultiLineString     5
#define SF_MultiPolygon        6
#define SF_GeometryCollection  7
#define SF_CircularString      8
#define SF_CompoundCurve       9
#define SF_CurvePolygon       10
#define SF_MultiCurve         11
#define SF_MultiSurface       12
#define SF_Curve              13
#define SF_Surface            14
#define SF_PolyhedralSurface  15
#define SF_TIN                16
#define SF_Triangle           17
#define SF_Type_Max           17

Rcpp::List CPL_read_wkb(Rcpp::List wkb_list, bool EWKB, bool spatialite, bool promote_multi);
Rcpp::List CPL_write_wkb(Rcpp::List sfc, bool EWKB);
unsigned int make_type(const char *cls, const char *dim, bool EWKB, int *tp, int srid);
Rcpp::List get_dim_sfc(Rcpp::List sfc);
int srid_from_crs(Rcpp::List crs);
