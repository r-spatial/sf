# Changelog

## version 1.1-1

- add
  [`gdal_compressors()`](https://r-spatial.github.io/sf/reference/gdal_compressors.md)
  to query GDAL compressor and decomporessor capability

## version 1.1-0

- [`st_cast.sfc()`](https://r-spatial.github.io/sf/reference/st_cast.md)
  deals with zero-length objects;
  [\#2584](https://github.com/r-spatial/sf/issues/2584)

- rewrite/migrate `vctrs` methods for `sf` and `sfc`;
  [\#2568](https://github.com/r-spatial/sf/issues/2568),
  [\#2584](https://github.com/r-spatial/sf/issues/2584), w. help from
  [@DavisVaughan](https://github.com/DavisVaughan)

- [`st_agr()`](https://r-spatial.github.io/sf/reference/st_agr.md) and
  `st_agr<-()` better handle multiple geometry columns

- for an `sfc` object `x`, `x[0]` retains the class of `x`;
  [\#2568](https://github.com/r-spatial/sf/issues/2568)

- When sampling a degenerate (zero length) line, a warning is raised
  rather than a message;
  [\#2575](https://github.com/r-spatial/sf/issues/2575)

## version 1.0-24

CRAN release: 2026-01-12

- [`gdal_write()`](https://r-spatial.github.io/sf/reference/gdal.md)
  handles drivers that only have a `CreateCopy()` option;
  <https://github.com/r-spatial/stars/issues/762>

- if `datum` is missing in a call to
  [`st_graticule()`](https://r-spatial.github.io/sf/reference/st_graticule.md),
  a graticule by default will try to use the geographic coordinate
  reference system of arguments `x` or `crs`; when nothing is found
  there it falls back to `OGC:CRS84` (WGS84).

- the figure margins parameter `mar` can be specified in a call to
  [`plot.sf()`](https://r-spatial.github.io/sf/reference/plot.md);
  [\#2558](https://github.com/r-spatial/sf/issues/2558)

- fix class label setting in `[.sf()`;
  [\#2557](https://github.com/r-spatial/sf/issues/2557)

## version 1.0-23

CRAN release: 2025-11-28

- allow tests reading blosc compressed Zarr files to fail

- [`st_as_sf.data.frame()`](https://r-spatial.github.io/sf/reference/st_as_sf.md)
  sets `z_range` and `m_range` if needed;
  <https://github.com/geoarrow/geoarrow-r/issues/75>

## version 1.0-22

CRAN release: 2025-11-10

- [`st_combine()`](https://r-spatial.github.io/sf/reference/geos_combine.md)
  on `POINT` geometries ignores empty points;
  [\#2551](https://github.com/r-spatial/sf/issues/2551)

- handle empty points better in
  [`st_point()`](https://r-spatial.github.io/sf/reference/st.md),
  [`st_as_sf.data.frame()`](https://r-spatial.github.io/sf/reference/st_as_sf.md)
  and
  [`st_distance()`](https://r-spatial.github.io/sf/reference/geos_measures.md);
  <https://github.com/r-spatial/s2/issues/289>

- for unprojected lines,
  [`st_line_interpolate()`](https://r-spatial.github.io/sf/reference/st_line_project_point.md)
  requires distance values with degree units;
  [\#2542](https://github.com/r-spatial/sf/issues/2542)

- `unique.sfc()` added;
  [\#2546](https://github.com/r-spatial/sf/issues/2546)

- for geodetic coordinates,
  [`st_perimeter()`](https://r-spatial.github.io/sf/reference/geos_measures.md)
  uses ellipsoidal computation if
  [`sf_use_s2()`](https://r-spatial.github.io/sf/reference/s2.md) is
  `FALSE`; [\#2541](https://github.com/r-spatial/sf/issues/2541)

- `st_as_sf.owin()` and `st_as_sfc.owin()` no longer ignore `crs`
  argument; [\#2532](https://github.com/r-spatial/sf/issues/2532)

- clarify approximation errors in
  [`st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  and how they differ for the GEOS or S2 backends, with examples by
  David Kaplan [@dmkaplan2000](https://github.com/dmkaplan2000);
  [\#2528](https://github.com/r-spatial/sf/issues/2528)

## version 1.0-21

CRAN release: 2025-05-15

- `st_crs(..., parameters = TRUE)` returns base geographic CRS as
  `gcs_crs`; [\#2524](https://github.com/r-spatial/sf/issues/2524)

- loading `sf` no longer initializes the RNG state; see
  <https://github.com/r-quantities/units/issues/409>

- fix
  [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md)
  on geodetic coordinates;
  [\#2515](https://github.com/r-spatial/sf/issues/2515)

- use [`compareVersion()`](https://rdrr.io/r/utils/compareVersion.html)
  consistently to compare GDAL versions;
  [\#2512](https://github.com/r-spatial/sf/issues/2512)

## version 1.0-20

CRAN release: 2025-03-24

- [`st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  on geodetic coordinates with negative buffer distance now
  automatically switches to using GEOS, while giving a warning;
  [\#1987](https://github.com/r-spatial/sf/issues/1987)

- [`st_interpolate_aw()`](https://r-spatial.github.io/sf/reference/interpolate_aw.md)
  fixes bug when a GEOMETRYCOLLECTION contains multiple POLYGON
  structures; found by [@mtennekes](https://github.com/mtennekes)

- [`st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  for geodetic coordinates allows `max_dist` and `min_level` to be
  specified by feature;
  [\#2488](https://github.com/r-spatial/sf/issues/2488) and
  <https://github.com/r-spatial/s2/pull/264>

- [`distinct.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  allows for comparing exact equality of geometries when `exact = TRUE`;
  [\#2484](https://github.com/r-spatial/sf/issues/2484)

- [`st_minimum_bounding_circle()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  returns geometries representing the smallest circle that contains the
  input; [\#2473](https://github.com/r-spatial/sf/issues/2473)

## version 1.0-19

CRAN release: 2024-11-05

- fix type checks in C++ GDAL area and length computation functions,
  anticipating GDAL 3.10.0;
  [\#2466](https://github.com/r-spatial/sf/issues/2466),
  [\#2468](https://github.com/r-spatial/sf/issues/2468),
  [\#2469](https://github.com/r-spatial/sf/issues/2469) by
  [@rsbivand](https://github.com/rsbivand) and
  [@rouault](https://github.com/rouault)

- improve test on empty geometries, which changed in 1.0-18;
  [\#2463](https://github.com/r-spatial/sf/issues/2463)

- [`gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.md)
  `ogrinfo` has an argument `read_only` which, when `TRUE` (or `options`
  includes `"-ro"`), opens a datasource in read-only mode
  ([\#2460](https://github.com/r-spatial/sf/issues/2460); `sf` did this
  before 1.0-17); by default a datasource is opened in update
  (read-write) mode (since sf 1.0-17;
  [\#2420](https://github.com/r-spatial/sf/issues/2420))

- the `sf` -\> `ppp` conversion `as.ppp.sf()` accepts a data.frame of
  marks instead of just 1 column
  [\#2450](https://github.com/r-spatial/sf/issues/2450), by
  [@agila5](https://github.com/agila5)

- add flag for preservation of point order in `st_voronoi`
  [\#1371](https://github.com/r-spatial/sf/issues/1371) for GEOS \>=
  3.12

## version 1.0-18

CRAN release: 2024-10-11

- support `POLYGON FULL` simple feature geometry, representing the
  entire Earth surface, as used by `s2geometry`; see also
  <https://r-spatial.org/r/2024/10/11/polygonfull.html> for a longer
  introduction; [\#2441](https://github.com/r-spatial/sf/issues/2441)

- [`st_sfc()`](https://r-spatial.github.io/sf/reference/sfc.md) has an
  argument `oriented` which, when set to `TRUE`, adds an attribute
  `oriented=TRUE` to an `sfc` object, indicating that this object should
  not be reoriented in conversion to `s2_geography` (avoiding using the
  global option `s2_oriented`);
  [`st_as_sfc.bbox()`](https://r-spatial.github.io/sf/reference/st_as_sfc.md)
  sets this to `TRUE`;
  [\#2441](https://github.com/r-spatial/sf/issues/2441)

- fix build failure with GDAL \< 3.4.0
  [\#2436](https://github.com/r-spatial/sf/issues/2436)

- [`st_simplify()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  now accepts feature-wise tolerance values when `s2` is switched on
  [\#2442](https://github.com/r-spatial/sf/issues/2442)

## version 1.0-17

CRAN release: 2024-09-06

- add
  [`st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.md)
  method for `bbox` objects; this uses
  OGRCoordinateTransformation::TransformBounds(), densifying first and
  antemeridian proof;
  [\#2415](https://github.com/r-spatial/sf/issues/2415)

- [`st_filter.sf()`](https://r-spatial.github.io/sf/reference/st_join.md)
  correctly scopes `x` and `y` arguments using !! operator;
  [\#2416](https://github.com/r-spatial/sf/issues/2416)

- `[.sfc` and `[<-.sfc` use matrix/array type subsetting for `sfc`
  objects that have a `dim` attribute

- add
  [`st_exterior_ring()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  to extract exterior rings (remove holes);
  [\#2406](https://github.com/r-spatial/sf/issues/2406)

- add [`text.sf()`](https://r-spatial.github.io/sf/reference/plot.md),
  [`text.sfc()`](https://r-spatial.github.io/sf/reference/plot.md),
  [`points.sf()`](https://r-spatial.github.io/sf/reference/plot.md),
  [`points.sfc()`](https://r-spatial.github.io/sf/reference/plot.md) to
  annotate base plots at geometry centroids;
  [\#2399](https://github.com/r-spatial/sf/issues/2399)

- [`st_sf()`](https://r-spatial.github.io/sf/reference/sf.md) no longer
  strips `tbl` or `tbl_df` class labels;
  [\#2378](https://github.com/r-spatial/sf/issues/2378)

- [`st_layers()`](https://r-spatial.github.io/sf/reference/st_layers.md)
  returns an object of class `c("sf_layers", "data.frame")`, with a
  dedicated `print` method.

- when `dim` is not `XYZM`, `sf_as_sf.data.frame()` interprets a length
  4 `coords` argument to specify the corners of a rectangular polygon;
  [\#2357](https://github.com/r-spatial/sf/issues/2357)

- [`st_interpolate_aw()`](https://r-spatial.github.io/sf/reference/interpolate_aw.md)
  gains an `na.rm` argument, for removing features with `NA` attributes
  before interpolating;
  [\#830](https://github.com/r-spatial/sf/issues/830)

- [`merge.sf()`](https://r-spatial.github.io/sf/reference/merge.sf.md)
  no longer renames geometry column;
  [\#2334](https://github.com/r-spatial/sf/issues/2334)

## version 1.0-16

CRAN release: 2024-03-24

- [`st_join()`](https://r-spatial.github.io/sf/reference/st_join.md) no
  longer renames the geometry column;
  [\#2327](https://github.com/r-spatial/sf/issues/2327)

- [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md)
  works when unprojected polygon geometry crosses the antemeridian;
  [\#2331](https://github.com/r-spatial/sf/issues/2331)

- clean up and modernization of S3 registration of methods and tests;
  [\#2285](https://github.com/r-spatial/sf/issues/2285),
  [\#2288](https://github.com/r-spatial/sf/issues/2288),
  [\#2316](https://github.com/r-spatial/sf/issues/2316),
  [\#2341](https://github.com/r-spatial/sf/issues/2341),
  [\#2342](https://github.com/r-spatial/sf/issues/2342), by
  [@olivroy](https://github.com/olivroy)

- `[.sfc` works when setting argument `op`;
  [\#2320](https://github.com/r-spatial/sf/issues/2320)

- [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md)
  for polygons is sensitive to setting `oriented = TRUE` to prevent
  wrongly correcting ring directions;
  [\#2308](https://github.com/r-spatial/sf/issues/2308)

- add support for the GDAL `footprint` utility (requiring GDAL \>=
  3.8.0) to `gdal_utils`;
  [\#2305](https://github.com/r-spatial/sf/issues/2305), by
  [@goergen95](https://github.com/goergen95)

- existing environment variables `PROJ_LIB` and `PROJ_DATA` are (again)
  ignored on `sf` binary CRAN installations (win + macos), effectively
  by overwriting them during the R session and restoring them on exit;
  this does not happen if environment variable `R_SF_USE_PROJ_DATA` is
  set to `true`. [\#2298](https://github.com/r-spatial/sf/issues/2298)

- add
  [`st_line_project()`](https://r-spatial.github.io/sf/reference/st_line_project_point.md)
  to find how far a point is when projected on a line;
  [\#2291](https://github.com/r-spatial/sf/issues/2291)

- add
  [`st_line_interpolate()`](https://r-spatial.github.io/sf/reference/st_line_project_point.md)
  to obtain a point at a certain distance along a line;
  [\#2291](https://github.com/r-spatial/sf/issues/2291)

## version 1.0-15

CRAN release: 2023-12-18

- add
  [`st_perimeter()`](https://r-spatial.github.io/sf/reference/geos_measures.md)
  generic to cover both geographic and projected coordinates;
  [\#268](https://github.com/r-spatial/sf/issues/268),
  [\#2279](https://github.com/r-spatial/sf/issues/2279), by
  [@JosiahParry](https://github.com/JosiahParry)

- add
  [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md)
  method for `bbox`, with special provisions for ellipsoidal
  coordinates; [\#2283](https://github.com/r-spatial/sf/issues/2283)

- documentation clean-up by [@olivroy](https://github.com/olivroy);
  [\#2266](https://github.com/r-spatial/sf/issues/2266),
  [\#2285](https://github.com/r-spatial/sf/issues/2285)

- [`st_convex_hull()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  uses
  [`s2::s2_convex_hull()`](https://r-spatial.github.io/s2/reference/s2_boundary.html)
  for geodetic coordinates;
  [\#2250](https://github.com/r-spatial/sf/issues/2250)

- add `directed` argument to
  [`st_line_merge()`](https://r-spatial.github.io/sf/reference/geos_unary.md);
  [\#2264](https://github.com/r-spatial/sf/issues/2264)

- `st_union.sfc()` given `x` and `y` works consistently across geodetic
  and projected objects;
  [\#2262](https://github.com/r-spatial/sf/issues/2262)

- `st_union.sf()` given `x` and `y` unions pairwise if
  `by_feature = TRUE`;
  [\#2259](https://github.com/r-spatial/sf/issues/2259)

- [`st_read()`](https://r-spatial.github.io/sf/reference/st_read.md)
  work around issue with GPKG driver if `wkt_filter` is set;
  [\#2248](https://github.com/r-spatial/sf/issues/2248)

- [`st_read()`](https://r-spatial.github.io/sf/reference/st_read.md)
  uses GDAL’s stream reading when `use_stream = TRUE`;
  [\#2238](https://github.com/r-spatial/sf/issues/2238) by
  [@paleolimbot](https://github.com/paleolimbot)

- [`st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.md)
  responds to in-session changes to
  [`sf_proj_network()`](https://r-spatial.github.io/sf/reference/proj_tools.md);
  [\#2166](https://github.com/r-spatial/sf/issues/2166)

- [`plot.sf()`](https://r-spatial.github.io/sf/reference/plot.md):
  `key.width` is sensitive to pointsize graphics parameter `par("ps")`;
  keys with factor levels suggest a proper size if they won’t fit.

- [`plot.sf()`](https://r-spatial.github.io/sf/reference/plot.md):
  `key.pos` can hold a second value in \[0, 1\] determining the relative
  position of the key in the available space

- `[<-.sf` fixes the `agr` attribute when it is broken;
  [\#2211](https://github.com/r-spatial/sf/issues/2211)

- if the env. variable `ADD_SF_NAMESPACE` is set to `true`, `sf` objects
  get a new attribute, `.sf_namespace`, which forces loading the `sf`
  namespace when it has not been loaded so far, e.g. for proper printing
  or plotting of an `sf` object;
  [\#2212](https://github.com/r-spatial/sf/issues/2212) by Mike Mahoney

- [`distinct.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  is type-safe for `sf` objects with zero rows;
  [\#2204](https://github.com/r-spatial/sf/issues/2204)

- [`summarise.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  raises an error if `.by` is given but no
  [`across()`](https://dplyr.tidyverse.org/reference/across.html) on the
  geometry; [\#2207](https://github.com/r-spatial/sf/issues/2207)

- [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  matches fields on name first, than on position; this matters for
  formats that have pre-defined names, such as GPX;
  [\#2202](https://github.com/r-spatial/sf/issues/2202)

## version 1.0-14

CRAN release: 2023-07-11

- fix [`plot.sf()`](https://r-spatial.github.io/sf/reference/plot.md)
  when using a key for multiple factor variables;
  [\#2196](https://github.com/r-spatial/sf/issues/2196),
  [\#2195](https://github.com/r-spatial/sf/issues/2195)

- fix use of `as.numeric_version` in a test, for upcoming change in
  r-devel

- code tidy-ing: fix many lintr suggestions, thanks to Michael Chirico
  ([\#2181](https://github.com/r-spatial/sf/issues/2181) -
  [\#2191](https://github.com/r-spatial/sf/issues/2191))

## version 1.0-13

CRAN release: 2023-05-24

- [`gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.md)
  adds `"ogrinfo"` utility (requires GDAL \>= 3.7.0);
  [\#2160](https://github.com/r-spatial/sf/issues/2160)

- [`st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.md)
  catches errors when setting invalid crs values, raised by Jon Skøien

- add
  [`rename_with.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  method; [\#1472](https://github.com/r-spatial/sf/issues/1472)

- use GEOS’ overlayNG routines for (GEOS) Intersection, Difference,
  Union and SymDifference;
  [\#2143](https://github.com/r-spatial/sf/issues/2143)

- added `duplicated.sf()`;
  [\#2138](https://github.com/r-spatial/sf/issues/2138),
  [\#2140](https://github.com/r-spatial/sf/issues/2140), thanks to
  [@bart1](https://github.com/bart1)

- [`select.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  allows selecting the same column twice under different names;
  [\#1886](https://github.com/r-spatial/sf/issues/1886)

- `st_as_sf.ppplist()` is deprecated;
  [\#1926](https://github.com/r-spatial/sf/issues/1926)

- [`st_cast()`](https://r-spatial.github.io/sf/reference/st_cast.md)
  handles empty geometries;
  [\#1961](https://github.com/r-spatial/sf/issues/1961)

- don’t repeat longlat messages in
  [`summarise.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md);
  [\#1519](https://github.com/r-spatial/sf/issues/1519)

- fix random sampling on the sphere;
  [\#2133](https://github.com/r-spatial/sf/issues/2133)

## version 1.0-12

CRAN release: 2023-03-19

- update NAMESPACE to `useDynLib(sf, .registration=TRUE)`;
  [\#2127](https://github.com/r-spatial/sf/issues/2127) thanks to
  [@eddelbuettel](https://github.com/eddelbuettel)

- fix call in
  [`gdal_addo()`](https://r-spatial.github.io/sf/reference/gdal_addo.md);
  [\#2124](https://github.com/r-spatial/sf/issues/2124)

- fix issues that came up with older GDAL version, use
  `GDAL_VERSION_NUM` consistently;
  [\#2123](https://github.com/r-spatial/sf/issues/2123)
  [\#2121](https://github.com/r-spatial/sf/issues/2121)
  [\#2119](https://github.com/r-spatial/sf/issues/2119)

## version 1.0-11

CRAN release: 2023-03-15

- [`st_make_grid()`](https://r-spatial.github.io/sf/reference/st_make_grid.md)
  also accepts area units for `cellsize`, for square and hexagonal
  grids; [\#1505](https://github.com/r-spatial/sf/issues/1505)

- add
  [`st_concave_hull()`](https://r-spatial.github.io/sf/reference/geos_unary.md),
  for concave hulls, if GEOS \>= 3.11.0;
  [\#1964](https://github.com/r-spatial/sf/issues/1964)

- add
  [`st_triangulate_constrained()`](https://r-spatial.github.io/sf/reference/geos_unary.md),
  for constrained Delaunay triangulation, if GEOS \>= 3.10.0;
  [\#1964](https://github.com/r-spatial/sf/issues/1964)

- clean up the retrieval of length or angle units from WKT
  representations;
  <https://lists.osgeo.org/pipermail/gdal-dev/2023-March/056994.html>

- conversion to GEOS uses the `GEOS_PREC_VALID_OUTPUT` flag, which makes
  sure that the “\[o\]utput is always valid. Collapsed geometry elements
  (including both polygons and lines) are removed.”

## version 1.0-10

CRAN release: 2023-03-12

- [`gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.md)
  has a `config_options` argument to set further GDAL options, just like
  [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md);
  [\#2003](https://github.com/r-spatial/sf/issues/2003)

- fix slow writing of logical vectors in
  [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md);
  [\#1409](https://github.com/r-spatial/sf/issues/1409);
  [\#1689](https://github.com/r-spatial/sf/issues/1689)

- [`st_drivers()`](https://r-spatial.github.io/sf/reference/st_drivers.md)
  has an argument `regex` to filter on driver (long) name;
  [\#2090](https://github.com/r-spatial/sf/issues/2090)

- drop C++11 as a system requirement

- `c.sfc()` (and, consequently,
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html))
  gives an error if components have different CRS;
  [\#1884](https://github.com/r-spatial/sf/issues/1884)

- data imported from `maps` are associated with the Clark 1866
  ellipsoid; [\#2080](https://github.com/r-spatial/sf/issues/2080)

- fix importing legacy `SpatialPolygon` objects without comments;
  [\#2063](https://github.com/r-spatial/sf/issues/2063),
  [\#2069](https://github.com/r-spatial/sf/issues/2069),
  <https://github.com/rstudio/leaflet/issues/833>

- [`st_read()`](https://r-spatial.github.io/sf/reference/st_read.md) no
  longer errors on mixes of `XY` and `XYZ` geometries;
  [\#2046](https://github.com/r-spatial/sf/issues/2046)
  [\#1592](https://github.com/r-spatial/sf/issues/1592)

- in [`plot.sf()`](https://r-spatial.github.io/sf/reference/plot.md),
  when numeric `breaks` are given a legend key is always plotted;
  [\#2065](https://github.com/r-spatial/sf/issues/2065)

- `st_crs()$axes` returns a `data.frame` with axes properties (name,
  orientation, conversion factor) when GDAL \>= 3.0.0

- clean up unit handling for geometry measures (length, area, distance)
  and crs;

- `st_crs(x)$ud_unit` returns `NULL` if units are unknown;
  [\#2049](https://github.com/r-spatial/sf/issues/2049)

- [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  substitutes an `NA` crs with
  `ENGCRS["Undefined Cartesian SRS with unknown unit"]`;
  [\#2049](https://github.com/r-spatial/sf/issues/2049),
  [\#2054](https://github.com/r-spatial/sf/issues/2054)

- [`st_can_transform()`](https://r-spatial.github.io/sf/reference/st_transform.md)
  checks whether a transformation between two crs exists; see
  <https://github.com/dieghernan/tidyterra/issues/64>;
  [\#2049](https://github.com/r-spatial/sf/issues/2049)

- the matrix returned by
  [`st_coordinates()`](https://r-spatial.github.io/sf/reference/st_coordinates.md)
  has no row names, to reduce output size

## version 1.0-9

CRAN release: 2022-11-08

- adjust for changes how R-devel handles `POSIXlt`;
  [\#2028](https://github.com/r-spatial/sf/issues/2028)

- add
  [`st_break_antimeridian()`](https://r-spatial.github.io/sf/reference/st_break_antimeridian.md);
  [\#1983](https://github.com/r-spatial/sf/issues/1983),
  [\#1991](https://github.com/r-spatial/sf/issues/1991) by Roger Bivand

- add `Fibonacci` as a spatial sampling type in
  [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md)

- use the global `options("sf_use_s2")` to determine whether to use s2,
  rather than a value in a local environment;
  [\#1977](https://github.com/r-spatial/sf/issues/1977)

- fix utils `mdiminfo` and `mdimtranslate` in
  [`gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.md)

- extend arguments of
  [`gdal_read_mdim()`](https://r-spatial.github.io/sf/reference/gdal.md)
  needed by
  [`stars::read_mdim()`](https://r-spatial.github.io/stars/reference/mdim.html)
  if `stars` \>= 0.5-7; add
  [`gdal_write_mdim()`](https://r-spatial.github.io/sf/reference/gdal.md)

- add [`drop_na()`](https://tidyr.tidyverse.org/reference/drop_na.html)
  method for `sf` objects;
  [\#1975](https://github.com/r-spatial/sf/issues/1975)

## version 1.0-8

CRAN release: 2022-07-14

- [`st_drop_geometry.default()`](https://r-spatial.github.io/sf/reference/st_geometry.md)
  returns `x` unmodified;

- [`sf_project()`](https://r-spatial.github.io/sf/reference/sf_project.md)
  accepts 3- or 4-column matrices, containing z and t values;

- optimization for
  [`st_sfc()`](https://r-spatial.github.io/sf/reference/sfc.md) by
  [@paleolimbot](https://github.com/paleolimbot);
  [\#1938](https://github.com/r-spatial/sf/issues/1938),
  [\#1925](https://github.com/r-spatial/sf/issues/1925)

- `[<-.sfc()` recomputes the bounding box;
  [`st_sfc()`](https://r-spatial.github.io/sf/reference/sfc.md) gets
  parameter `compute_bbox`;
  [\#1965](https://github.com/r-spatial/sf/issues/1965)

- add new algorithm and drop option to
  [`st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.md)
  when using GEOS and GEOS \>= 3.10.1;
  [\#1655](https://github.com/r-spatial/sf/issues/1655)

- add
  [`st_minimum_rotated_rectangle()`](https://r-spatial.github.io/sf/reference/geos_unary.md),
  available when GEOS \>= 3.9.0;
  [\#1953](https://github.com/r-spatial/sf/issues/1953)

- fix
  [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md)
  with `type = "hexagonal"` for corner case (n=1), add a `progress`
  argument for a progress bar;
  [\#1945](https://github.com/r-spatial/sf/issues/1945)

- add package `pbapply` to Suggests;
  [\#1945](https://github.com/r-spatial/sf/issues/1945)

- add pdf driver to windows build;
  [\#1942](https://github.com/r-spatial/sf/issues/1942)

- clarify `pipeline` argument in
  [`st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.md)
  when axis order is ambiguous;
  [\#1934](https://github.com/r-spatial/sf/issues/1934)

- handle argument `xpd` in calls to
  [`plot.sfc_POLYGON()`](https://r-spatial.github.io/sf/reference/plot.md)
  and
  [`plot.sfc_MULTIPOLYGON()`](https://r-spatial.github.io/sf/reference/plot.md)

- add
  [`pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
  method, by Henning Teickner;
  [\#1915](https://github.com/r-spatial/sf/issues/1915)

- add
  [`gdal_addo()`](https://r-spatial.github.io/sf/reference/gdal_addo.md)
  to add or remove overviews from raster images;
  [\#1921](https://github.com/r-spatial/sf/issues/1921)

- [`st_layers()`](https://r-spatial.github.io/sf/reference/st_layers.md)
  returns `crs` of each layer in a `crs` list of `crs` objects

- restore
  [`st_graticule()`](https://r-spatial.github.io/sf/reference/st_graticule.md)
  behaviour to pre-sf 1.0-0;
  <https://github.com/tidyverse/ggplot2/issues/4571>

- [`gdal_metadata()`](https://r-spatial.github.io/sf/reference/gdal.md)
  sets metadata item names properly

- [`st_read()`](https://r-spatial.github.io/sf/reference/st_read.md)
  gains an argument `optional` passed on to `as.data.frame` to avoid
  changing column names;
  [\#1916](https://github.com/r-spatial/sf/issues/1916)

- GPX files are autodetected by
  [`st_read()`](https://r-spatial.github.io/sf/reference/st_read.md);
  [\#1917](https://github.com/r-spatial/sf/issues/1917)

- unnecessary coordinate names are not returned in
  [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md),
  making the output size smaller;
  [\#1879](https://github.com/r-spatial/sf/issues/1879)

## version 1.0-7

CRAN release: 2022-03-07

- [`st_drop_geometry()`](https://r-spatial.github.io/sf/reference/st_geometry.md)
  is a generic; [\#1914](https://github.com/r-spatial/sf/issues/1914)

- `st_crs(x)$ud_unit` returns the unit of the coordinate reference
  system of `x`

- geometric predicates return `sgbp` objects omitting self-intersections
  etc. by passing `remove_self = TRUE` and unique symmetric relationship
  by passing `retain_unique = TRUE` (to `...` if needed); this
  simplifies identifying (and removing) duplicated geometries;
  duplicates are identified by e.g. by
  `st_equals(x, retain_unique = TRUE) |> unlist() |> unique()`;
  [\#1893](https://github.com/r-spatial/sf/issues/1893)

- fix compile issue against GDAL \< 2.5.0 introduced in 1.0-6;
  [\#1899](https://github.com/r-spatial/sf/issues/1899)

## version 1.0-6

CRAN release: 2022-02-04

- adapt to new `spatstat.random` package;
  [\#1892](https://github.com/r-spatial/sf/issues/1892)

- `st_geometry<-()` also allows to rename a geometry column in an `sf`
  object; [\#1890](https://github.com/r-spatial/sf/issues/1890)

- for `sf` objects, the
  [`st_as_sfc()`](https://r-spatial.github.io/sf/reference/st_as_sfc.md)
  method is an alias for
  [`st_geometry()`](https://r-spatial.github.io/sf/reference/st_geometry.md);
  [\#1882](https://github.com/r-spatial/sf/issues/1882)

- [`st_make_grid()`](https://r-spatial.github.io/sf/reference/st_make_grid.md)
  speeded up; [\#1579](https://github.com/r-spatial/sf/issues/1579)
  thanks to Krzysztof Dyba

- remove direct and indirect dependencies on `rgeos` and `rgdal`;
  [\#1869](https://github.com/r-spatial/sf/issues/1869)

- use [`stats::dist`](https://rdrr.io/r/stats/dist.html) rather than
  GEOS for symmetric point-point Euclidian distance computation;
  [\#1874](https://github.com/r-spatial/sf/issues/1874)

## version 1.0-5

CRAN release: 2021-12-17

- package startup message reports status of
  [`sf_use_s2()`](https://r-spatial.github.io/sf/reference/s2.md);
  [\#1782](https://github.com/r-spatial/sf/issues/1782)

- [`sf_use_s2()`](https://r-spatial.github.io/sf/reference/s2.md) uses
  [`message()`](https://rdrr.io/r/base/message.html) to report a change;
  [\#1782](https://github.com/r-spatial/sf/issues/1782)

- [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md)
  using regular sampling for ellipsoidal coordinates “works” as if
  coordinates were Cartesian;
  [\#1837](https://github.com/r-spatial/sf/issues/1837)

## version 1.0-4

CRAN release: 2021-11-14

- new function
  [`st_delete()`](https://r-spatial.github.io/sf/reference/st_write.md)
  deletes a data source, or layer(s) within a data source;
  [\#1828](https://github.com/r-spatial/sf/issues/1828)

- fix memory leak in `WKT1_ESRI` retrieval;
  [\#1690](https://github.com/r-spatial/sf/issues/1690)

## version 1.0-3

CRAN release: 2021-10-07

- cope with how GEOS \>= 3.10.0 handles illegal geometries (e.g.,
  non-closed rings);
  [\#1807](https://github.com/r-spatial/sf/issues/1807)

- `crs` objects have a `$srid` method to extract the SRID (as authority
  “name:code”); [\#1804](https://github.com/r-spatial/sf/issues/1804)

- [`st_as_grob()`](https://r-spatial.github.io/sf/reference/st_as_grob.md)
  methods for `sfc_*` objects correctly handle empty geometries;
  [\#1789](https://github.com/r-spatial/sf/issues/1789) with help from
  Hiroaki Yutani

- when writing objects with `NA` as CRS to GeoPackage, assign “Unknown
  Cartesian CRS” first - this is in line with using Cartesian geometry
  operations for objects with `NA` as CRS;
  [\#1776](https://github.com/r-spatial/sf/issues/1776)

- add coerce method from `sgbp` to `sparseMatrix`;
  [\#1750](https://github.com/r-spatial/sf/issues/1750)

- fix [`st_cast()`](https://r-spatial.github.io/sf/reference/st_cast.md)
  for `GEOMETRYCOLLECTIONS` containing empty geometries;
  [\#1767](https://github.com/r-spatial/sf/issues/1767)

- fix
  [`st_is_valid()`](https://r-spatial.github.io/sf/reference/valid.md)
  for bogus polygons and projected coordinates;
  [\#1666](https://github.com/r-spatial/sf/issues/1666),
  [\#1760](https://github.com/r-spatial/sf/issues/1760);
  [\#1761](https://github.com/r-spatial/sf/issues/1761)

## version 1.0-2

CRAN release: 2021-07-26

- [`st_read()`](https://r-spatial.github.io/sf/reference/st_read.md) and
  [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  using GDAL handle binary attributes (OFTBinary fields) ;
  [\#1721](https://github.com/r-spatial/sf/issues/1721)

- a `pivot_longer` method is added for `sf` objects (the `data.frame`
  method works, but raises a warning)

- `rbind.sf` preserves primary geometry column;
  [\#1717](https://github.com/r-spatial/sf/issues/1717)

- `configure` constrains using `--static` to `Darwin` platform;
  [\#1702](https://github.com/r-spatial/sf/issues/1702),
  [\#1712](https://github.com/r-spatial/sf/issues/1712),
  [\#1713](https://github.com/r-spatial/sf/issues/1713)

- old-style `crs` objects created with sf \< 0.9-0 generate a message,
  and will cause a warning in the future.

- when [`st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.md)
  is called with a WKT2 as text input, its `input` field will be
  replaced with the CRS name (if it has one).

- GEOS (\>= 3.9.0) operations use `GEOSGeom_setPrecision_r` to set
  precision of geometries;
  [\#1535](https://github.com/r-spatial/sf/issues/1535)

- [`st_read()`](https://r-spatial.github.io/sf/reference/st_read.md)
  with specified `query` ignores argument `layers`, and warns if it is
  given; [\#1444](https://github.com/r-spatial/sf/issues/1444)

## version 1.0-1

CRAN release: 2021-06-29

- fix regression in
  [`st_intersection()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.md):
  when using s2 attributes were assigned wrongly;
  [\#1704](https://github.com/r-spatial/sf/issues/1704)

- `crs` (sf) to `CRS` (sp) conversion no longer needs validation by
  `rgdal`; <https://github.com/edzer/sp/issues/107>

- retrieve ESRI’s WKT version of CRS by `st_crs(id)$WKT1_ESRI`;
  [\#1690](https://github.com/r-spatial/sf/issues/1690)

## version 1.0-0

CRAN release: 2021-06-09

- add `s2` to Imports:

- add Dewey Dunnington to contributors

- [`sf_use_s2()`](https://r-spatial.github.io/sf/reference/s2.md) prints
  a message when using s2 has been switched to on or off.

- use `s2` spherical geometry as default when coordinates are
  ellipsoidal. This can be switched off (defaulting to planar geometry,
  using GEOS, as in sf \< 1.0-0) by setting environment variable
  `_SF_USE_S2` to `false` before package `sf` is loaded, or by
  `sf_use_s2(FALSE)`;
  [\#1649](https://github.com/r-spatial/sf/issues/1649)

- [`st_nearest_feature()`](https://r-spatial.github.io/sf/reference/st_nearest_feature.md)
  with missing `y` returns nearest features in the remaining set of `x`;
  <https://github.com/r-spatial/s2/issues/111>

- [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  gains an argument `config_options` to set GDAL config options;
  [\#1618](https://github.com/r-spatial/sf/issues/1618)

- fix regression in `sf_project(..., keep = TRUE)`;
  [\#1635](https://github.com/r-spatial/sf/issues/1635)

## version 0.9-8

CRAN release: 2021-03-17

- add
  [`st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.md)
  method for terra’s `SpatVector` class;
  [\#1567](https://github.com/r-spatial/sf/issues/1567)

- [`distinct.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  works by default on all variables, and keeps active geometry active;
  [\#1613](https://github.com/r-spatial/sf/issues/1613)

- improve (fix?) polygonize/contour code;
  [\#1608](https://github.com/r-spatial/sf/issues/1608)

- [`sf_proj_network()`](https://r-spatial.github.io/sf/reference/proj_tools.md)
  reports whether PROJ uses network (CDN) grids, can switch it on or
  off, and can set the CDN url.

- [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  returns obj, invisibly;
  [\#1597](https://github.com/r-spatial/sf/issues/1597)

- fix regression in n-ary
  [`st_intersection()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.md),
  [\#1595](https://github.com/r-spatial/sf/issues/1595), introduced at
  [\#1549](https://github.com/r-spatial/sf/issues/1549)

- [`st_inscribed_circle()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  computes the maximum inscribed circle for polygons (requires GEOS \>=
  3.9.0)

- allow to
  [`st_cast()`](https://r-spatial.github.io/sf/reference/st_cast.md)
  from COMPOUNDCURVE, MULTISURFACE or CURVEPOLYGON to
  GEOMETRYCOLLECTION, and back;
  [\#1573](https://github.com/r-spatial/sf/issues/1573)

- Fixed a bug in
  [`st_as_grob()`](https://r-spatial.github.io/sf/reference/st_as_grob.md)
  when plotting a mix of MULTI and non-MULTI geometries of the same base
  type

## version 0.9-7

CRAN release: 2021-01-06

- n-ary
  [`st_intersection()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.md)
  skips failing geometries, rather than returning an error;
  [\#1549](https://github.com/r-spatial/sf/issues/1549)

- use
  [`s2_centroid()`](https://r-spatial.github.io/s2/reference/s2_boundary.html)
  for geographical coordinates if
  [`sf_use_s2()`](https://r-spatial.github.io/sf/reference/s2.md) is
  `TRUE`.

- [`st_as_text()`](https://r-spatial.github.io/sf/reference/st_as_text.md)
  method for `crs` objects can return projjson (if GDAL \>= 3.1.0 and
  PROJ \> 6.2.0)

- [`st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.md)
  no longer warns on conversions like
  `"+proj=ob_tran +o_proj=longlat +o_lat_p=45 +o_lon_p=30"`

- `st_as_wkb()` takes `srid` from `wkt` field of `crs` when `input`
  field doesn’t contain it;
  [\#1490](https://github.com/r-spatial/sf/issues/1490)

- [`plot.sf()`](https://r-spatial.github.io/sf/reference/plot.md) adds
  `key.pos=0` option to run the logic behind the key without plotting
  it; [\#1487](https://github.com/r-spatial/sf/issues/1487)

- fix bug in
  [`select.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  when selected variables were renamed;
  [\#1483](https://github.com/r-spatial/sf/issues/1483)

- `st_as_sf.stars(..., merge = TRUE)` now works if CRS is `NA`;
  [\#1389](https://github.com/r-spatial/sf/issues/1389)

- add (dynamically loaded) `as_wkb()` methods for `sf`, `sfc` and `sfg`,
  making [`st_as_s2()`](https://r-spatial.github.io/sf/reference/s2.md)
  unnecessary

- [`st_as_s2()`](https://r-spatial.github.io/sf/reference/s2.md)
  transforms non-longlat objects to EPSG:4326 first

## version 0.9-6

CRAN release: 2020-09-13

- [`gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.md)
  print (GDAL-style) progress bar if `quiet = FALSE` (except for `info`
  and `mdiminfo`)

- fix `CPL_gdal_warper` for multi bands;
  <https://github.com/r-spatial/stars/issues/320>

- [`sf_proj_search_paths()`](https://r-spatial.github.io/sf/reference/proj_tools.md)
  retrieves and sets the proj search path (if GDAL \> 3.0.3)

- when loading sf,
  [`sf_use_s2()`](https://r-spatial.github.io/sf/reference/s2.md) is set
  to `FALSE` unless environment variable `_SF_USE_S2` equals `true`
  (this changes to `TRUE` in sf 1.0-0)

- resolve GDAL/PROJ version vulnerabilities in CRS-crs conversion;
  [\#1479](https://github.com/r-spatial/sf/issues/1479)

- [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md)
  gains an argument, `by_polygon`, to more clevery sample `MULTIPOLYGON`
  geometries; [\#1480](https://github.com/r-spatial/sf/issues/1480)

- [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md)
  accepts non-integer sample sizes, with a (suppressable) warning and
  handles values of sizes that would round to zero;
  [\#1480](https://github.com/r-spatial/sf/issues/1480)

- [`gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.md)
  adds utils `mdiminfo` and `mdimtranslate` (requires GDAL \>= 3.1)

- [`st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.md)
  gains an argument `is_coverage`, which, when set to `TRUE`, leads to
  much faster unioning in case features form a coverage (polygons don’t
  overlap); [\#1462](https://github.com/r-spatial/sf/issues/1462) by Don
  Baston

- fix `gdal_utils("translate")` locking input file;
  [\#1452](https://github.com/r-spatial/sf/issues/1452)

- [`st_make_grid()`](https://r-spatial.github.io/sf/reference/st_make_grid.md)
  no longer selects cells intersecting with `x`;
  [\#1447](https://github.com/r-spatial/sf/issues/1447)

- use
  [`s2::s2_dwithin_matrix()`](https://r-spatial.github.io/s2/reference/s2_closest_feature.html)
  in
  [`st_is_within_distance()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md);
  [\#1367](https://github.com/r-spatial/sf/issues/1367)

## version 0.9-5

CRAN release: 2020-07-14

- Only when package `s2` \>= 1.0.1 is available: support for spherical
  geometry operators (predicates, transformers, measures, nearest
  point/feature) for geographic coordinates in package `s2` is now by
  default switched off, and can be switched on by `sf_use_s2(TRUE)`; see
  <https://www.r-spatial.org/r/2020/06/17/s2.html> and vignette sf7. It
  is planned to be switched on by default in sf 1.0-0.

- drop Z and/or M coordinate in
  [`st_as_s2()`](https://r-spatial.github.io/sf/reference/s2.md), with
  message

- geometry predicates and transformers gain an … argument to pass
  [`s2::s2_options()`](https://r-spatial.github.io/s2/reference/s2_options.html)

- [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  now ensures the geometry column sticks to the back rather than the
  front of the data frame;
  [\#1425](https://github.com/r-spatial/sf/issues/1425)

- [`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)
  now preserves the active geometry column even when it is renamed;
  [\#1431](https://github.com/r-spatial/sf/issues/1431)

- proj units query adjusted to PROJ 7.1.0 release;
  [\#1434](https://github.com/r-spatial/sf/issues/1434)

## version 0.9-4

CRAN release: 2020-06-12

- empty geom generators take care of XYZ etc dim;
  [\#1400](https://github.com/r-spatial/sf/issues/1400)

- [`write_sf()`](https://r-spatial.github.io/sf/reference/st_write.md)
  and [`read_sf()`](https://r-spatial.github.io/sf/reference/st_read.md)
  no longer warn when reading tables without geometries

- [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  writes non-spatial tables when given a plain `data.frame` or `tbl_df`;
  [\#1345](https://github.com/r-spatial/sf/issues/1345)

- the default for `stringsAsFactors` in `st_read` and `st_sf` is `FALSE`
  for R version \>= 4.1.0

- the sf method for
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  supports renaming the geometry column;
  [\#1415](https://github.com/r-spatial/sf/issues/1415)

## version 0.9-3

CRAN release: 2020-05-04

- [`st_is_valid()`](https://r-spatial.github.io/sf/reference/valid.md)
  is a generic

- Windows CRAN binaries use GDAL 3.0.4, PROJ 6.3.1 and GEOS 3.8.0,
  thanks to Jeroen Ooms’ rwinlib work;
  [\#1275](https://github.com/r-spatial/sf/issues/1275)

- [`plot.sf()`](https://r-spatial.github.io/sf/reference/plot.md) gains
  an `extent` argument to set the extent (xlim, ylim) of the plot;
  `extent` must be an object with an
  [`st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.md)
  method, such as an `sf` or a `stars` object;
  [\#1193](https://github.com/r-spatial/sf/issues/1193)

## version 0.9-2

CRAN release: 2020-04-14

- `st_axis_order(TRUE)` gives and error if GDAL has version \< 2.5.0

- loading PROJ units `link`, `us_in`, `ind_yd`, `ind_ft`, and `ind_ch`
  into the udunits database is no longer done at package load time, but
  when function
  [`sf_add_proj_units()`](https://r-spatial.github.io/sf/reference/sf_project.md)
  is called.

- fix line sampling for small densities;
  [\#1365](https://github.com/r-spatial/sf/issues/1365)

- [`sf_project()`](https://r-spatial.github.io/sf/reference/sf_project.md)
  handles `crs` objects when PROJ version is below 6 using proj.4 string
  representations.

- avoid using `isFALSE` in
  [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md);
  [\#1342](https://github.com/r-spatial/sf/issues/1342)

- fix regression in `gdal_utils("translate", ...)`;
  [\#1339](https://github.com/r-spatial/sf/issues/1339)

## version 0.9-1

CRAN release: 2020-04-06

- fix an invalid read bug in
  [`st_m_range()`](https://r-spatial.github.io/sf/reference/st_m_range.md);
  [\#1332](https://github.com/r-spatial/sf/issues/1332)

- `st_crs(4326) == st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")`
  returns `TRUE` for GDAL \>= 3.0, irrespective authority compliance of
  axis order; see [\#1331](https://github.com/r-spatial/sf/issues/1331)
  and <https://github.com/ropensci/scrubr/issues/34>

- [`sf_project()`](https://r-spatial.github.io/sf/reference/sf_project.md)
  has a parameter `authority_compliant` to return coordinates in
  “visualisation order”; when `TRUE` it returns coordinates in authority
  compliant axis order (e.g. EPSG:4326 latitude longitude); default is
  [`st_axis_order()`](https://r-spatial.github.io/sf/reference/st_crs.md).

- fix test for Solaris and certain GDAL/PROJ versions

- fix error reading category table through GDAL;
  <https://github.com/r-spatial/stars/issues/245>

## version 0.9-0

CRAN release: 2020-03-24

- see r-spatial blog post:
  <https://www.r-spatial.org/r/2020/03/17/wkt.html>

- modify `crs` objects to reflect our post-proj4string world
  ([\#1146](https://github.com/r-spatial/sf/issues/1146);
  [\#1225](https://github.com/r-spatial/sf/issues/1225)): crs objects
  contain two fields, `input` with the user input (if any), and `wkt`
  with a well-known-text (or WKT2) representation of the coordinate
  reference system. `crs` objects have a `$` method to dynamically
  retrieve the `epsg` (integer) or `proj4string` representation, using
  e.g. `x$epsg`.

- support for PostGIS 3 using WKT and the new-style `crs` objects;
  [\#1234](https://github.com/r-spatial/sf/issues/1234),
  [\#1303](https://github.com/r-spatial/sf/issues/1303),
  [\#1308](https://github.com/r-spatial/sf/issues/1308) by
  [@etiennebr](https://github.com/etiennebr)

- [`st_write_db()`](https://r-spatial.github.io/sf/reference/sf-defunct.md)
  and
  [`st_read_db()`](https://r-spatial.github.io/sf/reference/sf-defunct.md)
  are defunct. Use `st_write` and `st_read` instead.

- [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  uses `append`, replacing (and deprecating) argument `update`;
  `st_write` fails when a layer already exists and `append` has not been
  set explicitly to `TRUE` (append) or `FALSE` (overwrite);
  [\#1266](https://github.com/r-spatial/sf/issues/1266)

- `st_proj_info()` was renamed into `sf_proj_info`; `sf_proj_info` can
  get and set the PROJ data search path and use of CDN;
  [\#1277](https://github.com/r-spatial/sf/issues/1277)

- adapt to new `dplyr` version;
  <https://github.com/tidyverse/dplyr/issues/4917>

- [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md)
  is a generic

- write `stars` rasters with wkt info, rather than proj4strings

- when GEOS \>= 3.8.0, `st_make_valid` is provided by `sf` rather than
  by `lwgeom` [\#989](https://github.com/r-spatial/sf/issues/989)

- allow for single-sided buffers for linear geometries;
  [\#1001](https://github.com/r-spatial/sf/issues/1001)

- add
  [`st_reverse()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  methods to reverse points in a linestring (requires GEOS \>= 3.7.0);
  [\#1246](https://github.com/r-spatial/sf/issues/1246)

- [`st_make_grid()`](https://r-spatial.github.io/sf/reference/st_make_grid.md)
  returns grid cells or points that intersect with the target geometry,
  not its bounding box;
  [\#1260](https://github.com/r-spatial/sf/issues/1260)

- allow for PROJ \>= 7;
  [\#1254](https://github.com/r-spatial/sf/issues/1254)

- [`st_geometry_type()`](https://r-spatial.github.io/sf/reference/st_geometry_type.md)
  accepts `by_geometry` argument;
  [\#1264](https://github.com/r-spatial/sf/issues/1264)

## version 0.8-1

CRAN release: 2020-01-28

- [`st_as_sf.map()`](https://r-spatial.github.io/sf/reference/st_as_sf.md)
  no longer requires `maptools` and `sp`; dropped dependency on
  maptools.

- work around a bug in 6.0.0 \<= PROJ \< 6.3.1: replace
  `+init=epsg:XXXX ...` strings with the `XXXX` EPSG integer, to work
  around a bug in PROJ; see <https://github.com/OSGeo/PROJ/pull/1875>
  and links therein. If `...` arguments are present, raise a warning
  that these are ignored.

- [`st_as_sf.map()`](https://r-spatial.github.io/sf/reference/st_as_sf.md)
  no longer requires `maptools` and `sp`; drop dependency on maptools.

- conversion between `spatstat` classes `owin`, `ppp` and `psp` and `sf`
  classes no longer use `maptools`;
  [\#1204](https://github.com/r-spatial/sf/issues/1204)

- [`gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.md)
  processes open options `-oo` and `-doo` properly;
  <https://github.com/ITSLeeds/geofabric/issues/12>

- [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md)
  directly interfaces `spatstat` sampling methods,
  e.g. `type = "Thomas"` calls `spatstat::rThomas` after converting
  input arguments (window) and converts returned `ppp` object to `sf`’s
  `POINT` geometries;
  [\#1204](https://github.com/r-spatial/sf/issues/1204) with help from
  Ege Rubak and Jakub Nowosad

- [`sf_project()`](https://r-spatial.github.io/sf/reference/sf_project.md)
  gains an option `keep = TRUE` to return `Inf` values for points not
  projectable; [\#1228](https://github.com/r-spatial/sf/issues/1228)

- support `vctrs` methods for geometry list columns; this makes `unnest`
  work again ([\#1172](https://github.com/r-spatial/sf/issues/1172));
  [\#1196](https://github.com/r-spatial/sf/issues/1196) by Lionel Henry

- `st_as_sf.pq_geometry()` converts binary geom columns from
  RPostgres::dbGetQuery;
  [\#1195](https://github.com/r-spatial/sf/issues/1195)

- [`st_cast()`](https://r-spatial.github.io/sf/reference/st_cast.md) can
  convert `MULTICURVE` to `MULTILINESTRING`;
  [\#1194](https://github.com/r-spatial/sf/issues/1194)

- [`st_read()`](https://r-spatial.github.io/sf/reference/st_read.md)
  gains a parameter `wkt_filter` for spatially filtering the features to
  be read; [\#1192](https://github.com/r-spatial/sf/issues/1192)

- [`st_area()`](https://r-spatial.github.io/sf/reference/geos_measures.md)
  and
  [`st_length()`](https://r-spatial.github.io/sf/reference/geos_measures.md)
  handle `+to_meter` argument in PROJ strings;
  [\#1170](https://github.com/r-spatial/sf/issues/1170)

- add
  [`st_filter()`](https://r-spatial.github.io/sf/reference/st_join.md)
  generic for filtering on spatial features only;
  [\#1148](https://github.com/r-spatial/sf/issues/1148)

- a new UBSAN error in `wkb_read()` was resolved;
  [\#1154](https://github.com/r-spatial/sf/issues/1154),
  [\#1152](https://github.com/r-spatial/sf/issues/1152)

- new method
  [`st_shift_longitude()`](https://r-spatial.github.io/sf/reference/st_shift_longitude.md)
  to re-center data for a Pacific view.
  [\#1218](https://github.com/r-spatial/sf/issues/1218)

- output of
  [`st_as_text()`](https://r-spatial.github.io/sf/reference/st_as_text.md)
  with `MULTIPOINT` has nested parentheses around points. E.g.,
  `MULTIPOINT ((0 0), (1 1))` instead of `MULTIPOINT (0 0, 1 1)`;
  [\#1219](https://github.com/r-spatial/sf/issues/1219),
  [\#1221](https://github.com/r-spatial/sf/issues/1221)

## version 0.8-0

CRAN release: 2019-09-17

- fix tests for PROJ 6.2.0 not accepting +units=

- fixes for tidyr 1.0-0 release; attempt to port
  [`nest.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  and
  [`unnest.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md);
  [\#1068](https://github.com/r-spatial/sf/issues/1068),
  [\#1145](https://github.com/r-spatial/sf/issues/1145)

- [`gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.md)
  better closes connections after use;
  [\#1143](https://github.com/r-spatial/sf/issues/1143)

- [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  gains a `drivers` options, to limit the drivers attempted;
  [\#1142](https://github.com/r-spatial/sf/issues/1142)

- rather than replacing,
  [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  and
  [`write_sf()`](https://r-spatial.github.io/sf/reference/st_write.md)
  append to an existing layer if `update=TRUE`;
  [\#1126](https://github.com/r-spatial/sf/issues/1126)

- improve plotting of `POSIXct` and `Date` attributes (`Date` requiring
  classInt \>= 0.4-2)

- `NULL` geometries read by GDAL are returned as empty geometries;
  [\#1119](https://github.com/r-spatial/sf/issues/1119)

- `gdal_utils('rasterize', ...)` accepts non-existing destinations,
  defined by e.g. resolution and extent options (see
  [\#1116](https://github.com/r-spatial/sf/issues/1116) for an example),
  and overwrites if needed (see
  [\#1136](https://github.com/r-spatial/sf/issues/1136) for an example)

- add Dan Baston as contributor;
  [\#1120](https://github.com/r-spatial/sf/issues/1120) and many others

- in addition to `NULL`,
  [`st_sfc()`](https://r-spatial.github.io/sf/reference/sfc.md) also
  converts `NA` values into empty geometries;
  [\#1114](https://github.com/r-spatial/sf/issues/1114).

- [`st_join()`](https://r-spatial.github.io/sf/reference/st_join.md) is
  a generic

## version 0.7-7

CRAN release: 2019-07-24

- [`plot()`](https://r-spatial.github.io/sf/reference/plot.md) handles
  `POSIXct` values in legend

- constructor functions like
  [`st_linestring()`](https://r-spatial.github.io/sf/reference/st.md)
  check and break on `NA` coordinates;
  [\#1101](https://github.com/r-spatial/sf/issues/1101),
  [\#1102](https://github.com/r-spatial/sf/issues/1102)

## version 0.7-6

CRAN release: 2019-07-05

- have examples of `st_write` write only to the temporary R session
  directory

## version 0.7-5

CRAN release: 2019-07-03

- `as(x, "Spatial")` gives a proper error message on empty geometries;
  [\#1093](https://github.com/r-spatial/sf/issues/1093)

- [`st_cast()`](https://r-spatial.github.io/sf/reference/st_cast.md)
  takes care of empty polygons;
  [\#1094](https://github.com/r-spatial/sf/issues/1094)

- `st_nearest_*` functions warn in case they are used with geographic
  coordinates; [\#1081](https://github.com/r-spatial/sf/issues/1081)

- [`st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.md)
  no longer segfaults on zero row `sf` objects;
  [\#1077](https://github.com/r-spatial/sf/issues/1077)

- [`st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.md)
  no longer breaks on zero row `sf` objects;
  [\#1075](https://github.com/r-spatial/sf/issues/1075)

- when PROJ \>= 6.1.0 is available and sf comes with datum files (as is
  the case with statically linked Windows and OSX CRAN binaries),
  `PROJ_LIB` is no longer temporarily overwritten, but the PROJ C api is
  used to set the datum path;
  [\#1074](https://github.com/r-spatial/sf/issues/1074), suggested by
  Jeroen Ooms

- sf compiles against GDAL 3.x and PROJ 6.1.0, using the new `proj.h`
  interface; [\#1070](https://github.com/r-spatial/sf/issues/1070)

- [`st_distance()`](https://r-spatial.github.io/sf/reference/geos_measures.md)
  returns `NA` for empty geometries, rather than 0;
  [\#1055](https://github.com/r-spatial/sf/issues/1055)

## version 0.7-4

CRAN release: 2019-04-25

- add example on how voronoi polygons can be tied back to the points
  they contain; [\#1030](https://github.com/r-spatial/sf/issues/1030)

- `st_difference(x, y)`, with `x` an `sfc` with zero feature geometries,
  returns `x`; [\#1024](https://github.com/r-spatial/sf/issues/1024)

- don’t reset (base) plot device when `add = TRUE`

- `==` and `!=` return `NA` when one of the operands is an empty
  geometry; [\#1013](https://github.com/r-spatial/sf/issues/1013)

- [`st_intersects()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  is a generic

- drop requiring `proj_api.h` in favor of `proj.h`, this enables
  compatibility to PROJ 6.0.0 and GDAL 2.5.0-dev;
  [\#988](https://github.com/r-spatial/sf/issues/988)

- fix regression in binary predicates introduced in
  [\#855](https://github.com/r-spatial/sf/issues/855);
  [\#999](https://github.com/r-spatial/sf/issues/999) reported by Barry
  Rowlingson

- fix bug in `gdal_utils` util `warper` on certain GDAL/OS combinations;
  <https://github.com/r-spatial/stars/issues/117>

- `c.sfc()` ignores the type (class) of empty `sfc` objects when
  choosing the result type;
  [\#985](https://github.com/r-spatial/sf/issues/985),
  [\#982](https://github.com/r-spatial/sf/issues/982)

- rename the default value for `distance` to `"Euclidean"`, rather than
  `"distance"` in
  [`st_distance()`](https://r-spatial.github.io/sf/reference/geos_measures.md)

## version 0.7-3

CRAN release: 2019-02-21

- add argument `exact` to
  [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md),
  defaulting to `FALSE`;
  [\#896](https://github.com/r-spatial/sf/issues/896)

- fixed n-ary
  [`st_difference()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.md)
  for cases where geometries are entirely contained in others;
  [\#975](https://github.com/r-spatial/sf/issues/975), by Jonathan
  Marshall

- faster [`Ops.sfc()`](https://r-spatial.github.io/sf/reference/Ops.md),
  added
  [`st_normalize()`](https://r-spatial.github.io/sf/reference/st_normalize.md);
  [\#973](https://github.com/r-spatial/sf/issues/973) by Thomas Lin
  Pedersen

- new grob constructor for sfc objects;
  [\#971](https://github.com/r-spatial/sf/issues/971) by Thomas Lin
  Pedersen; add Thomas as contributor

- add
  [`group_split()`](https://dplyr.tidyverse.org/reference/group_split.html)
  and
  [`group_map()`](https://dplyr.tidyverse.org/reference/group_map.html)
  methods for `sf` objects (experimental);
  [\#969](https://github.com/r-spatial/sf/issues/969)

- make
  [`st_interpolate_aw()`](https://r-spatial.github.io/sf/reference/interpolate_aw.md)
  a generic;

- argument `col` for `plot` of `GEOMETRY` `sfc`’s is `NA` (open) for
  (multi) polygon geometries

## version 0.7-2

CRAN release: 2018-12-20

- feature IDs are no longer returned as names on the geometry list
  column, but optionally returned by `st_read` as attribute column;
  [\#812](https://github.com/r-spatial/sf/issues/812)

- when plotting multiple attributes, plot.sf adds a (single, common) key
  if `key.pos` is set

- precision can be specified in distance units;
  [\#901](https://github.com/r-spatial/sf/issues/901)

- support log-scale in color legend by setting `logz` to `TRUE` in
  `plot.sf`

- [`st_intersects()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  etc. will prepare `y` when `y` is polygons and `x` is points;
  [\#885](https://github.com/r-spatial/sf/issues/885) by Dan Baston

- [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  (and
  [`write_sf()`](https://r-spatial.github.io/sf/reference/st_write.md))
  returns its first argument, invisibly;
  [\#889](https://github.com/r-spatial/sf/issues/889)

## version 0.7-1

CRAN release: 2018-10-24

- fix bug that broke n-ary
  [`st_intersection()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.md)
  on platforms using clang;
  [\#867](https://github.com/r-spatial/sf/issues/867)

## version 0.7-0

CRAN release: 2018-10-17

- adds several interfaces to GDAL functions, meant to be used by package
  `stars`

- [`st_read()`](https://r-spatial.github.io/sf/reference/st_read.md)
  receives a `query` argument that can run queries against OGR datasets;
  [\#834](https://github.com/r-spatial/sf/issues/834), by Barry
  Rowlingson and Michael Sumner

- [`read_sf()`](https://r-spatial.github.io/sf/reference/st_read.md) no
  longer first creates tibbles from `data.frame`s, but creates them
  directly; [\#853](https://github.com/r-spatial/sf/issues/853), db
  propagation by Etienne Racine

- check difference between compile-time and run-time GEOS versions;
  [\#844](https://github.com/r-spatial/sf/issues/844)

- all GEOS routines are (more) robust against memory leaks, by using
  unique pointers; [\#822](https://github.com/r-spatial/sf/issues/822),
  [\#845](https://github.com/r-spatial/sf/issues/845), by Dan Baston

- [`st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  receives the buffer styles `endCapStyle`, `joinStyle` and
  `mitreLimit`; [\#833](https://github.com/r-spatial/sf/issues/833),
  [\#842](https://github.com/r-spatial/sf/issues/842) by Michael Sumner

## version 0.6-4

- [`st_area()`](https://r-spatial.github.io/sf/reference/geos_measures.md)
  is a generic; <https://github.com/r-spatial/stars/issues/32>

- [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  resolves `~` correctly;
  [\#456](https://github.com/r-spatial/sf/issues/456)

- read and write feature IDs as sfc list column names;
  [\#812](https://github.com/r-spatial/sf/issues/812)

- [`st_centroid()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  works for empty geometries, returning an empty point
  [\#769](https://github.com/r-spatial/sf/issues/769)

- add
  [`st_nearest_points()`](https://r-spatial.github.io/sf/reference/st_nearest_points.md),
  to obtain the (`LINESTRING` connecting the) two nearest points for
  pairs of geometries;
  [\#788](https://github.com/r-spatial/sf/issues/788)

- add hexagonal tiling to
  [`st_make_grid()`](https://r-spatial.github.io/sf/reference/st_make_grid.md)

- add regular and hexagonal sampling to
  [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md)

- fixes for PROJ 5.0.1;
  [\#545](https://github.com/r-spatial/sf/issues/545)

- fixes for GDAL 2.3.0;
  [\#759](https://github.com/r-spatial/sf/issues/759)

- [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md)
  supports regular sampling of `LINESTRING`;
  [\#725](https://github.com/r-spatial/sf/issues/725) by
  [@statnmap](https://github.com/statnmap)

- Support reading and writing of database `Pool` objects;
  [\#756](https://github.com/r-spatial/sf/issues/756)

- fix plotting of `sf` objects without attributes;
  [\#755](https://github.com/r-spatial/sf/issues/755)

- add reference to the [R Journal
  article](https://journal.r-project.org/articles/RJ-2018-009/index.html)
  in CITATION

## version 0.6-3

CRAN release: 2018-05-17

- move dependency `RPostgreSQL` from Imports: back to Suggests:

- `st_centroid.sf()` and `st_point_on_surface.sf` also warn if
  attributes are not constant over geometries.

- [`summarise()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  allows the user to define geometries for summaries;
  [\#714](https://github.com/r-spatial/sf/issues/714), by Kirill Mueller

- [`plot.sf()`](https://r-spatial.github.io/sf/reference/plot.md) emits
  a warning if `col` does not have length 1 or `nrow(x)`, and requires
  `pal` (rather than `col`) to set a palette for factors.

- [`plot.sf()`](https://r-spatial.github.io/sf/reference/plot.md)
  provides control over legend keys using `key.length` and `key.width`,
  decrease default key length;
  [\#731](https://github.com/r-spatial/sf/issues/731)

- `sgbp` objects receive an `as.data.frame` method;
  [\#715](https://github.com/r-spatial/sf/issues/715)

## version 0.6-2

CRAN release: 2018-04-25

- GDAL read/write supports logical variables;
  [\#722](https://github.com/r-spatial/sf/issues/722)

- add [`st_crop()`](https://r-spatial.github.io/sf/reference/st_crop.md)
  to simplify cropping objects with a rectangular area;
  [\#720](https://github.com/r-spatial/sf/issues/720)

- fix bug in `[<-` when columns are added to an `sf` object;
  [\#718](https://github.com/r-spatial/sf/issues/718)

- use dynamic registration of S3 methods, similar to how hms does this;
  [\#710](https://github.com/r-spatial/sf/issues/710) by Kirill Mueller

- (partially) address writing GPKG to network drive, writing to temp
  file first; [\#628](https://github.com/r-spatial/sf/issues/628)

- add Kirill Mueller as contributor

- [`st_make_grid()`](https://r-spatial.github.io/sf/reference/st_make_grid.md)
  is faster; [\#708](https://github.com/r-spatial/sf/issues/708), by Dan
  Baston

- [`st_read()`](https://r-spatial.github.io/sf/reference/st_read.md) and
  [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  are generic, with methods for directly reading from and writing to
  database connections; `st_read_db` and `st_write_db` are deprecated;
  [\#558](https://github.com/r-spatial/sf/issues/558), thanks to Etienne
  Racine [@etiennebr](https://github.com/etiennebr)

- Package `RPostgreSQL` moved from Suggests to Imports

- restore compatibility with GDAL 2.0.x versions (which won’t have
  `gdal_utils`); [\#686](https://github.com/r-spatial/sf/issues/686)

- [`read_sf()`](https://r-spatial.github.io/sf/reference/st_read.md) can
  also read tables without geometry;
  [\#684](https://github.com/r-spatial/sf/issues/684), by Andy Teucher

## version 0.6-1

CRAN release: 2018-03-22

- method
  [`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
  works; [\#669](https://github.com/r-spatial/sf/issues/669),
  [\#672](https://github.com/r-spatial/sf/issues/672)

- `+`, `-`, `*` and `/` for pairs of geometries (`sfg`, `sfc`) return
  geometric union, difference, intersection and symmetric difference,
  respectively.

- [`st_cast()`](https://r-spatial.github.io/sf/reference/st_cast.md)
  from `MULTIPOLYGON` to `MULTILINESTRING` should work properly;
  [\#660](https://github.com/r-spatial/sf/issues/660)

- all Rcpp interfaces needed by package `stars` have been moved into
  `sf`; pkg `stars` is R-only, and only `sf` needs linking to GDAL.

- [`gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.md)
  interfaces the 9 gdal utils using the C++ API

- improve resetting (base) plots; add `reset = FALSE` in a call to
  `plot` to enable adding to plots that have a legend

- [`st_read()`](https://r-spatial.github.io/sf/reference/st_read.md)
  returns a `data.frame` when a table contains no geometries, rather
  than giving an error; it does emit a warning in this case. See
  <https://stat.ethz.ch/pipermail/r-sig-geo/2018-February/026344.html>

- move `pillar` from `Imports:` to `Suggests:`

- update to the new rwinlib distribution of gdal (adds JPG2000);
  [\#639](https://github.com/r-spatial/sf/issues/639)

- speed up computation of centroids for largest polygon;
  [\#623](https://github.com/r-spatial/sf/issues/623)

- add `st_as_sfc.raw` method

- Bugfix: binary operations (`st_intersection`, `st_difference`, etc) no
  longer fail when operating on data frames of class `"tbl_df"` with
  common column names;
  [\#644](https://github.com/r-spatial/sf/issues/644)

## version 0.6-0

CRAN release: 2018-01-06

- add `pillar` to Imports: to provide method for printing WKT geometries
  in tibbles

- `st_as_text`, and subsequently `format` and `print`, use argument
  `digits` (or `options(digits = n)`) to control the number of digits
  used for printing coordinates; default is `options("digits")`, which
  is typically 7.

- `st_is_within_distance` works with geographic coordinates

- `st_cast` from `MULTIPOLYGON` to `MULTILINESTRING` no longer changes
  the number of features/feature geometries, but conversion from
  `MULTIPOLYGON` to `LINESTRING` (typically) does;
  [\#596](https://github.com/r-spatial/sf/issues/596)

- `st_distance` for long/lat geographic coordinates uses `lwgeom`,
  accepting all geometry types; argument `dist_fun` is deprecated as a
  consequence, and distance calculations are different from those in sf
  versions 0.5-5 or earlier;
  [\#593](https://github.com/r-spatial/sf/issues/593)

- add package `lwgeom` to Suggests; `st_area`, `st_length`,
  `st_distance`, `st_segmentize` for long/lat CRS use package `lwgeom`
  instead of `geosphere`;
  [\#593](https://github.com/r-spatial/sf/issues/593)

- `st_length` returns zero for polygon-type geometries;
  [\#593](https://github.com/r-spatial/sf/issues/593)

- if present, add units of attribute to default plot title;
  [\#591](https://github.com/r-spatial/sf/issues/591)

- add `unnest` method, which depends on `tidyr` \> 0.7-2;
  [\#570](https://github.com/r-spatial/sf/issues/570) PR by
  [@karldw](https://github.com/karldw)

- add `largest` option to `st_join` to get largest intersection match
  only; [\#547](https://github.com/r-spatial/sf/issues/547), by
  [@tiernanmartin](https://github.com/tiernanmartin)

- change default maximum number of feature to print to 10, controllable
  by `options(sf_max_print)`;
  [\#556](https://github.com/r-spatial/sf/issues/556)

- add `Hausdorff` (and `Frechet` for those with GEOS 3.7.0) as options
  to `st_distance`; add `par` for densified versions

- add `st_snap`, for snapping geometries to other geometries, within a
  tolerance

- make `st_wrap_dateline` a generic, with methods for `sf`, `sfc` and
  `sfg`; [\#541](https://github.com/r-spatial/sf/issues/541)

- `plot.sf` and `st_as_grob` (used by ggplot2) are robust against
  misspecified ring directions (holes that have the same direction as
  the exterior rings), by using `rule = "evenodd"`;
  [\#540](https://github.com/r-spatial/sf/issues/540)

- functions depending on `liblwgeom` (`st_make_valid`, `st_geohash`,
  `st_plit`) have been moved to their own package,
  <https://github.com/r-spatial/lwgeom>; argument `use_gdal` of
  `st_transform` has been deprecated, instead one can use
  [`lwgeom::st_transform_proj`](https://r-spatial.github.io/lwgeom/reference/st_transform_proj.html);
  sf no longer tries to link to liblwgeom;
  [\#509](https://github.com/r-spatial/sf/issues/509),
  [\#537](https://github.com/r-spatial/sf/issues/537),
  [\#487](https://github.com/r-spatial/sf/issues/487)

- `st_read`, `st_sf` and `st_sfc` gain a parameter `check_ring_dir`
  (default: `FALSE`) that checks ring directions and corrects to:
  exterior counter clockwise, holes clockwise, when seen from above.

- get rid of
  [`classInt::classIntervals`](https://r-spatial.github.io/classInt/reference/classIntervals.html)
  warning if number of unique values is smaller than the number of
  breaks asked for

## version 0.5-5

CRAN release: 2017-10-31

- have `classInt` in Imports:, to not break other package checks

- add vignettes 5: plotting sf objects and 6: miscellaneous;
  [\#324](https://github.com/r-spatial/sf/issues/324)

- add (default) color key to `plot.sf` if single map is plotted,
  contributed by [@hughjonesd](https://github.com/hughjonesd);
  [\#528](https://github.com/r-spatial/sf/issues/528)

- `st_as_sfc` can read EWKT;
  [\#530](https://github.com/r-spatial/sf/issues/530)

- argument `max.plot` takes its default from `options(sf_max.plot=n)`,
  if present; [\#516](https://github.com/r-spatial/sf/issues/516)

- `plot.sf` gets an arguments `pal` to specify a color palette function;
  [\#526](https://github.com/r-spatial/sf/issues/526)

- `plot.sf` gets arguments `breaks` and `nbreaks`; add support for
  [`classInt::classIntervals`](https://r-spatial.github.io/classInt/reference/classIntervals.html)
  styles for finding class intervals (using `breaks`)

- add `st_as_sf` methods for `ppp`, `lpp` and `psp` objects from
  spatstat.

- allow for direct route to proj.4 ignoring GDAL (requiring liblwgeom);
  [\#509](https://github.com/r-spatial/sf/issues/509),
  [\#511](https://github.com/r-spatial/sf/issues/511)

- add `print` method for `crs` objects;
  [\#517](https://github.com/r-spatial/sf/issues/517)

- `sf_extSoftVersion` reveals whether GDAL was linked to GEOS;
  [\#510](https://github.com/r-spatial/sf/issues/510)

- better check input of `st_polygon`;
  [\#514](https://github.com/r-spatial/sf/issues/514)

- add `st_node`, similar to `rgeos::gNode`

- support for reading `OFTInteger64List` fields;
  [\#508](https://github.com/r-spatial/sf/issues/508)

- sparse geometric binary predicate lists have a class, `sgbp`, and
  attributes `region.id` and `predicate`;
  [\#234](https://github.com/r-spatial/sf/issues/234),
  [\#524](https://github.com/r-spatial/sf/issues/524)

- prevent `st_split` from stopping the R session;
  [\#492](https://github.com/r-spatial/sf/issues/492)

- `st_intersection`, `st_union` and so on also print a message when used
  directly on long/lat coordinates;
  [\#496](https://github.com/r-spatial/sf/issues/496)

- add `rep` method for `sfc` objects

- comparing two `crs` objects uses the GDAL function `IsSame`;
  [\#180](https://github.com/r-spatial/sf/issues/180)

- add `st_collection_extract`, which, given an object with geometries of
  type `GEOMETRY` or `GEOMETRYCOLLECTION`, returns an object consisting
  only of elements of the specified type; by Andy Teucher,
  [\#482](https://github.com/r-spatial/sf/issues/482)

- `st_write` exports GeoJSON with UTF-8 encoding on Windows;
  [\#444](https://github.com/r-spatial/sf/issues/444)

- move package methods from Imports: to Depends: ;
  [\#478](https://github.com/r-spatial/sf/issues/478)

- deal better with precision setting and propagation;
  [\#476](https://github.com/r-spatial/sf/issues/476)

- fix bug in `st_layers` in case layers have no geometry;
  [\#334](https://github.com/r-spatial/sf/issues/334)

- clarify argument `envelope` in `st_voronoi`;
  [\#474](https://github.com/r-spatial/sf/issues/474)

- change aggregate to make it return the same geometry as ‘by’, padding
  attributes with NA where needed;
  [\#453](https://github.com/r-spatial/sf/issues/453)

## version 0.5-4

CRAN release: 2017-08-28

- fix compatibility problems introduced by `tidyr` 0.7-0 using rlang
  magic

- convert path names to UTF-8 in `st_read`, `st_write` and `st_layers`;
  [\#471](https://github.com/r-spatial/sf/issues/471)

- `st_sfc` converts `NULL` values into empty geometries, and correctly
  identifies empty `POINT`s;
  [\#466](https://github.com/r-spatial/sf/issues/466),
  [\#463](https://github.com/r-spatial/sf/issues/463)

- `st_write` abbreviates column names if driver is `ESRI Shapefile`;
  [\#464](https://github.com/r-spatial/sf/issues/464)

- add `of_largest_polygon` argument to `st_centroid`, to get the
  centroid of the largest polygon;
  [\#450](https://github.com/r-spatial/sf/issues/450)

- fix use of `st_relate` as join predicate for `st_join`;
  [\#454](https://github.com/r-spatial/sf/issues/454)

- fix bug where `st_intersects` with empty second argument would crash;
  [\#458](https://github.com/r-spatial/sf/issues/458)

- produce better WKT;
  [\#463](https://github.com/r-spatial/sf/issues/463)

- fix bug in `st_cast.sf`;
  [\#461](https://github.com/r-spatial/sf/issues/461),
  [\#462](https://github.com/r-spatial/sf/issues/462)

- change `st_read` SRS assignment logic; corrects reading projected
  geojson with gdal 2.2.0;
  [\#449](https://github.com/r-spatial/sf/issues/449)

- `st_intersection` etc. on `tbl` also return `tbl`;
  [\#448](https://github.com/r-spatial/sf/issues/448)

- `[.sf` preserves class, e.g. of `tbl`;
  [\#448](https://github.com/r-spatial/sf/issues/448)

## version 0.5-3

CRAN release: 2017-07-30

- support and propagate all Proj.4 +units=xx length units;
  [\#446](https://github.com/r-spatial/sf/issues/446)

- allow for arith ops on empty `sfc` objects

- have `st_graticule` return an empty graticule object when argument
  `datum` is `NA`;

- export `as_Spatial`, to make it easier for packages to convert `sfc`
  objects without importing `sf`

- `st_distance` gains a parameter `by_element` to obtain pairwise
  distances; [\#437](https://github.com/r-spatial/sf/issues/437)

- add the ability to `aggregate` using a simple feature `by` argument;
  [\#429](https://github.com/r-spatial/sf/issues/429)

- make the `op` argument to `[.sf` work

- speed up `st_coordinates` for `POINT` geometries;
  [\#433](https://github.com/r-spatial/sf/issues/433)

- fix performance regression for `st_bbox`;
  [\#418](https://github.com/r-spatial/sf/issues/418)

- correct bug in `st_union`, `st_difference` and `st_sym_difference`
  introduced in 0.5-2;
  [\#431](https://github.com/r-spatial/sf/issues/431)

- inform gdal about the CRS always through the proj4string, never
  through the epsg; see
  [\#424](https://github.com/r-spatial/sf/issues/424)

- properly deal with kilometre units;
  [\#424](https://github.com/r-spatial/sf/issues/424) (fixed by Karl
  Dunkle Werner)

- add `st_is_within_distance`, only to return a sparse index matrix;
  [\#419](https://github.com/r-spatial/sf/issues/419)

- have `st_graticule` work with world2 (0,360);
  [\#421](https://github.com/r-spatial/sf/issues/421),
  [\#422](https://github.com/r-spatial/sf/issues/422), fixed by Ben Best

- `st_graticule` to return graticules in native crs;
  <https://github.com/tidyverse/ggplot2/issues/2200> (WIP)

- `st_graticule` to support data in `NA_crs_`;
  <https://github.com/tidyverse/ggplot2/issues/2199>

- fix bug when joining an sf-tibble with a `tibble`;
  [\#414](https://github.com/r-spatial/sf/issues/414)

- read gdal `StringList`, `RealList`, and `IntegerList` fields into a
  list-column; [\#416](https://github.com/r-spatial/sf/issues/416)

## version 0.5-2

CRAN release: 2017-07-12

- made ready for rwinlib/gdal2;
  [\#408](https://github.com/r-spatial/sf/issues/408)

- make `[.sf` for selections including `NA` values like `x[c(1,NA,2)]`;
  [\#403](https://github.com/r-spatial/sf/issues/403)

- add a `[<-` method for `sfc` objects; automatically replaces `NULL`
  with an empty geometry;
  [\#411](https://github.com/r-spatial/sf/issues/411)

- add
  [`st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  to return a point that is guaranteed to be on the surface (standard
  compliance)

- `read_sf` returns an sf-tibble, an object of class
  `c("sf", "tbl_df", "tbl", "data.frame")`

- work around for
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
  not dispatching geometry column subsetting to `sf::[.sfc`

- allow `units` object as `dist` argument to `st_buffer`; these must be
  convertable to `arc_degree` for geographic, and to a length unit for
  non-geographic data;
  [\#399](https://github.com/r-spatial/sf/issues/399)

- prevent gdal from crashing when trying to `st_transform` an empty
  geometry; [\#398](https://github.com/r-spatial/sf/issues/398)

- add `st_as_sfc` method for `bbox`, returning the bbox polygon;
  [\#377](https://github.com/r-spatial/sf/issues/377)

- strip file name extension from default layer name in `st_write`;
  [\#392](https://github.com/r-spatial/sf/issues/392)

- have `st_sf` replace `NULL` values in an `sfc` list-column with the
  appropriate empty geometry;
  [\#372](https://github.com/r-spatial/sf/issues/372)

- allow setting `ndiscr` through `ggplot2::coords_sf` to improve
  graticule plotting in `geom_sf`;
  [\#396](https://github.com/r-spatial/sf/issues/396)

## version 0.5-1

CRAN release: 2017-06-23

- add spatial indexes to most binary geometry operations;
  [\#394](https://github.com/r-spatial/sf/issues/394) and
  [http://r-spatial.org/r/2017/06/22/spatial-index.html](http://r-spatial.org/r/2017/06/22/spatial-index.md)

- drastically reduce memory footprint of `st_intersection` and similar;
  [\#394](https://github.com/r-spatial/sf/issues/394)

- support RSQLite 2.0 by providing an `st_as_sfc` method for list
  columns of class `blob`

- drop dependency on dbplyr

## version 0.5-0

CRAN release: 2017-06-15

- better handle empty/NULL geometries in shapefiles;
  [\#351](https://github.com/r-spatial/sf/issues/351)

- add `unite_.sf` method

- deprecate `FUN` argument to `st_join`;
  [\#376](https://github.com/r-spatial/sf/issues/376)

- improve graticule tic label placement in `ggplot2`;
  [\#375](https://github.com/r-spatial/sf/issues/375) and
  <https://github.com/tidyverse/ggplot2/issues/2119>

- improve `configure` logic to deal with libraries installed in custom
  locations; [\#335](https://github.com/r-spatial/sf/issues/335)

- fix bug where `geom_sf` wouldn’t deal with Z and/or M geoms;
  [\#373](https://github.com/r-spatial/sf/issues/373)

- return more conveniently typed empty geoms;
  [\#372](https://github.com/r-spatial/sf/issues/372)

- fix subsetting with `[` of `sf` using `drop = TRUE`,
  [\#370](https://github.com/r-spatial/sf/issues/370)

- in addition to `m`, allow `rad` units to `st_segmentize`

- add example how to `st_read` GeoJSON from a string;
  [\#185](https://github.com/r-spatial/sf/issues/185)

- add `separate_.sf` method

- add `st_split` to split geometries (only available if compiled against
  liblwgeom), [\#359](https://github.com/r-spatial/sf/issues/359)

- fix bug reading and writing dates (months 1 off):
  [\#358](https://github.com/r-spatial/sf/issues/358)

- \[.sf and \[.sfc also select on i when i is an `sfg` object, and
  accept a geometric predicate function with optional arguments;
  [\#352](https://github.com/r-spatial/sf/issues/352)

- on reading through GDAL, empty (NULL) geometries no longer result in
  an error; on creation, they no longer automatically give a `GEOMETRY`
  object; [\#351](https://github.com/r-spatial/sf/issues/351)

- on plotting with
  [`ggplot2::geom_sf`](https://ggplot2.tidyverse.org/reference/ggsf.html),
  empty geometries no longer break; grid functions return
  [`nullGrob()`](https://rdrr.io/r/grid/grid.null.html) for them;
  [\#351](https://github.com/r-spatial/sf/issues/351)

- arith operations on empty geometries no longer break or give warnings;
  [\#351](https://github.com/r-spatial/sf/issues/351)

- have `st_as_sf.data.frame` by default break on `NA` values in
  coordinates; [\#342](https://github.com/r-spatial/sf/issues/342)

- have `st_join` accept further arguments, to be passed on to the `join`
  function (e.g. a pattern for `st_relate`)

- have WKB reader throw an error on (some) malformed inputs, and check
  for buffer bounds

## version 0.4-3

CRAN release: 2017-05-15

- back-port `do_union` argument to dplyr \<= 0.5.0, using lazyeval

- all strings returned from OGR/GDAL get encoding set to `UTF-8`, making
  them work on non-UTF-8 platforms;
  [\#5](https://github.com/r-spatial/sf/issues/5)

- `$.crs` retrieves proj4string components, such as `st_crs(4326)$datum`
  in addition to `epsg` and `proj4string`

- let `st_geohash` return geohash for (average) points (only when sf was
  linked to liblwgeom)

## version 0.4-2

CRAN release: 2017-05-05

- `summarise.sf` always returns an `sf` object, also for global
  (non-grouped) summaries.

- `summarise.sf` gains an argument `do_union` which determines whether
  to union the geometries for which a summary is given, or to
  `st_combine` them (not resolving boundaries);
  [\#331](https://github.com/r-spatial/sf/issues/331)

- rename argument `union` of `aggregate.sf` into `do_union`, for
  consistency with `summarise`;
  [\#331](https://github.com/r-spatial/sf/issues/331)

- add a `nest_` method for `sf` objects

- `st_relate` gets a `pattern` parameter, same as `rgeos::gRelate`; add
  examples to get rook and queen neighbour lists using this;
  [\#234](https://github.com/r-spatial/sf/issues/234)

- support for direct reading of spatialite and sqlite geometry wkb blobs

- build proper support for `cbind` and `rbind` methods for `sf`, which
  work (as documented) when *all* arguments are of class `sf`;
  [`dplyr::bind_cols`](https://dplyr.tidyverse.org/reference/bind_cols.html)
  or `st_sf(data.frame(sf, df))` work for binding `data.frame`s to an
  `sf` object.

- [`st_segmentize()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  and
  [`st_line_sample()`](https://r-spatial.github.io/sf/reference/st_line_sample.md)
  accept units arguments

- document problem reading shapefiles from USB drives on OSX;
  [\#252](https://github.com/r-spatial/sf/issues/252)

- improve docs of `st_is_valid` and `st_make_valid`;
  [\#296](https://github.com/r-spatial/sf/issues/296)

- coercing `sf` to `data.frame` works better;
  [\#298](https://github.com/r-spatial/sf/issues/298)

- `st_line_sample` gains argument `sample` to specify the points t.b.
  sampled; [\#299](https://github.com/r-spatial/sf/issues/299)
  [\#300](https://github.com/r-spatial/sf/issues/300) thanks to
  [@joethorley](https://github.com/joethorley)

- add compatibility to upcoming dplyr 0.6.0;
  [\#304](https://github.com/r-spatial/sf/issues/304)
  [\#42](https://github.com/r-spatial/sf/issues/42)

- write GDAL fields by name, not by number, fixing a KML problem
  [\#308](https://github.com/r-spatial/sf/issues/308)

- `st_write` gains arguments `delete_layer` and `delete_dsn` to allow
  overwrite capability
  [\#307](https://github.com/r-spatial/sf/issues/307)
  [\#274](https://github.com/r-spatial/sf/issues/274)

- `write_sf` defaults to `delete_layer=TRUE`, silently overwriting
  layers if they’re already present

- compatibility with GDAL 2.2beta0;
  [\#303](https://github.com/r-spatial/sf/issues/303);
  [\#309](https://github.com/r-spatial/sf/issues/309)

- replace `st_write_db` with a version that is fast for large datasets
  ([\#285](https://github.com/r-spatial/sf/issues/285)), thanks to Josh
  London

- take out more memory leaking examples in tests

- the `aggregate` method for `sf` objects assumes the `by` argument to
  be identical to that of
  [`stats::aggregate`](https://rdrr.io/r/stats/aggregate.html)

- `st_wrap_dateline` wraps (cuts up) geometries crossing the
  antimeridian, such that they no longer cross it.

## version 0.4-1

CRAN release: 2017-03-28

- restore 3.3.0 and c++11 requirement

- `st_read` respects time that is read as UTC

- `st_write` writes time always as UTC, since GDAL does not have a
  mechanism to define local timezones other than “unknown” or “local”

- `st_length` works for POINT and MULTIPOINT (returning 0); POLYGON and
  MULTIPOLYGON are converted to MULTILINESTRING before computing length,
  thus giving polygon perimeter
  ([\#268](https://github.com/r-spatial/sf/issues/268))

- `st_write` has `update` depend on driver; for databases, the default
  is `TRUE`, otherwise `FALSE` (it refers to update of the database, and
  not to overwriting the table in the database, this will by default not
  succeed); [\#274](https://github.com/r-spatial/sf/issues/274)

- `st_read` supports reading objects with multiple geometry columns
  [\#257](https://github.com/r-spatial/sf/issues/257)
  [\#255](https://github.com/r-spatial/sf/issues/255)

- support writing (exporting) objects with non-standard columns, such as
  `units` or `POSIXlt`
  [\#264](https://github.com/r-spatial/sf/issues/264)

- catch dependencies on GEOS 3.3.5 (hence no 0.4-0 CRAN binary for
  MacOSX) [\#260](https://github.com/r-spatial/sf/issues/260)

## version 0.4-0

CRAN release: 2017-03-21

- have `st_is_valid` catch corrupt geometries too, returning `NA` in
  that case (requiring GEOS 3.5.0)

- add `st_make_valid`, only available when sf was linked to `liblwgeom`

- add `st_coordinates` method, returning coordinates matrix with indexes

- remove `unlist.sfg`

- add `as.matrix.sfg`; have as.matrix.sfg add indexes to coordinates

- add `st_bind_cols` method

- improve handling features that can’t be projected

- support uniform sampling over polygons on the sphere

- add `st_sample`, for sampling points on multipoints, linestrings, or
  polygons

- add `c` method for `sfc` objects

- import and export `magrittr::%>%`

- support ggplot’ing geometrycollections

- drop C++11 requirement, allowing build for older R versions

- add `st_proj_info`, modelled after `rgdal::projInfo`

- overwriting datasets with
  [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  is no longer allowed; `update=TRUE` appends to them, permitted the
  driver supports appending.

- `st_write` gains an argument, `update`, which when `TRUE` will try to
  append to existing datasets
  ([\#204](https://github.com/r-spatial/sf/issues/204))

- added list of corresponding function for migration from sp, rgdal and
  rgeos to sf at <https://github.com/edzer/sfr/wiki/migrating>

- remove deprecated `st_list`

- rename `st_makegrid` to `st_make_grid`, and `st_linemerge` to
  `st_line_merge`

- add NEWS.md file ([\#207](https://github.com/r-spatial/sf/issues/207))

- faster conversion of `data.frame` into `POINT` `sf` object, using
  `st_as_sf` (Michael Sumner)

- `rbind` method for `sf` objects keeps coordinate reference system

## version 0.3-4, Feb 6, 2017

CRAN release: 2017-02-06

- add `st_contains_properly` spatial predicate

- GEOS functions (geometry operations) accept XYZ geometries (and ignore
  Z)

- make `prepared = TRUE` the default for all geometry binary operations

## version 0.3-2, Feb 4, 2017

CRAN release: 2017-02-04

- add user interrupt checks in all GEOS geometry operations

- make `st_graticule` do something useful for polar projections

- make `st_graticule` return `NA` labels when labels are useless

- add `merge.sf` methods to merge `sf` object and `data.frame`
  ([\#193](https://github.com/r-spatial/sf/issues/193))

- add `st_join` for table joins based on (user-defined) spatial
  predicates

- add `dplyr`-style non-spatial joins for `sf` objects (`left_join`,
  `full_join` etc.)
  ([\#193](https://github.com/r-spatial/sf/issues/193))

- allow for multiple non-gathered variables
  ([\#196](https://github.com/r-spatial/sf/issues/196))

- add missing meridian to `st_graticule`
  ([\#198](https://github.com/r-spatial/sf/issues/198))

## version 0.3-1, Jan 31, 2017

CRAN release: 2017-01-31

- add `merge` method
  ([\#193](https://github.com/r-spatial/sf/issues/193))

- `st_graticule` for laea
  ([\#198](https://github.com/r-spatial/sf/issues/198))

- allow `st_buffer` with feature-dependent buffer distance
  ([\#197](https://github.com/r-spatial/sf/issues/197))

- have `spread` return an `sf` object
  ([\#196](https://github.com/r-spatial/sf/issues/196))

- clarify `overwrite = TRUE` in write docs

- fix `st_as_sf.map`
  ([\#194](https://github.com/r-spatial/sf/issues/194))

- add `prepared` arg to spatial binary predicates, to speed up large
  intersections

- add `st_voronoi` interface (requires that lib GEOS \>= 3.5.0)

- add `st_as_sf` methods for `map` objects (library maps)

- add RStudio project file

- have `st_bbox` return a `bbox` object which has an `st_crs` method

- rename `st_drop_zm` into `st_zm`, for general more handling of Z and M

- allow for 3D coordinates returned, when `+proj=geocent`
  ([\#172](https://github.com/r-spatial/sf/issues/172);
  [\#103](https://github.com/r-spatial/sf/issues/103))

- fix `NA_integer_` handling in shapefiles I/O
  ([\#184](https://github.com/r-spatial/sf/issues/184))

- add and fix `st_agr` API, to set and get attribute-to-geometry
  relationships

## version 0.2-8, Jan 5, 2017

CRAN release: 2017-01-05
