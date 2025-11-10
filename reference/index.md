# Package index

## All functions

- [`Ops(`*`<sfg>`*`)`](https://r-spatial.github.io/sf/reference/Ops.md)
  [`Ops(`*`<sfc>`*`)`](https://r-spatial.github.io/sf/reference/Ops.md)
  : Arithmetic operators for simple feature geometries

- [`aggregate(`*`<sf>`*`)`](https://r-spatial.github.io/sf/reference/aggregate.sf.md)
  :

  aggregate an `sf` object

- [`rbind(`*`<sf>`*`)`](https://r-spatial.github.io/sf/reference/bind.md)
  [`cbind(`*`<sf>`*`)`](https://r-spatial.github.io/sf/reference/bind.md)
  [`st_bind_cols()`](https://r-spatial.github.io/sf/reference/bind.md) :
  Bind rows (features) of sf objects

- [`as_Spatial()`](https://r-spatial.github.io/sf/reference/coerce-methods.md)
  :

  Methods to coerce simple features to `Spatial*` and
  `Spatial*DataFrame` objects

- [`dbDataType(`*`<PostgreSQLConnection>`*`,`*`<sf>`*`)`](https://r-spatial.github.io/sf/reference/dbDataType.md)
  [`dbDataType(`*`<DBIObject>`*`,`*`<sf>`*`)`](https://r-spatial.github.io/sf/reference/dbDataType.md)
  : Determine database type for R vector

- [`dbWriteTable(`*`<PostgreSQLConnection>`*`,`*`<character>`*`,`*`<sf>`*`)`](https://r-spatial.github.io/sf/reference/dbWriteTable.md)
  [`dbWriteTable(`*`<DBIObject>`*`,`*`<character>`*`,`*`<sf>`*`)`](https://r-spatial.github.io/sf/reference/dbWriteTable.md)
  :

  Write `sf` object to Database

- [`db_drivers`](https://r-spatial.github.io/sf/reference/db_drivers.md)
  :

  Drivers for which update should be `TRUE` by default

- [`extension_map`](https://r-spatial.github.io/sf/reference/extension_map.md)
  : Map extension to driver

- [`gdal_addo()`](https://r-spatial.github.io/sf/reference/gdal_addo.md)
  : Add or remove overviews to/from a raster image

- [`gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.md)
  : Native interface to gdal utils

- [`st_intersection()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.md)
  [`st_difference()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.md)
  [`st_sym_difference()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.md)
  [`st_snap()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.md)
  : Geometric operations on pairs of simple feature geometry sets

- [`st_intersects()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  [`st_disjoint()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  [`st_touches()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  [`st_crosses()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  [`st_within()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  [`st_contains()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  [`st_contains_properly()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  [`st_overlaps()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  [`st_equals()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  [`st_covers()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  [`st_covered_by()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  [`st_equals_exact()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  [`st_is_within_distance()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
  : Geometric binary predicates on pairs of simple feature geometry sets

- [`st_combine()`](https://r-spatial.github.io/sf/reference/geos_combine.md)
  [`st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.md)
  : Combine or union feature geometries

- [`st_area()`](https://r-spatial.github.io/sf/reference/geos_measures.md)
  [`st_length()`](https://r-spatial.github.io/sf/reference/geos_measures.md)
  [`st_perimeter()`](https://r-spatial.github.io/sf/reference/geos_measures.md)
  [`st_distance()`](https://r-spatial.github.io/sf/reference/geos_measures.md)
  : Compute geometric measurements

- [`st_dimension()`](https://r-spatial.github.io/sf/reference/geos_query.md)
  [`st_is_simple()`](https://r-spatial.github.io/sf/reference/geos_query.md)
  [`st_is_empty()`](https://r-spatial.github.io/sf/reference/geos_query.md)
  : Dimension, simplicity, validity or is_empty queries on simple
  feature geometries

- [`st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_boundary()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_convex_hull()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_concave_hull()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_simplify()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_triangulate()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_triangulate_constrained()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_inscribed_circle()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_minimum_rotated_rectangle()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_minimum_bounding_circle()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_voronoi()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_polygonize()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_line_merge()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_centroid()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_reverse()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_node()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_segmentize()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  [`st_exterior_ring()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
  : Geometric unary operations on simple feature geometry sets

- [`st_interpolate_aw()`](https://r-spatial.github.io/sf/reference/interpolate_aw.md)
  : Areal-weighted interpolation of polygon data

- [`is_driver_available()`](https://r-spatial.github.io/sf/reference/is_driver_available.md)
  : Check if driver is available

- [`is_driver_can()`](https://r-spatial.github.io/sf/reference/is_driver_can.md)
  : Check if a driver can perform an action

- [`is_geometry_column()`](https://r-spatial.github.io/sf/reference/is_geometry_column.md)
  : Check if the columns could be of a coercable type for sf

- [`merge(`*`<sf>`*`)`](https://r-spatial.github.io/sf/reference/merge.sf.md)
  : merge method for sf and data.frame object

- [`nc`](https://r-spatial.github.io/sf/reference/nc.md) : North
  Carolina SIDS data

- [`plot(`*`<sf>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  [`get_key_pos()`](https://r-spatial.github.io/sf/reference/plot.md)
  [`plot(`*`<sfc_POINT>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  [`plot(`*`<sfc_MULTIPOINT>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  [`plot(`*`<sfc_LINESTRING>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  [`plot(`*`<sfc_CIRCULARSTRING>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  [`plot(`*`<sfc_MULTILINESTRING>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  [`plot(`*`<sfc_POLYGON>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  [`plot(`*`<sfc_MULTIPOLYGON>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  [`plot(`*`<sfc_GEOMETRYCOLLECTION>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  [`plot(`*`<sfc_GEOMETRY>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  [`plot(`*`<sfg>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  [`plot_sf()`](https://r-spatial.github.io/sf/reference/plot.md)
  [`sf.colors()`](https://r-spatial.github.io/sf/reference/plot.md)
  [`text(`*`<sf>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  [`text(`*`<sfc>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  [`points(`*`<sf>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  [`points(`*`<sfc>`*`)`](https://r-spatial.github.io/sf/reference/plot.md)
  : plot sf object

- [`prefix_map`](https://r-spatial.github.io/sf/reference/prefix_map.md)
  : Map prefix to driver

- [`sf_proj_search_paths()`](https://r-spatial.github.io/sf/reference/proj_tools.md)
  [`sf_proj_network()`](https://r-spatial.github.io/sf/reference/proj_tools.md)
  [`sf_proj_pipelines()`](https://r-spatial.github.io/sf/reference/proj_tools.md)
  : Manage PROJ settings

- [`rawToHex()`](https://r-spatial.github.io/sf/reference/rawToHex.md) :
  Convert raw vector(s) into hexadecimal character string(s)

- [`sf_use_s2()`](https://r-spatial.github.io/sf/reference/s2.md)
  [`st_as_s2()`](https://r-spatial.github.io/sf/reference/s2.md) :
  functions for spherical geometry, using s2 package

- [`st_sf()`](https://r-spatial.github.io/sf/reference/sf.md)
  [`` `[`( ``*`<sf>`*`)`](https://r-spatial.github.io/sf/reference/sf.md)
  [`print(`*`<sf>`*`)`](https://r-spatial.github.io/sf/reference/sf.md)
  : Create sf object

- [`sf_extSoftVersion()`](https://r-spatial.github.io/sf/reference/sf_extSoftVersion.md)
  : Provide the external dependencies versions of the libraries linked
  to sf

- [`sf_add_proj_units()`](https://r-spatial.github.io/sf/reference/sf_project.md)
  [`sf_project()`](https://r-spatial.github.io/sf/reference/sf_project.md)
  : directly transform a set of coordinates

- [`st_sfc()`](https://r-spatial.github.io/sf/reference/sfc.md)
  [`` `[`( ``*`<sfc>`*`)`](https://r-spatial.github.io/sf/reference/sfc.md)
  : Create simple feature geometry list column

- [`print(`*`<sgbp>`*`)`](https://r-spatial.github.io/sf/reference/sgbp.md)
  [`t(`*`<sgbp>`*`)`](https://r-spatial.github.io/sf/reference/sgbp.md)
  [`as.matrix(`*`<sgbp>`*`)`](https://r-spatial.github.io/sf/reference/sgbp.md)
  [`dim(`*`<sgbp>`*`)`](https://r-spatial.github.io/sf/reference/sgbp.md)
  [`Ops(`*`<sgbp>`*`)`](https://r-spatial.github.io/sf/reference/sgbp.md)
  [`as.data.frame(`*`<sgbp>`*`)`](https://r-spatial.github.io/sf/reference/sgbp.md)
  : Methods for dealing with sparse geometry binary predicate lists

- [`st_point()`](https://r-spatial.github.io/sf/reference/st.md)
  [`st_multipoint()`](https://r-spatial.github.io/sf/reference/st.md)
  [`st_linestring()`](https://r-spatial.github.io/sf/reference/st.md)
  [`st_polygon()`](https://r-spatial.github.io/sf/reference/st.md)
  [`st_multilinestring()`](https://r-spatial.github.io/sf/reference/st.md)
  [`st_multipolygon()`](https://r-spatial.github.io/sf/reference/st.md)
  [`st_geometrycollection()`](https://r-spatial.github.io/sf/reference/st.md)
  [`print(`*`<sfg>`*`)`](https://r-spatial.github.io/sf/reference/st.md)
  [`head(`*`<sfg>`*`)`](https://r-spatial.github.io/sf/reference/st.md)
  [`format(`*`<sfg>`*`)`](https://r-spatial.github.io/sf/reference/st.md)
  [`c(`*`<sfg>`*`)`](https://r-spatial.github.io/sf/reference/st.md)
  [`as.matrix(`*`<sfg>`*`)`](https://r-spatial.github.io/sf/reference/st.md)
  : Create simple feature from a numeric vector, matrix or list

- [`NA_agr_`](https://r-spatial.github.io/sf/reference/st_agr.md)
  [`st_agr()`](https://r-spatial.github.io/sf/reference/st_agr.md)
  [`` `st_agr<-`() ``](https://r-spatial.github.io/sf/reference/st_agr.md)
  [`st_set_agr()`](https://r-spatial.github.io/sf/reference/st_agr.md) :

  get or set relation_to_geometry attribute of an `sf` object

- [`st_as_binary()`](https://r-spatial.github.io/sf/reference/st_as_binary.md)
  : Convert sfc object to an WKB object

- [`st_as_grob()`](https://r-spatial.github.io/sf/reference/st_as_grob.md)
  : Convert sf\* object to a grob

- [`st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.md) :
  Convert foreign object to an sf object

- [`st_as_sfc()`](https://r-spatial.github.io/sf/reference/st_as_sfc.md)
  : Convert foreign geometry object to an sfc object

- [`st_as_text()`](https://r-spatial.github.io/sf/reference/st_as_text.md)
  : Return Well-known Text representation of simple feature geometry or
  coordinate reference system

- [`is.na(`*`<bbox>`*`)`](https://r-spatial.github.io/sf/reference/st_bbox.md)
  [`st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.md)
  [`NA_bbox_`](https://r-spatial.github.io/sf/reference/st_bbox.md)
  [`FULL_bbox_`](https://r-spatial.github.io/sf/reference/st_bbox.md)
  [`format(`*`<bbox>`*`)`](https://r-spatial.github.io/sf/reference/st_bbox.md)
  : Return bounding of a simple feature or simple feature set

- [`st_break_antimeridian()`](https://r-spatial.github.io/sf/reference/st_break_antimeridian.md)
  : Break antimeridian for plotting not centred on Greenwich

- [`st_cast()`](https://r-spatial.github.io/sf/reference/st_cast.md) :
  Cast geometry to another type: either simplify, or cast explicitly

- [`st_cast_sfc_default()`](https://r-spatial.github.io/sf/reference/st_cast_sfc_default.md)
  : Coerce geometry to MULTI\* geometry

- [`st_collection_extract()`](https://r-spatial.github.io/sf/reference/st_collection_extract.md)
  :

  Given an object with geometries of type `GEOMETRY` or
  `GEOMETRYCOLLECTION`, return an object consisting only of elements of
  the specified type.

- [`st_coordinates()`](https://r-spatial.github.io/sf/reference/st_coordinates.md)
  : retrieve coordinates in matrix form

- [`st_crop()`](https://r-spatial.github.io/sf/reference/st_crop.md) :
  crop an sf object to a specific rectangle

- [`st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.md)
  [`` `st_crs<-`() ``](https://r-spatial.github.io/sf/reference/st_crs.md)
  [`st_set_crs()`](https://r-spatial.github.io/sf/reference/st_crs.md)
  [`NA_crs_`](https://r-spatial.github.io/sf/reference/st_crs.md)
  [`is.na(`*`<crs>`*`)`](https://r-spatial.github.io/sf/reference/st_crs.md)
  [`` `$`( ``*`<crs>`*`)`](https://r-spatial.github.io/sf/reference/st_crs.md)
  [`format(`*`<crs>`*`)`](https://r-spatial.github.io/sf/reference/st_crs.md)
  [`st_axis_order()`](https://r-spatial.github.io/sf/reference/st_crs.md)
  : Retrieve coordinate reference system from object

- [`st_drivers()`](https://r-spatial.github.io/sf/reference/st_drivers.md)
  : Get GDAL drivers

- [`st_geometry()`](https://r-spatial.github.io/sf/reference/st_geometry.md)
  [`` `st_geometry<-`() ``](https://r-spatial.github.io/sf/reference/st_geometry.md)
  [`st_set_geometry()`](https://r-spatial.github.io/sf/reference/st_geometry.md)
  [`st_drop_geometry()`](https://r-spatial.github.io/sf/reference/st_geometry.md)
  : Get, set, replace or rename geometry from an sf object

- [`st_geometry_type()`](https://r-spatial.github.io/sf/reference/st_geometry_type.md)
  : Return geometry type of an object

- [`st_graticule()`](https://r-spatial.github.io/sf/reference/st_graticule.md)
  : Compute graticules and their parameters

- [`st_is()`](https://r-spatial.github.io/sf/reference/st_is.md) : test
  equality between the geometry type and a class or set of classes

- [`st_is_full()`](https://r-spatial.github.io/sf/reference/st_is_full.md)
  : predicate whether a geometry is equal to a POLYGON FULL

- [`st_is_longlat()`](https://r-spatial.github.io/sf/reference/st_is_longlat.md)
  : Assert whether simple feature coordinates are longlat degrees

- [`st_jitter()`](https://r-spatial.github.io/sf/reference/st_jitter.md)
  : jitter geometries

- [`st_join()`](https://r-spatial.github.io/sf/reference/st_join.md)
  [`st_filter()`](https://r-spatial.github.io/sf/reference/st_join.md) :
  spatial join, spatial filter

- [`st_layers()`](https://r-spatial.github.io/sf/reference/st_layers.md)
  : Return properties of layers in a datasource

- [`st_line_project()`](https://r-spatial.github.io/sf/reference/st_line_project_point.md)
  [`st_line_interpolate()`](https://r-spatial.github.io/sf/reference/st_line_project_point.md)
  : Project point on linestring, interpolate along a linestring

- [`st_line_sample()`](https://r-spatial.github.io/sf/reference/st_line_sample.md)
  : Sample points on a linear geometry

- [`is.na(`*`<m_range>`*`)`](https://r-spatial.github.io/sf/reference/st_m_range.md)
  [`st_m_range()`](https://r-spatial.github.io/sf/reference/st_m_range.md)
  [`NA_m_range_`](https://r-spatial.github.io/sf/reference/st_m_range.md)
  : Return 'm' range of a simple feature or simple feature set

- [`st_make_grid()`](https://r-spatial.github.io/sf/reference/st_make_grid.md)
  : Create a regular tesselation over the bounding box of an sf or sfc
  object

- [`st_nearest_feature()`](https://r-spatial.github.io/sf/reference/st_nearest_feature.md)
  : get index of nearest feature

- [`st_nearest_points()`](https://r-spatial.github.io/sf/reference/st_nearest_points.md)
  : get nearest points between pairs of geometries

- [`st_normalize()`](https://r-spatial.github.io/sf/reference/st_normalize.md)
  : Normalize simple features

- [`st_precision()`](https://r-spatial.github.io/sf/reference/st_precision.md)
  [`st_set_precision()`](https://r-spatial.github.io/sf/reference/st_precision.md)
  [`` `st_precision<-`() ``](https://r-spatial.github.io/sf/reference/st_precision.md)
  : Get precision

- [`st_read()`](https://r-spatial.github.io/sf/reference/st_read.md)
  [`read_sf()`](https://r-spatial.github.io/sf/reference/st_read.md) :
  Read simple features or layers from file or database

- [`st_relate()`](https://r-spatial.github.io/sf/reference/st_relate.md)
  : Compute DE9-IM relation between pairs of geometries, or match it to
  a given pattern

- [`st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.md)
  : sample points on or in (sets of) spatial features

- [`st_shift_longitude()`](https://r-spatial.github.io/sf/reference/st_shift_longitude.md)
  : Shift or re-center geographical coordinates for a Pacific view

- [`st_can_transform()`](https://r-spatial.github.io/sf/reference/st_transform.md)
  [`st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.md)
  [`st_wrap_dateline()`](https://r-spatial.github.io/sf/reference/st_transform.md)
  [`sf_proj_info()`](https://r-spatial.github.io/sf/reference/st_transform.md)
  : Transform or convert coordinates of simple feature

- [`st_viewport()`](https://r-spatial.github.io/sf/reference/st_viewport.md)
  : Create viewport from sf, sfc or sfg object

- [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  [`write_sf()`](https://r-spatial.github.io/sf/reference/st_write.md)
  [`st_delete()`](https://r-spatial.github.io/sf/reference/st_write.md)
  : Write simple features object to file or database

- [`is.na(`*`<z_range>`*`)`](https://r-spatial.github.io/sf/reference/st_z_range.md)
  [`st_z_range()`](https://r-spatial.github.io/sf/reference/st_z_range.md)
  [`NA_z_range_`](https://r-spatial.github.io/sf/reference/st_z_range.md)
  : Return 'z' range of a simple feature or simple feature set

- [`st_zm()`](https://r-spatial.github.io/sf/reference/st_zm.md) : Drop
  or add Z and/or M dimensions from feature geometries

- [`summary(`*`<sfc>`*`)`](https://r-spatial.github.io/sf/reference/summary.sfc.md)
  : Summarize simple feature column

- [`type_sum.sfc()`](https://r-spatial.github.io/sf/reference/tibble.md)
  [`obj_sum.sfc()`](https://r-spatial.github.io/sf/reference/tibble.md)
  [`pillar_shaft.sfc()`](https://r-spatial.github.io/sf/reference/tibble.md)
  : Summarize simple feature type for tibble

- [`filter.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`arrange.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`group_by.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`ungroup.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`rowwise.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`mutate.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`transmute.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`select.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`rename.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`rename_with.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`slice.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`summarise.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`distinct.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`gather.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`pivot_longer.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`pivot_wider.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`spread.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`sample_n.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`sample_frac.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`group_split.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`nest.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`separate.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`separate_rows.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`unite.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`unnest.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`drop_na.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`inner_join.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`left_join.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`right_join.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`full_join.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`semi_join.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  [`anti_join.sf()`](https://r-spatial.github.io/sf/reference/tidyverse.md)
  : Tidyverse methods for sf objects

- [`transform(`*`<sf>`*`)`](https://r-spatial.github.io/sf/reference/transform.sf.md)
  : transform method for sf objects

- [`st_is_valid()`](https://r-spatial.github.io/sf/reference/valid.md)
  [`st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.md)
  : Check validity or make an invalid geometry valid

- [`vec_ptype2.sfc()`](https://r-spatial.github.io/sf/reference/vctrs.md)
  [`vec_cast.sfc()`](https://r-spatial.github.io/sf/reference/vctrs.md)
  : vctrs methods for sf objects
