# version 0.7-3

* fixed n-ary `st_difference` for cases where geometries are entirely contained in others; #975, by Jonathan Marshall

* faster `Ops.sfc`, added `st_normalize`; #973 by Thomas Lin Pedersen

* new grob constructor for sfc objects; #971 by Thomas Lin Pedersen (now contributor)

* add `group_split` and `group_map` methods for `sf` objects (experimental); #969

* make `st_interpolate_aw` a generic;

* argument `col` for `plot` of `GEOMETRY` `sfc`'s now is `NA` (open) for (multi) polygon geometries

# version 0.7-2

* feature IDs are no longer returned as names on the geometry list column, but optionally returned by `st_read` as attribute column; #812

* when plotting multiple attributes, plot.sf now adds a (single, common) key if `key.pos` is set

* precision can now be specified in distance units; #901

* support log-scale in color legend by setting `logz` to `TRUE` in `plot.sf`

* `st_intersects` etc. will prepare `y` when `y` is polygons and `x` is points; #885 by Dan Baston

* `st_write` (and `write_sf`) now returns its first argument, invisibly; #889

# version 0.7-1

* fix bug that broke n-ary `st_intersection` on platforms using clang; #867

# version 0.7-0

* adds several interfaces to GDAL functions, meant to be used by package `stars`

* `st_read` receives a `query` argument that can run queries against OGR datasets; #834, by Barry Rowlingson and Michael Sumner

* `read_sf` no longer first creates tibbles from `data.frame`s, but creates them directly; #853, db propagation by Etienne Racine

* check difference between compile-time and run-time GEOS versions; #844

* all GEOS routines are now (more) robust against memory leaks, by using unique pointers; #822, #845, by Dan Baston

* `st_buffer` receives the buffer styles `endCapStyle`, `joinStyle` and `mitreLimit`; #833, #842 by Michael Sumner

# version 0.6-4

* `st_area` is now a generic; https://github.com/r-spatial/stars/issues/32

* `st_write` now resolves `~` correctly; #456

* read and write feature IDs as sfc list column names; #812

* `st_centroid` now works for empty geometries, returning an empty point #769

* add `st_nearest_points`, to obtain the (`LINESTRING` connecting the) two nearest points for pairs of geometries; #788

* add hexagonal tiling to `st_make_grid`

* add regular and hexagonal sampling to `st_sample`

* fixes for PROJ 5.0.1; #545

* fixes for GDAL 2.3.0; #759

* `st_sample` supports regular sampling of `LINESTRING`; #725 by @statnmap 

* Support reading and writing of database `Pool` objects; #756

* fix plotting of `sf` objects without attributes; #755

* add reference to the [R Journal article](https://journal.r-project.org/archive/2018/RJ-2018-009/index.html) in CITATION

# version 0.6-3

* move dependency `RPostgreSQL` from Imports: back to Suggests:

* `st_centroid.sf` and `st_point_on_surface.sf` now also warn if attributes are not constant over geometries.

* `summarise` now allows the user to define geometries for summaries; #714, by Kirill Mueller

* `plot.sf` now emits a warning if `col` does not have length 1 or `nrow(x)`, and requires `pal` (rather than `col`) to set a palette for factors.

* `plot.sf` provides control over legend keys using `key.length` and `key.width`, decrease default key length; #731

* `sgbp` objects receive an `as.data.frame` method; #715

# version 0.6-2

* GDAL read/write now supports logical variables; #722

* add `st_crop` to simplify cropping objects with a rectangular area; #720

* fix bug in `[<-` when columns are added to an `sf` object; #718

* use dynamic registration of S3 methods, similar to how hms does this; #710 by Kirill Mueller

* (partially) address writing GPKG to network drive, writing to temp file first; #628

* add Kirill Mueller as contributor

* `st_make_grid` is now faster; #708, by Dan Baston

* `st_read` and `st_write` are now generic, with methods for directly reading from and writing to database connections; `st_read_db` and `st_write_db` are now deprecated; #558, thanks to Etienne Racine @etiennebr

* Package `RPostgreSQL` moved from Suggests to Imports

* restore compatibility with GDAL 2.0.x versions (which won't have `gdal_utils`); #686

* `read_sf` can now also read tables without geometry; #684, by Andy Teucher

# version 0.6-1

* method `distinct` now works; #669, #672

* `+`, `-`, `*` and `/` for pairs of geometries (`sfg`, `sfc`) now return geometric union, difference, intersection and symmetric difference, respectively.

* `st_cast` from `MULTIPOLYGON` to `MULTILINESTRING` should now work properly; #660

* all Rcpp interfaces needed by package `stars` have now been moved into `sf`; pkg `stars` is now R-only, and only `sf` needs linking to GDAL.

* `gdal_utils()` now interfaces the 9 gdal utils using the C++ API

* improve resetting (base) plots; add `reset = FALSE` in a call to `plot` to enable adding to plots that have a legend

* `st_read` now returns a `data.frame` when a table contains no geometries, rather than giving an error; it does emit a warning in this case. See https://stat.ethz.ch/pipermail/r-sig-geo/2018-February/026344.html

* move `pillar` from `Imports:` to `Suggests:`

* update to the new rwinlib distribution of gdal (adds JPG2000); #639

* speed up computation of centroids for largest polygon; #623

* add `st_as_sfc.raw` method

* Bugfix: binary operations (`st_intersection`, `st_difference`, etc) no longer fail when operating on data frames of class `"tbl_df"` with common column names; #644

# version 0.6-0

* add `pillar` to Imports: to provide method for printing WKT geometries in tibbles

* `st_as_text`, and subsequently `format` and `print`, now use argument `digits` (or `options(digits = n)`) to control the number of digits used for printing coordinates; default is now `options("digits")`, which is typically 7.

* `st_is_within_distance` now works with geographic coordinates

* `st_cast` from `MULTIPOLYGON` to `MULTILINESTRING` no longer changes the number of features/feature geometries, but conversion from `MULTIPOLYGON` to `LINESTRING` now (typically) does; #596

* `st_distance` for long/lat geographic coordinates now uses `lwgeom`, accepting all geometry types; argument `dist_fun` is deprecated as a consequence, and distance calculations are different from those in sf versions 0.5-5 or earlier; #593

* add package `lwgeom` to Suggests; `st_area`, `st_length`, `st_distance`, `st_segmentize` for long/lat CRS now use package `lwgeom` instead of `geosphere`; #593

* `st_length` now returns zero for polygon-type geometries; #593

* if present, add units of attribute to default plot title; #591

* add `unnest` method, which depends on `tidyr` > 0.7-2; #570 PR by @karldw

* add `largest` option to `st_join` to get largest intersection match only; #547, by @tiernanmartin

* change default maximum number of feature to print to 10, controllable by `options(sf_max_print)`; #556

* add `Hausdorff` (and `Frechet` for those with GEOS 3.7.0) as options to `st_distance`; add `par` for densified versions

* add `st_snap`, for snapping geometries to other geometries, within a tolerance

* make `st_wrap_dateline` a generic, with methods for `sf`, `sfc` and `sfg`; #541

* `plot.sf` and `st_as_grob` (used by ggplot2) are now robust against misspecified ring directions (holes that have the same direction as the exterior rings), by using `rule = "evenodd"`; #540

* functions depending on `liblwgeom` (`st_make_valid`, `st_geohash`, `st_plit`) have been moved to their own package, https://github.com/r-spatial/lwgeom; argument `use_gdal` of `st_transform` has been deprecated, instead one can now use `lwgeom::st_transform_proj`; sf now no longer tries to link to liblwgeom; #509, #537, #487

* `st_read`, `st_sf` and `st_sfc` gain a parameter `check_ring_dir` (default: `FALSE`) that checks ring directions and corrects to: exterior counter clockwise, holes clockwise, when seen from above.

* get rid of `classInt::classIntervals` warning if number of unique values is smaller than the number of breaks asked for

# version 0.5-5

* have `classInt` in Imports:, to not break other package checks

* add vignettes 5: plotting sf objects and 6: miscellaneous; #324

* add (default) color key to `plot.sf` if single map is plotted, contributed by @hughjonesd; #528 

* `st_as_sfc` can now read EWKT; #530

* argument `max.plot` takes its default from `options(sf_max.plot=n)`, if present; #516

* `plot.sf` gets an arguments `pal` to specify a color palette function; #526

* `plot.sf` gets arguments `breaks` and `nbreaks`; add support for `classInt::classIntervals` styles for finding class intervals (using `breaks`)

* add `st_as_sf` methods for `ppp`, `lpp` and `psp` objects from spatstat.

* allow for direct route to proj.4 ignoring GDAL (requiring liblwgeom); #509, #511

* add `print` method for `crs` objects; #517

* `sf_extSoftVersion` now reveals whether GDAL was linked to GEOS; #510

* better check input of `st_polygon`; #514

* add `st_node`, similar to `rgeos::gNode`

* support for reading `OFTInteger64List` fields; #508

* sparse geometric binary predicate lists now have a class, `sgbp`, and attributes `region.id` and `predicate`; #234, #524

* prevent `st_split` from stopping the R session; #492

* `st_intersection`, `st_union` and so on now also print a message when used directly on long/lat coordinates; #496

* add `rep` method for `sfc` objects

* comparing two `crs` objects now uses the GDAL function `IsSame`; #180

* add `st_collection_extract`, which, given an object with geometries of type `GEOMETRY` or `GEOMETRYCOLLECTION`, returns an object consisting only of elements of the specified type; by Andy Teucher, #482

* `st_write` exports GeoJSON with UTF-8 encoding on Windows; #444

* move package methods from Imports: to Depends: ; #478

* deal better with precision setting and propagation; #476

* fix bug in `st_layers` in case layers have no geometry; #334

* clarify argument `envelope` in `st_voronoi`; #474

* change aggregate to make it return the same geometry as 'by', padding attributes with NA where needed; #453 

# version 0.5-4

* fix compatibility problems introduced by `tidyr` 0.7-0 using rlang magic

* convert path names to UTF-8 in `st_read`, `st_write` and `st_layers`; #471

* `st_sfc` converts `NULL` values into empty geometries, and correctly identifies empty `POINT`s; #466, #463

* `st_write` abbreviates column names if driver is `ESRI Shapefile`; #464

* add `of_largest_polygon` argument to `st_centroid`, to get the centroid of the largest polygon; #450

* fix use of `st_relate` as join predicate for `st_join`; #454

* fix bug where `st_intersects` with empty second argument would crash; #458

* produce better WKT; #463

* fix bug in `st_cast.sf`; #461, #462

* change `st_read` SRS assignment logic; corrects reading projected geojson with gdal 2.2.0; #449

* `st_intersection` etc. on `tbl` now also return `tbl`; #448

* `[.sf` preserves class, e.g. of `tbl`; #448

# version 0.5-3

* support and propagate all Proj.4 +units=xx length units; #446

* allow for arith ops on empty `sfc` objects

* have `st_graticule` return an empty graticule object when argument `datum` is `NA`; 

* export `as_Spatial`, to make it easer for packages to convert `sfc` objects without importing `sf`

* `st_distance` gains a parameter `by_element` to obtain pairwise distances; #437

* add the ability to `aggregate` using a simple feature `by` argument; #429

* make the `op` argument to `[.sf` work

* speed up `st_coordinates` for `POINT` geometries; #433

* fix performance regression for `st_bbox`; #418

* correct bug in `st_union`, `st_difference` and `st_sym_difference` introduced in 0.5-2; #431

* inform gdal about the CRS always through the proj4string, never through the epsg; see #424

* properly deal with kilometre units; #424 (fixed by Karl Dunkle Werner)

* add `st_is_within_distance`, only to return a sparse index matrix; #419

* have `st_graticule` work with world2 (0,360); #421, #422, fixed by Ben Best

* `st_graticule` to return graticules in native crs; https://github.com/tidyverse/ggplot2/issues/2200 (WIP)

* `st_graticule` to support data in `NA_crs_`; https://github.com/tidyverse/ggplot2/issues/2199

* fix bug when joining an sf-tibble with a `tibble`; #414

* read gdal `StringList`, `RealList`, and `IntegerList` fields into a list-column; #416

# version 0.5-2

* made ready for rwinlib/gdal2; #408

* make `[.sf` for selections including `NA` values like `x[c(1,NA,2)]`; #403

* add a `[<-` method for `sfc` objects; automatically replaces `NULL` with an empty geometry; #411

* add `st_point_on_surface()` to return a point that is guaranteed to be on the surface (standard compliance)

* `read_sf` now returns an sf-tibble, an object of class `c("sf", "tbl_df", "tbl", "data.frame")`

* work around for `dplyr::filter` not dispatching geometry column subsetting to `sf::[.sfc`

* allow `units` object as `dist` argument to `st_buffer`; these must be convertable to `arc_degree` for geographic, and to a length unit for non-geographic data; #399

* prevent gdal from crashing when trying to `st_transform` an empty geometry; #398

* add `st_as_sfc` method for `bbox`, returning the bbox polygon; #377

* strip file name extension from default layer name in `st_write`; #392

* have `st_sf` replace `NULL` values in an `sfc` list-column with the appropriate empty geometry; #372

* allow setting `ndiscr` through `ggplot2::coords_sf` to improve graticule plotting in `geom_sf`; #396

# version 0.5-1

* add spatial indexes to most binary geometry operations; #394 and http://r-spatial.org/r/2017/06/22/spatial-index.html

* drastically reduce memory footprint of `st_intersection` and similar; #394

* support RSQLite 2.0 by providing an `st_as_sfc` method for list columns of class `blob`

* drop dependency on dbplyr

# version 0.5-0

* better handle empty/NULL geometries in shapefiles; #351

* add `unite_.sf` method

* deprecate `FUN` argument to `st_join`; #376

* improve graticule tic label placement in `ggplot2`; #375 and https://github.com/tidyverse/ggplot2/issues/2119

* improve `configure` logic to deal with libraries installed in custom locations; #335

* fix bug where `geom_sf` wouldn't deal with Z and/or M geoms; #373

* return more conveniently typed empty geoms; #372 

* fix subsetting with `[` of `sf` using `drop = TRUE`, #370

* in addition to `m`, allow `rad` units to `st_segmentize` 

* add example how to `st_read` GeoJSON from a string; #185

* add `separate_.sf` method

* add `st_split` to split geometries (only available if compiled against liblwgeom), #359

* fix bug reading and writing dates (months 1 off): #358

* [.sf and [.sfc now also select on i when i is an `sfg` object, and accept a geometric predicate function with optional arguments; #352

* on reading through GDAL, empty (NULL) geometries no longer result in an error; on creation, they no longer automatically give a `GEOMETRY` object; #351

* on plotting with `ggplot2::geom_sf`, empty geometries no longer break; grid functions return `nullGrob()` for them; #351

* arith operations on empty geometries no longer break or give warnings; #351

* have `st_as_sf.data.frame` by default break on `NA` values in coordinates; #342

* have `st_join` accept further arguments, to be passed on to the `join` function (e.g. a pattern for `st_relate`)

* have WKB reader throw an error on (some) malformed inputs, and check for buffer bounds

# version 0.4-3

* back-port `do_union` argument to dplyr <= 0.5.0, using lazyeval

* all strings returned from OGR/GDAL now get encoding set to `UTF-8`, making them work on non-UTF-8 platforms; #5

* `$.crs` now retrieves proj4string components, such as `st_crs(4326)$datum` in addition to `epsg` and `proj4string`

* let `st_geohash` return geohash for (average) points (only when sf was linked to liblwgeom)

# version 0.4-2

* `summarise.sf` now always returns an `sf` object, also for global (non-grouped) summaries.

* `summarise.sf` gains an argument `do_union` which determines whether to union the geometries for which a summary is given, or to `st_combine` them (not resolving boundaries); #331

* rename argument `union` of `aggregate.sf` into `do_union`, for consistency with `summarise`; #331

* add a `nest_` method for `sf` objects

* `st_relate` gets a `pattern` parameter, same as `rgeos::gRelate`; add examples to get rook and queen neighbour lists using this; #234

* support for direct reading of spatialite and sqlite geometry wkb blobs

* build proper support for `cbind` and `rbind` methods for `sf`, which work (as documented) when _all_ arguments are of class `sf`; `dplyr::bind_cols` or `st_sf(data.frame(sf, df))` work for binding `data.frame`s to an `sf` object.

* `st_segmentize()` and `st_line_sample()` now accept units arguments

* document problem reading shapefiles from USB drives on OSX; #252

* improve docs of `st_is_valid` and `st_make_valid`; #296

* coercing `sf` to `data.frame` now works better; #298

* `st_line_sample` gains argument `sample` to specify the points t.b. sampled; #299 #300 thanks to @joethorley

* add compatibility to upcoming dplyr 0.6.0; #304 #42

* write GDAL fields by name, not by number, fixing a KML problem #308

* `st_write` gains arguments `delete_layer` and `delete_dsn` to allow overwrite capability #307 #274

* `write_sf` defaults to `delete_layer=TRUE`, silently overwriting layers if they're already present

* compatibility with GDAL 2.2beta0; #303; #309

* replace `st_write_db` with a version that is fast for large datasets (#285), thanks to Josh London

* take out more memory leaking examples in tests

* the `aggregate` method for `sf` objects now assumes the `by` argument to be identical to that of `stats::aggregate`

* `st_wrap_dateline` wraps (cuts up) geometries crossing the antimeridian, such that they no longer cross it.

# version 0.4-1

* restore 3.3.0 and c++11 requirement

* `st_read` now respects time that is read as UTC

* `st_write` now writes time always as UTC, since GDAL does not have a mechanism to define local timezones other than "unknown" or "local"

* `st_length` now works for POINT and MULTIPOINT (returning 0); POLYGON and MULTIPOLYGON are converted to MULTILINESTRING before computing length, thus giving polygon perimeter (#268)

* `st_write` now has `update` depend on driver; now, for databases, the default is `TRUE`, otherwise `FALSE` (it refers to update of the database, and not to overwriting the table in the database, this will by default not succeed); #274

* `st_read` supports reading objects with multiple geometry columns #257 #255

* support writing (exporting) objects with non-standard columns, such as `units` or `POSIXlt` #264

* catch dependencies on GEOS 3.3.5 (hence no 0.4-0 CRAN binary for MacOSX) #260

# version 0.4-0

* have `st_is_valid` catch corrupt geometries too, returning `NA` in that case (requiring GEOS 3.5.0)

* add `st_make_valid`, only available when sf was linked to `liblwgeom`

* add `st_coordinates` method, returning coordinates matrix with indexes

* remove `unlist.sfg` 

* add `as.matrix.sfg`; have as.matrix.sfg add indexes to coordinates

* add `st_bind_cols` method

* improve handling features that can't be projected 

* support uniform sampling over polygons on the sphere

* add `st_sample`, for sampling points on multipoints, linestrings, or polygons

* add `c` method for `sfc` objects

* import and export `magrittr::%>%`

* support ggplot'ing geometrycollections

* drop C++11 requirement, allowing build for older R versions

* add `st_proj_info`, modelled after `rgdal::projInfo`

* overwriting datasets with `st_write` is no longer allowed; update=TRUE appends to them, permitted the driver supports appending.

* `st_write` gains an argument, `update`, which when `TRUE` will try to append to existing datasets (#204)

* added list of corresponding function for migration from sp, rgdal and rgeos to sf at https://github.com/edzer/sfr/wiki/migrating

* remove deprecated `st_list`

* rename `st_makegrid` to `st_make_grid`, and `st_linemerge` to `st_line_merge`
* add NEWS.md file (#207)

* faster conversion of `data.frame` into `POINT` `sf` object, using `st_as_sf` (Michael Sumner)

* `rbind` method for `sf` objects now keeps coordinate reference system

# version 0.3-4, Feb 6, 2017

* add `st_contains_properly` spatial predicate

* GEOS functions (geometry operations) now accept XYZ geometries (and ignore Z)

* make `prepared = TRUE` the default for all geometry binary operations

# version 0.3-2, Feb 4, 2017

* add user interrupt checks in all GEOS geometry operations

* make `st_graticule` do something useful for polar projections

* make `st_graticule` return `NA` labels when labels are useless

* add `merge.sf` methods to merge `sf` object and `data.frame` (#193)

* add `st_join` for table joins based on (user-defined) spatial predicates

* add `dplyr`-style non-spatial joins for `sf` objects (`left_join`, `full_join` etc.) (#193)

* allow for multiple non-gathered variables (#196)

* add missing meridian to `st_graticule` (#198)

# version 0.3-1, Jan 31, 2017

* add `merge` method (#193)

* `st_graticule` for laea (#198)

* allow `st_buffer` with feature-dependent buffer distance (#197)

* have `spread` return an `sf` object (#196)

* clarify `overwrite = TRUE` in write docs

* fix `st_as_sf.map` (#194)

* add `prepared` arg to spatial binary predicates, to speed up large intersections

* add `st_voronoi` interface (requires that lib GEOS >= 3.5.0)

* add `st_as_sf` methods for `map` objects (library maps)

* add RStudio project file

* have `st_bbox` return a `bbox` object which has an `st_crs` method

* rename `st_drop_zm` into `st_zm`, for general more handling of Z and M

* allow for 3D coordinates returned, when `+proj=geocent` (#172; #103)

* fix `NA_integer_` handling in shapefiles I/O (#184)

* add and fix `st_agr` API, to set and get attribute-to-geometry relationships

# version 0.2-8, Jan 5, 2017

