# version 0.4-1

* `st_read` now respects time that is read as UTC

* `st_write` now writes time always as UTC, since GDAL does not have a mechanism to define local timezones other than "unkown" or "local"

* `st_length` now works for POINT and MULTIPOINT (returning 0); POLYGON and MULTIPOLYGON are converted to MULTILINESTRING before computing length, thus giving polygon perimeter (#268)

* `st_write` now has `update` depend on driver; now, for databases, the default is `TRUE`, otherwise `FALSE` (it refers to update of the database, and not to overwriting the table in the database, this will by default not succeed); #274

* `st_read` supports reading objects with multiple geometry columns #257 #255

* support writing (exporting) objects with non-standard columns, such as `units` or `POSIXlt` #264

* catch dependencies on GEOS 3.3.5 (hence no 0.4-0 CRAN binary for MacOSX) #260

# version 0.4-0

* have `st_is_valid` catch corrupt geometries too, returning `NA` in that case (requiring GEOS 3.5.0)

* add `st_make_valid`, only working if sf was linked to `liblwgeom`

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

* faster conversion of `data.frame` into `POINT` `sf` object, using `st_as_sf` (Mike Sumner)

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

* allow for 3D coordinates returned, when `+proj=geocent` (#172, #103)

* fix `NA_integer_` handling in shapefiles I/O (#184)

* add and fix `st_agr` API, to set and get attribute-to-geometry relationships

# version 0.2-8, Jan 5, 2017

