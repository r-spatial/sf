# version 0.3-5

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

