# Create simple feature geometry list column

Create simple feature geometry list column, set class, and add
coordinate reference system and precision. For data.frame alternatives
see [`st_sf()`](https://r-spatial.github.io/sf/reference/sf.md). To
convert a foreign object to `sfc`, see
[`st_as_sfc()`](https://r-spatial.github.io/sf/reference/st_as_sfc.md)

## Usage

``` r
st_sfc(
  ...,
  crs = NA_crs_,
  precision = 0,
  check_ring_dir = FALSE,
  dim,
  recompute_bbox = FALSE,
  oriented = NA
)

# S3 method for class 'sfc'
x[i, j, ..., op = st_intersects]
```

## Arguments

- ...:

  zero or more simple feature geometries (objects of class `sfg`), or a
  single list of such objects; `NULL` values will get replaced by empty
  geometries.

- crs:

  coordinate reference system: integer with the EPSG code, or character
  with proj4string

- precision:

  numeric; see
  [st_as_binary](https://r-spatial.github.io/sf/reference/st_as_binary.md)

- check_ring_dir:

  see [st_read](https://r-spatial.github.io/sf/reference/st_read.md)

- dim:

  character; if this function is called without valid geometries, this
  argument may carry the right dimension to set empty geometries

- recompute_bbox:

  logical; use `TRUE` to force recomputation of the bounding box

- oriented:

  logical; if `TRUE`, the ring is oriented such that left of the edges
  is inside the polygon; this is needed for convering polygons larger
  than half the globe to s2

- x:

  object of class `sfc`

- i:

  record selection. Might also be an `sfc`/`sf` object to work with the
  `op` argument

- j:

  ignored if `op` is specified

- op:

  function, geometrical binary predicate function to apply when `i` is a
  `sf`/`sfc` object. Additional arguments can be specified using `...`,
  see examples.

## Value

an object of class `sfc`, which is a classed list-column with simple
feature geometries.

## Details

A simple feature geometry list-column is a list of class
`c("stc_TYPE", "sfc")` which most often contains objects of identical
type; in case of a mix of types or an empty set, `TYPE` is set to the
superclass `GEOMETRY`.

if `x` has a `dim` attribute (i.e. is an `array` or `matrix`) then `op`
cannot be used.

## Examples

``` r
pt1 = st_point(c(0,1))
pt2 = st_point(c(1,1))
(sfc = st_sfc(pt1, pt2))
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 1 xmax: 1 ymax: 1
#> CRS:           NA
#> POINT (0 1)
#> POINT (1 1)
sfc[sfc[1], op = st_is_within_distance, dist = 0.5]
#> Geometry set for 1 feature 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 1 xmax: 0 ymax: 1
#> CRS:           NA
#> POINT (0 1)
d = st_sf(data.frame(a=1:2, geom=sfc))
```
