# Check validity or make an invalid geometry valid

Checks whether a geometry is valid, or makes an invalid geometry valid

## Usage

``` r
st_is_valid(x, ...)

# S3 method for class 'sfc'
st_is_valid(x, ..., NA_on_exception = TRUE, reason = FALSE)

# S3 method for class 'sf'
st_is_valid(x, ...)

# S3 method for class 'sfg'
st_is_valid(x, ...)

st_make_valid(x, ...)

# S3 method for class 'sfg'
st_make_valid(x, ...)

# S3 method for class 'sfc'
st_make_valid(
  x,
  ...,
  oriented = FALSE,
  s2_options = s2::s2_options(snap = s2::s2_snap_precision(1e+07), ...),
  geos_method = "valid_structure",
  geos_keep_collapsed = TRUE
)
```

## Arguments

- x:

  object of class `sfg`, `sfc` or `sf`

- ...:

  passed on to
  [s2_options](https://r-spatial.github.io/s2/reference/s2_options.html)

- NA_on_exception:

  logical; if TRUE, for polygons that would otherwise raise a GEOS error
  (exception, e.g. for a POLYGON having more than zero but less than 4
  points, or a LINESTRING having one point) return an `NA` rather than
  raising an error, and suppress warning messages (e.g. about
  self-intersection); if FALSE, regular GEOS errors and warnings will be
  emitted.

- reason:

  logical; if `TRUE`, return a character with, for each geometry, the
  reason for invalidity, `NA` on exception, or `"Valid Geometry"`
  otherwise.

- oriented:

  logical; only relevant if `st_is_longlat(x)` is `TRUE`; see
  [s2](https://r-spatial.github.io/sf/reference/s2.md)

- s2_options:

  only relevant if `st_is_longlat(x)` is `TRUE`; options for
  [s2_rebuild](https://r-spatial.github.io/s2/reference/s2_boundary.html),
  see
  [s2_options](https://r-spatial.github.io/s2/reference/s2_options.html)
  and Details.

- geos_method:

  character; either "valid_linework" (Original method, combines all
  rings into a set of noded lines and then extracts valid polygons from
  that linework) or "valid_structure" (Structured method, first makes
  all rings valid then merges shells and subtracts holes from shells to
  generate valid result. Assumes that holes and shells are correctly
  categorized.) (requires GEOS \>= 3.10.1)

- geos_keep_collapsed:

  logical; When this parameter is not set to `FALSE`, the
  "valid_structure" method will keep any component that has collapsed
  into a lower dimensionality. For example, a ring collapsing to a line,
  or a line collapsing to a point (requires GEOS \>= 3.10.1)

## Value

`st_is_valid` returns a logical vector indicating for each geometries of
`x` whether it is valid. `st_make_valid` returns an object with a
topologically valid geometry.

Object of the same class as `x`

## Details

For projected geometries, `st_make_valid` uses the `lwgeom_makevalid`
method also used by the PostGIS command `ST_makevalid` if the GEOS
version linked to is smaller than 3.8.0, and otherwise the version
shipped in GEOS; for geometries having ellipsoidal coordinates
[`s2::s2_rebuild`](https://r-spatial.github.io/s2/reference/s2_boundary.html)
is being used.

if `s2_options` is not specified and `x` has a non-zero precision set,
then this precision value will be used as the value in
`s2_snap_precision`, passed on to `s2_options`, rather than the 1e7
default.

## Examples

``` r
p1 = st_as_sfc("POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))")
st_is_valid(p1)
#> [1] FALSE
st_is_valid(st_sfc(st_point(0:1), p1[[1]]), reason = TRUE)
#> [1] "Valid Geometry"         "Self-intersection[5 5]"
library(sf)
x = st_sfc(st_polygon(list(rbind(c(0,0),c(0.5,0),c(0.5,0.5),c(0.5,0),c(1,0),c(1,1),c(0,1),c(0,0)))))
suppressWarnings(st_is_valid(x))
#> [1] FALSE
y = st_make_valid(x)
st_is_valid(y)
#> [1] TRUE
y %>% st_cast()
#> Geometry set for 1 feature 
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 1 ymax: 1
#> CRS:           NA
#> POLYGON ((0 0, 0 1, 1 1, 1 0, 0.5 0, 0 0))
```
