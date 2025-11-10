# Combine or union feature geometries

Combine several feature geometries into one, without unioning or
resolving internal boundaries

## Usage

``` r
st_combine(x)

st_union(x, y, ..., by_feature = FALSE, is_coverage = FALSE)
```

## Arguments

- x:

  object of class `sf`, `sfc` or `sfg`

- y:

  object of class `sf`, `sfc` or `sfg` (optional)

- ...:

  ignored

- by_feature:

  logical; if `TRUE`, union each feature if `y` is missing or else each
  pair of features; if `FALSE` return a single feature that is the
  geometric union of the set of features in `x` if `y` is missing, or
  else the unions of each of the elements of the Cartesian product of
  both sets

- is_coverage:

  logical; if `TRUE`, use an optimized algorithm for features that form
  a polygonal coverage (have no overlaps)

## Value

`st_combine` returns a single, combined geometry, with no resolved
boundaries; returned geometries may well be invalid.

If `y` is missing, `st_union(x)` returns a single geometry with resolved
boundaries, else the geometries for all unioned pairs of `x[i]` and
`y[j]`.

## Details

`st_combine` combines geometries without resolving borders, using
[c.sfg](https://r-spatial.github.io/sf/reference/st.md) (analogous to
[c](https://rdrr.io/r/base/c.html) for ordinary vectors).

If `st_union` is called with a single argument, `x`, (with `y` missing)
and `by_feature` is `FALSE` all geometries are unioned together and an
`sfg` or single-geometry `sfc` object is returned. If `by_feature` is
`TRUE` each feature geometry is unioned individually. This can for
instance be used to resolve internal boundaries after polygons were
combined using `st_combine`. If `y` is provided, all elements of `x` and
`y` are unioned, pairwise if `by_feature` is TRUE, or else as the
Cartesian product of both sets.

Unioning a set of overlapping polygons has the effect of merging the
areas (i.e. the same effect as iteratively unioning all individual
polygons together). Unioning a set of LineStrings has the effect of
fully noding and dissolving the input linework. In this context "fully
noded" means that there will be a node or endpoint in the output for
every endpoint or line segment crossing in the input. "Dissolved" means
that any duplicate (e.g. coincident) line segments or portions of line
segments will be reduced to a single line segment in the output.
Unioning a set of Points has the effect of merging all identical points
(producing a set with no duplicates).

## See also

[st_intersection](https://r-spatial.github.io/sf/reference/geos_binary_ops.md),
[st_difference](https://r-spatial.github.io/sf/reference/geos_binary_ops.md),
[st_sym_difference](https://r-spatial.github.io/sf/reference/geos_binary_ops.md)

## Examples

``` r
nc = st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
st_combine(nc)
#> Geometry set for 1 feature 
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
#> MULTIPOLYGON (((-81.47276 36.23436, -81.54084 3...
plot(st_union(nc))
```
