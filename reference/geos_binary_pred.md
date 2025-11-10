# Geometric binary predicates on pairs of simple feature geometry sets

Geometric binary predicates on pairs of simple feature geometry sets

## Usage

``` r
st_intersects(x, y, sparse = TRUE, ...)

st_disjoint(x, y = x, sparse = TRUE, prepared = TRUE, ...)

st_touches(x, y, sparse = TRUE, prepared = TRUE, ...)

st_crosses(x, y, sparse = TRUE, prepared = TRUE, ...)

st_within(x, y, sparse = TRUE, prepared = TRUE, ...)

st_contains(x, y, sparse = TRUE, prepared = TRUE, ..., model = "open")

st_contains_properly(x, y, sparse = TRUE, prepared = TRUE, ...)

st_overlaps(x, y, sparse = TRUE, prepared = TRUE, ...)

st_equals(
  x,
  y,
  sparse = TRUE,
  prepared = FALSE,
  ...,
  retain_unique = FALSE,
  remove_self = FALSE
)

st_covers(x, y, sparse = TRUE, prepared = TRUE, ..., model = "closed")

st_covered_by(x, y = x, sparse = TRUE, prepared = TRUE, ..., model = "closed")

st_equals_exact(x, y, par, sparse = TRUE, prepared = FALSE, ...)

st_is_within_distance(x, y = x, dist, sparse = TRUE, ..., remove_self = FALSE)
```

## Arguments

- x:

  object of class `sf`, `sfc` or `sfg`

- y:

  object of class `sf`, `sfc` or `sfg`; if missing, `x` is used

- sparse:

  logical; should a sparse index list be returned (`TRUE`) or a dense
  logical matrix? See below.

- ...:

  Arguments passed on to
  [`s2::s2_options`](https://r-spatial.github.io/s2/reference/s2_options.html)

  `snap`

  :   Use `s2_snap_identity()`, `s2_snap_distance()`, `s2_snap_level()`,
      or `s2_snap_precision()` to specify how or if coordinate rounding
      should occur.

  `snap_radius`

  :   As opposed to the snap function, which specifies the maximum
      distance a vertex should move, the snap radius (in radians) sets
      the minimum distance between vertices of the output that don't
      cause vertices to move more than the distance specified by the
      snap function. This can be used to simplify the result of a
      boolean operation. Use -1 to specify that any minimum distance is
      acceptable.

  `duplicate_edges`

  :   Use `TRUE` to keep duplicate edges (e.g., duplicate points).

  `edge_type`

  :   One of 'directed' (default) or 'undirected'.

  `validate`

  :   Use `TRUE` to validate the result from the builder.

  `polyline_type`

  :   One of 'path' (default) or 'walk'. If 'walk', polylines that
      backtrack are preserved.

  `polyline_sibling_pairs`

  :   One of 'discard' (default) or 'keep'.

  `simplify_edge_chains`

  :   Use `TRUE` to remove vertices that are within `snap_radius` of the
      original vertex.

  `split_crossing_edges`

  :   Use `TRUE` to split crossing polyline edges when creating
      geometries.

  `idempotent`

  :   Use `FALSE` to apply snap even if snapping is not necessary to
      satisfy vertex constraints.

  `dimensions`

  :   A combination of 'point', 'polyline', and/or 'polygon' that can
      used to constrain the output of
      [`s2_rebuild()`](https://r-spatial.github.io/s2/reference/s2_boundary.html)
      or a boolean operation.

- prepared:

  logical; prepare geometry for `x`, before looping over `y`? See
  Details.

- model:

  character; polygon/polyline model; one of "open", "semi-open" or
  "closed"; see Details.

- retain_unique:

  logical; if `TRUE` (and `y` is missing) return only indexes of points
  larger than the current index; this can be used to select unique
  geometries, see examples. This argument can be used for all geometry
  predicates; see also
  [distinct.sf](https://r-spatial.github.io/sf/reference/tidyverse.md)
  to find records where geometries AND attributes are distinct.

- remove_self:

  logical; if `TRUE` (and `y` is missing) return only indexes of
  geometries different from the current index; this can be used to omit
  self-intersections; see examples. This argument can be used for all
  geometry predicates

- par:

  numeric; parameter used for "equals_exact" (margin);

- dist:

  distance threshold; geometry indexes with distances smaller or equal
  to this value are returned; numeric value or units value having
  distance units.

## Value

If `sparse=FALSE`, `st_predicate` (with `predicate` e.g. "intersects")
returns a dense logical matrix with element `i,j` equal to `TRUE` when
`predicate(x[i], y[j])` (e.g., when geometry of feature i and j
intersect); if `sparse=TRUE`, an object of class
[`sgbp`](https://r-spatial.github.io/sf/reference/sgbp.md) is returned,
which is a sparse list representation of the same matrix, with list
element `i` an integer vector with all indices `j` for which
`predicate(x[i],y[j])` is `TRUE` (and hence a zero-length integer vector
if none of them is `TRUE`). From the dense matrix, one can find out if
one or more elements intersect by `apply(mat, 1, any)`, and from the
sparse list by `lengths(lst) > 0`, see examples below.

## Details

If `prepared` is `TRUE`, and `x` contains POINT geometries and `y`
contains polygons, then the polygon geometries are prepared, rather than
the points.

For most predicates, a spatial index is built on argument `x`; see
<https://r-spatial.org/r/2017/06/22/spatial-index.html>. Specifically,
`st_intersects`, `st_disjoint`, `st_touches` `st_crosses`, `st_within`,
`st_contains`, `st_contains_properly`, `st_overlaps`, `st_equals`,
`st_covers` and `st_covered_by` all build spatial indexes for more
efficient geometry calculations. `st_relate`, `st_equals_exact`, and do
not; `st_is_within_distance` uses a spatial index for geographic
coordinates when
[`sf_use_s2()`](https://r-spatial.github.io/sf/reference/s2.md) is true.

If `y` is missing, `st_predicate(x, x)` is effectively called, and a
square matrix is returned with diagonal elements
`st_predicate(x[i], x[i])`.

Sparse geometry binary predicate
([`sgbp`](https://r-spatial.github.io/sf/reference/sgbp.md)) lists have
the following attributes: `region.id` with the `row.names` of `x` (if
any, else `1:n`), `ncol` with the number of features in `y`, and
`predicate` with the name of the predicate used.

for `model`, see https://github.com/r-spatial/s2/issues/32

`st_contains_properly(A,B)` is true if A intersects B's interior, but
not its edges or exterior; A contains A, but A does not properly contain
A.

See also
[st_relate](https://r-spatial.github.io/sf/reference/st_relate.md) and
<https://en.wikipedia.org/wiki/DE-9IM> for a more detailed description
of the underlying algorithms.

`st_equals_exact` returns true for two geometries of the same type and
their vertices corresponding by index are equal up to a specified
tolerance.

## Note

For intersection on pairs of simple feature geometries, use the function
[`st_intersection`](https://r-spatial.github.io/sf/reference/geos_binary_ops.md)
instead of `st_intersects`.

## Examples

``` r
pts = st_sfc(st_point(c(.5,.5)), st_point(c(1.5, 1.5)), st_point(c(2.5, 2.5)))
pol = st_polygon(list(rbind(c(0,0), c(2,0), c(2,2), c(0,2), c(0,0))))
(lst = st_intersects(pts, pol))
#> Sparse geometry binary predicate list of length 3, where the predicate
#> was `intersects'
#>  1: 1
#>  2: 1
#>  3: (empty)
(mat = st_intersects(pts, pol, sparse = FALSE))
#>       [,1]
#> [1,]  TRUE
#> [2,]  TRUE
#> [3,] FALSE
# which points fall inside a polygon?
apply(mat, 1, any)
#> [1]  TRUE  TRUE FALSE
lengths(lst) > 0
#> [1]  TRUE  TRUE FALSE
# which points fall inside the first polygon?
st_intersects(pol, pts)[[1]]
#> [1] 1 2
# remove duplicate geometries:
p1 = st_point(0:1)
p2 = st_point(2:1)
p = st_sf(a = letters[1:8], geom = st_sfc(p1, p1, p2, p1, p1, p2, p2, p1))
st_equals(p)
#> Sparse geometry binary predicate list of length 8, where the predicate
#> was `equals'
#>  1: 1, 2, 4, 5, 8
#>  2: 1, 2, 4, 5, 8
#>  3: 3, 6, 7
#>  4: 1, 2, 4, 5, 8
#>  5: 1, 2, 4, 5, 8
#>  6: 3, 6, 7
#>  7: 3, 6, 7
#>  8: 1, 2, 4, 5, 8
st_equals(p, remove_self = TRUE)
#> Sparse geometry binary predicate list of length 8, where the predicate
#> was `equals', with remove_self = TRUE
#>  1: 2, 4, 5, 8
#>  2: 1, 4, 5, 8
#>  3: 6, 7
#>  4: 1, 2, 5, 8
#>  5: 1, 2, 4, 8
#>  6: 3, 7
#>  7: 3, 6
#>  8: 1, 2, 4, 5
(u = st_equals(p, retain_unique = TRUE))
#> Sparse geometry binary predicate list of length 8, where the predicate
#> was `equals', with retain_unique = TRUE
#>  1: 2, 4, 5, 8
#>  2: 4, 5, 8
#>  3: 6, 7
#>  4: 5, 8
#>  5: 8
#>  6: 7
#>  7: (empty)
#>  8: (empty)
# retain the records with unique geometries:
p[-unlist(u),]
#> Simple feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 1 xmax: 2 ymax: 1
#> CRS:           NA
#>   a        geom
#> 1 a POINT (0 1)
#> 3 c POINT (2 1)
```
