# Compute geometric measurements

Compute Euclidean or great circle distance between pairs of geometries;
compute, the area or the length of a set of geometries.

## Usage

``` r
st_area(x, ...)

# S3 method for class 'sfc'
st_area(x, ...)

st_length(x, ...)

st_perimeter(x, ...)

st_distance(
  x,
  y,
  ...,
  dist_fun,
  by_element = FALSE,
  which = ifelse(isTRUE(st_is_longlat(x)), "Great Circle", "Euclidean"),
  par = 0,
  tolerance = 0
)
```

## Arguments

- x:

  object of class `sf`, `sfc` or `sfg`

- ...:

  passed on to
  [s2_distance](https://r-spatial.github.io/s2/reference/s2_is_collection.html),
  [s2_distance_matrix](https://r-spatial.github.io/s2/reference/s2_closest_feature.html),
  or
  [s2_perimeter](https://r-spatial.github.io/s2/reference/s2_is_collection.html)

- y:

  object of class `sf`, `sfc` or `sfg`, defaults to `x`

- dist_fun:

  deprecated

- by_element:

  logical; if `TRUE`, return a vector with distance between the first
  elements of `x` and `y`, the second, etc; an error is raised if `x`
  and `y` are not the same length. If `FALSE`, return the dense matrix
  with all pairwise distances.

- which:

  character; for Cartesian coordinates only: one of `Euclidean`,
  `Hausdorff` or `Frechet`; for geodetic coordinates, great circle
  distances are computed; see details

- par:

  for `which` equal to `Hausdorff` or `Frechet`, optionally use a value
  between 0 and 1 to densify the geometry

- tolerance:

  ignored if `st_is_longlat(x)` is `FALSE`; otherwise, if set to a
  positive value, the first distance smaller than `tolerance` will be
  returned, and true distance may be smaller; this may speed up
  computation. In meters, or a `units` object convertible to meters.

## Value

If the coordinate reference system of `x` was set, these functions
return values with unit of measurement; see
[set_units](https://r-quantities.github.io/units/reference/units.html).

st_area returns the area of each feature geometry, computed in the
coordinate reference system used. In case `x` has geodetic coordinates
(unprojected), then if
[`sf_use_s2()`](https://r-spatial.github.io/sf/reference/s2.md) is
`FALSE`
[st_geod_area](https://r-spatial.github.io/lwgeom/reference/geod.html)
is used for area calculation, if it is `TRUE` then
[s2_area](https://r-spatial.github.io/s2/reference/s2_is_collection.html)
is used: the former assumes an ellipsoidal shape, the latter a spherical
shape of the Earth. In case of projected data, areas are computed in
flat space. The argument `...` can be used to specify `radius` to
[s2_area](https://r-spatial.github.io/s2/reference/s2_is_collection.html),
to modify the Earth radius.

st_length returns the length of a `LINESTRING` or `MULTILINESTRING`
geometry, using the coordinate reference system. `POINT`, `MULTIPOINT`,
`POLYGON` or `MULTIPOLYGON` geometries return zero.

If `by_element` is `FALSE` `st_distance` returns a dense numeric matrix
of dimension length(x) by length(y); otherwise it returns a numeric
vector the same length as `x` and `y` with an error raised if the
lengths of `x` and `y` are unequal. Distances involving empty geometries
are `NA`.

## Details

great circle distance calculations use by default spherical distances
([s2_distance](https://r-spatial.github.io/s2/reference/s2_is_collection.html)
or
[s2_distance_matrix](https://r-spatial.github.io/s2/reference/s2_closest_feature.html));
if [`sf_use_s2()`](https://r-spatial.github.io/sf/reference/s2.md) is
`FALSE`, ellipsoidal distances are computed using
[st_geod_distance](https://r-spatial.github.io/lwgeom/reference/geod.html)
which uses function `geod_inverse` from GeographicLib (part of PROJ);
see Karney, Charles FF, 2013, Algorithms for geodesics, Journal of
Geodesy 87(1), 43–55

## See also

[st_dimension](https://r-spatial.github.io/sf/reference/geos_query.md),
[st_cast](https://r-spatial.github.io/sf/reference/st_cast.md) to
convert geometry types

## Examples

``` r
b0 = st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))
b1 = b0 + 2
b2 = b0 + c(-0.2, 2)
x = st_sfc(b0, b1, b2)
st_area(x)
#> [1] 4 4 4
line = st_sfc(st_linestring(rbind(c(30,30), c(40,40))), crs = 4326)
st_length(line)
#> 1435335 [m]

outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)

poly = st_polygon(list(outer, hole1, hole2))
mpoly = st_multipolygon(list(
  list(outer, hole1, hole2),
  list(outer + 12, hole1 + 12)
))

st_length(st_sfc(poly, mpoly))
#> [1] 0 0
st_perimeter(poly)
#> [1] 48
st_perimeter(mpoly)
#> [1] 92
p = st_sfc(st_point(c(0,0)), st_point(c(0,1)), st_point(c(0,2)))
st_distance(p, p)
#>      [,1] [,2] [,3]
#> [1,]    0    1    2
#> [2,]    1    0    1
#> [3,]    2    1    0
st_distance(p, p, by_element = TRUE)
#> [1] 0 0 0
```
