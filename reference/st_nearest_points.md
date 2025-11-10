# get nearest points between pairs of geometries

get nearest points between pairs of geometries

## Usage

``` r
st_nearest_points(x, y, ...)

# S3 method for class 'sfc'
st_nearest_points(x, y, ..., pairwise = FALSE)

# S3 method for class 'sfg'
st_nearest_points(x, y, ...)

# S3 method for class 'sf'
st_nearest_points(x, y, ...)
```

## Arguments

- x:

  object of class `sfg`, `sfc` or `sf`

- y:

  object of class `sfg`, `sfc` or `sf`

- ...:

  ignored

- pairwise:

  logical; if `FALSE` (default) return nearest points between all pairs,
  if `TRUE`, return nearest points between subsequent pairs.

## Value

an [sfc](https://r-spatial.github.io/sf/reference/sfc.md) object with
all two-point `LINESTRING` geometries of point pairs from the first to
the second geometry, of length x \* y, with y cycling fastest. See
examples for ideas how to convert these to `POINT` geometries.

## Details

in case `x` lies inside `y`, when using S2, the end points are on
polygon boundaries, when using GEOS the end point are identical to `x`.

## See also

[st_nearest_feature](https://r-spatial.github.io/sf/reference/st_nearest_feature.md)
for finding the nearest feature

## Examples

``` r
r = sqrt(2)/10
pt1 = st_point(c(.1,.1))
pt2 = st_point(c(.9,.9))
pt3 = st_point(c(.9,.1))
b1 = st_buffer(pt1, r)
b2 = st_buffer(pt2, r)
b3 = st_buffer(pt3, r)
(ls0 = st_nearest_points(b1, b2)) # sfg
#> Geometry set for 1 feature 
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 0.2 ymin: 0.2 xmax: 0.8 ymax: 0.8
#> CRS:           NA
#> LINESTRING (0.2 0.2, 0.8 0.8)
(ls = st_nearest_points(st_sfc(b1), st_sfc(b2, b3))) # sfc
#> Geometry set for 2 features 
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 0.2 ymin: 0.1 xmax: 0.8 ymax: 0.8
#> CRS:           NA
#> LINESTRING (0.2 0.2, 0.8 0.8)
#> LINESTRING (0.2414214 0.1, 0.7585786 0.1)
plot(b1, xlim = c(-.2,1.2), ylim = c(-.2,1.2), col = NA, border = 'green')
plot(st_sfc(b2, b3), add = TRUE, col = NA, border = 'blue')
plot(ls, add = TRUE, col = 'red')


nc = st_read(system.file("gpkg/nc.gpkg", package="sf"))
#> Reading layer `nc.gpkg' from data source 
#>   `/home/runner/work/_temp/Library/sf/gpkg/nc.gpkg' using driver `GPKG'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
plot(st_geometry(nc))
ls = st_nearest_points(nc[1,], nc)
plot(ls, col = 'red', add = TRUE)
pts = st_cast(ls, "POINT") # gives all start & end points
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
#> Warning: point from first coordinate only
# starting, "from" points, corresponding to x:
plot(pts[seq(1, 200, 2)], add = TRUE, col = 'blue')
# ending, "to" points, corresponding to y:
plot(pts[seq(2, 200, 2)], add = TRUE, col = 'green')

```
