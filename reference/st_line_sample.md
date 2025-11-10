# Sample points on a linear geometry

Sample points on a linear geometry

## Usage

``` r
st_line_sample(x, n, density, type = "regular", sample = NULL)
```

## Arguments

- x:

  object of class `sf`, `sfc` or `sfg`

- n:

  integer; number of points to choose per geometry; if missing, n will
  be computed as `round(density * st_length(geom))`.

- density:

  numeric; density (points per distance unit) of the sampling, possibly
  a vector of length equal to the number of features (otherwise
  recycled); `density` may be of class `units`.

- type:

  character; indicate the sampling type, either "regular" or "random"

- sample:

  numeric; a vector of numbers between 0 and 1 indicating the points to
  sample - if defined sample overrules n, density and type.

## Examples

``` r
ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))),
  st_linestring(rbind(c(0,0),c(10,0))))
st_line_sample(ls, density = 1)
#> Geometry set for 2 features 
#> Geometry type: MULTIPOINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 9.5 ymax: 0.5
#> CRS:           NA
#> MULTIPOINT ((0 0.5))
#> MULTIPOINT ((0.5 0), (1.5 0), (2.5 0), (3.5 0),...
ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))),
 st_linestring(rbind(c(0,0),c(.1,0))), crs = 4326)
try(st_line_sample(ls, density = 1/1000)) # error
#> Error in st_line_sample(ls, density = 1/1000) : 
#>   st_line_sample for longitude/latitude not supported; use st_segmentize?
st_line_sample(st_transform(ls, 3857), n = 5) # five points for each line
#> Geometry set for 2 features 
#> Geometry type: MULTIPOINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 10018.75 ymax: 100192.6
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> MULTIPOINT ((0 11132.51), (0 33397.54), (0 5566...
#> MULTIPOINT ((1113.195 0), (3339.585 0), (5565.9...
st_line_sample(st_transform(ls, 3857), n = c(1, 3)) # one and three points
#> Geometry set for 2 features 
#> Geometry type: MULTIPOINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 9276.624 ymax: 55662.57
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> MULTIPOINT ((0 55662.57))
#> MULTIPOINT ((1855.325 0), (5565.975 0), (9276.6...
st_line_sample(st_transform(ls, 3857), density = 1/1000) # one per km
#> Geometry set for 2 features 
#> Geometry type: MULTIPOINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 10625.95 ymax: 110823.7
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> MULTIPOINT ((0 501.4646), (0 1504.394), (0 2507...
#> MULTIPOINT ((505.9977 0), (1517.993 0), (2529.9...
st_line_sample(st_transform(ls, 3857), density = c(1/1000, 1/10000)) # one per km, one per 10 km
#> Geometry set for 2 features 
#> Geometry type: MULTIPOINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 5565.975 ymax: 110823.7
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> MULTIPOINT ((0 501.4646), (0 1504.394), (0 2507...
#> MULTIPOINT ((5565.975 0))
st_line_sample(st_transform(ls, 3857), density = units::set_units(1, 1/km)) # one per km
#> Geometry set for 2 features 
#> Geometry type: MULTIPOINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 10625.95 ymax: 110823.7
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> MULTIPOINT ((0 501.4646), (0 1504.394), (0 2507...
#> MULTIPOINT ((505.9977 0), (1517.993 0), (2529.9...
# five equidistant points including start and end:
st_line_sample(st_transform(ls, 3857), sample = c(0, 0.25, 0.5, 0.75, 1))
#> Geometry set for 2 features 
#> Geometry type: MULTIPOINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 11131.95 ymax: 111325.1
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> MULTIPOINT ((0 0), (0 27831.29), (0 55662.57), ...
#> MULTIPOINT ((0 0), (2782.987 0), (5565.975 0), ...
```
