# Project point on linestring, interpolate along a linestring

Project point on linestring, interpolate along a linestring

## Usage

``` r
st_line_project(line, point, normalized = FALSE)

st_line_interpolate(line, dist, normalized = FALSE)
```

## Arguments

- line:

  object of class `sfc` with `LINESTRING` geometry

- point:

  object of class `sfc` with `POINT` geometry

- normalized:

  logical; if `TRUE`, use or return distance normalised to 0-1

- dist:

  numeric or units, vector with distance value(s), in units of the
  coordinates

## Value

`st_line_project` returns the distance(s) of point(s) along line(s),
when projected on the line(s)

`st_line_interpolate` returns the point(s) at dist(s), when measured
along (interpolated on) the line(s)

## Details

arguments `line`, `point` and `dist` are recycled to common length when
needed

## Examples

``` r
st_line_project(st_as_sfc("LINESTRING (0 0, 10 10)"), st_as_sfc(c("POINT (0 0)", "POINT (5 5)")))
#> [1] 0.000000 7.071068
st_line_project(st_as_sfc("LINESTRING (0 0, 10 10)"), st_as_sfc("POINT (5 5)"), TRUE)
#> [1] 0.5
st_line_interpolate(st_as_sfc("LINESTRING (0 0, 1 1)"), 1)
#> Geometry set for 1 feature 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0.7071068 ymin: 0.7071068 xmax: 0.7071068 ymax: 0.7071068
#> CRS:           NA
#> POINT (0.7071068 0.7071068)
st_line_interpolate(st_as_sfc("LINESTRING (0 0, 1 1)"), 1, TRUE)
#> Geometry set for 1 feature 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 1 xmax: 1 ymax: 1
#> CRS:           NA
#> POINT (1 1)
# https://github.com/r-spatial/sf/issues/2542; use for geographic coordinates:
l1 <- st_as_sfc("LINESTRING (10.1 50.1, 10.2 50.2)", crs = 'OGC:CRS84')
dists = units::set_units(seq(0, sqrt(2)/10, length.out = 5), degrees)
st_line_interpolate(l1, dists)
#> although coordinates are longitude/latitude, st_project_point assumes that they
#> are planar
#> Geometry set for 5 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 10.1 ymin: 50.1 xmax: 10.2 ymax: 50.2
#> Geodetic CRS:  WGS 84 (CRS84)
#> POINT (10.1 50.1)
#> POINT (10.125 50.125)
#> POINT (10.15 50.15)
#> POINT (10.175 50.175)
#> POINT (10.2 50.2)
```
