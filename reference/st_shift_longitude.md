# Shift or re-center geographical coordinates for a Pacific view

All longitudes \< 0 are added to 360, to avoid for instance parts of
Alaska being represented on the far left and right of a plot because
they have values straddling 180 degrees. In general, using a projected
coordinate reference system is to be preferred, but this method permits
a geographical coordinate reference system to be used. This is the sf
equivalent of
[recenter](https://edzer.github.io/sp/reference/recenter-methods.html)
in the sp package and `ST_ShiftLongitude` in PostGIS.

## Usage

``` r
st_shift_longitude(x)

# S3 method for class 'sfc'
st_shift_longitude(x, ...)

# S3 method for class 'sf'
st_shift_longitude(x, ...)
```

## Arguments

- x:

  object of class `sf` or `sfc`

- ...:

  ignored

## Examples

``` r
## sfc
pt1 = st_point(c(-170, 50))
pt2 = st_point(c(170, 50))
(sfc = st_sfc(pt1, pt2))
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -170 ymin: 50 xmax: 170 ymax: 50
#> CRS:           NA
#> POINT (-170 50)
#> POINT (170 50)
sfc = st_set_crs(sfc, 4326)
st_shift_longitude(sfc)
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 170 ymin: 50 xmax: 190 ymax: 50
#> Geodetic CRS:  WGS 84
#> POINT (190 50)
#> POINT (170 50)

## sf
d = st_as_sf(data.frame(id = 1:2, geometry = sfc))
st_shift_longitude(d)
#> Simple feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 170 ymin: 50 xmax: 190 ymax: 50
#> Geodetic CRS:  WGS 84
#>   id       geometry
#> 1  1 POINT (190 50)
#> 2  2 POINT (170 50)
```
