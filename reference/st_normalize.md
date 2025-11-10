# Normalize simple features

`st_normalize` transforms the coordinates in the input feature to fall
between 0 and 1. By default the current domain is set to the bounding
box of the input, but other domains can be used as well

## Usage

``` r
st_normalize(x, domain = st_bbox(x), ...)
```

## Arguments

- x:

  object of class sf, sfc or sfg

- domain:

  The domain `x` should be normalized from as a length 4 vector of the
  form `c(xmin, ymin, xmax, ymax)`. Defaults to the bounding box of `x`

- ...:

  ignored

## Examples

``` r
p1 = st_point(c(7,52))
st_normalize(p1, domain = c(0, 0, 10, 100))
#> POINT (0.7 0.52)

p2 = st_point(c(-30,20))
sfc = st_sfc(p1, p2, crs = 4326)
sfc
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -30 ymin: 20 xmax: 7 ymax: 52
#> Geodetic CRS:  WGS 84
#> POINT (7 52)
#> POINT (-30 20)
sfc_norm <- st_normalize(sfc)
st_bbox(sfc_norm)
#> xmin ymin xmax ymax 
#>    0    0    1    1 
```
