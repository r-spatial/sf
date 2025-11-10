# transform method for sf objects

Can be used to create or modify attribute variables; for transforming
geometries see
[st_transform](https://r-spatial.github.io/sf/reference/st_transform.md),
and all other functions starting with `st_`.

## Usage

``` r
# S3 method for class 'sf'
transform(`_data`, ...)
```

## Arguments

- \_data:

  object of class `sf`

- ...:

  Further arguments of the form `new_variable = expression`

## Examples

``` r
a = data.frame(x1 = 1:3, x2 = 5:7)
st_geometry(a) = st_sfc(st_point(c(0,0)), st_point(c(1,1)), st_point(c(2,2)))
transform(a, x1_sq = x1^2)
#> Simple feature collection with 3 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 2 ymax: 2
#> CRS:           NA
#>   x1 x2 x1_sq    geometry
#> 1  1  5     1 POINT (0 0)
#> 2  2  6     4 POINT (1 1)
#> 3  3  7     9 POINT (2 2)
transform(a, x1_x2 = x1*x2)
#> Simple feature collection with 3 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 2 ymax: 2
#> CRS:           NA
#>   x1 x2 x1_x2    geometry
#> 1  1  5     5 POINT (0 0)
#> 2  2  6    12 POINT (1 1)
#> 3  3  7    21 POINT (2 2)
```
