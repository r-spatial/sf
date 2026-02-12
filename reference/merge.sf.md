# merge method for sf and data.frame object

merge method for sf and data.frame object

## Usage

``` r
# S3 method for class 'sf'
merge(x, y, ...)
```

## Arguments

- x:

  object of class `sf`

- y:

  object of class `data.frame`

- ...:

  arguments passed on to `merge.data.frame`

## Examples

``` r
a = data.frame(a = 1:3, b = 5:7)
st_geometry(a) = st_sfc(st_point(c(0,0)), st_point(c(1,1)), st_point(c(2,2)))
b = data.frame(x = c("a", "b", "c"), b = c(2,5,6))
merge(a, b)
#> Simple feature collection with 2 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 1 ymax: 1
#> CRS:           NA
#>   b a x    geometry
#> 1 5 1 b POINT (0 0)
#> 2 6 2 c POINT (1 1)
merge(a, b, all = TRUE)
#> Simple feature collection with 4 features and 3 fields (with 1 geometry empty)
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 2 ymax: 2
#> CRS:           NA
#>   b  a    x    geometry
#> 1 2 NA    a POINT EMPTY
#> 2 5  1    b POINT (0 0)
#> 3 6  2    c POINT (1 1)
#> 4 7  3 <NA> POINT (2 2)
```
