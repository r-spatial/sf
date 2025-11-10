# test equality between the geometry type and a class or set of classes

test equality between the geometry type and a class or set of classes

## Usage

``` r
st_is(x, type)
```

## Arguments

- x:

  object of class `sf`, `sfc` or `sfg`

- type:

  character; class, or set of classes, to test against

## Examples

``` r
st_is(st_point(0:1), "POINT")
#> [1] TRUE
sfc = st_sfc(st_point(0:1), st_linestring(matrix(1:6,,2)))
st_is(sfc, "POINT")
#> [1]  TRUE FALSE
st_is(sfc, "POLYGON")
#> [1] FALSE FALSE
st_is(sfc, "LINESTRING")
#> [1] FALSE  TRUE
st_is(st_sf(a = 1:2, sfc), "LINESTRING")
#> [1] FALSE  TRUE
st_is(sfc, c("POINT", "LINESTRING"))
#> [1] TRUE TRUE
```
