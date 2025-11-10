# Assert whether simple feature coordinates are longlat degrees

Assert whether simple feature coordinates are longlat degrees

## Usage

``` r
st_is_longlat(x)
```

## Arguments

- x:

  object of class [sf](https://r-spatial.github.io/sf/reference/sf.md)
  or [sfc](https://r-spatial.github.io/sf/reference/sfc.md), or
  otherwise an object of a class that has an
  [st_crs](https://r-spatial.github.io/sf/reference/st_crs.md) method
  returning a `crs` object

## Value

`TRUE` if `x` has geographic coordinates, `FALSE` if it has projected
coordinates, or `NA` if `is.na(st_crs(x))`.
