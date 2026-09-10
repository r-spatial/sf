# retrieve coordinates in matrix form

retrieve coordinates in matrix form

## Usage

``` r
st_coordinates(x, ...)
```

## Arguments

- x:

  object of class sf, sfc or sfg

- ...:

  ignored

- round:

  logical; if `TRUE` and input object has a `precision` attribute
  different from 0, then its coordinates are rounded to the chosen
  precision level before building the output matrix. See also
  [`st_precision()`](https://r-spatial.github.io/sf/reference/st_precision.md).

## Value

matrix with coordinates (X, Y, possibly Z and/or M) in rows, possibly
followed by integer indicators `L1`,...,`L3` that point out to which
structure the coordinate belongs; for `POINT` this is absent (each
coordinate is a feature), for `LINESTRING` `L1` refers to the feature,
for `MULTILINESTRING` `L1` refers to the part and `L2` to the simple
feature, for `POLYGON` `L1` refers to the main ring or holes and `L2` to
the simple feature, for `MULTIPOLYGON` `L1` refers to the main ring or
holes, `L2` to the ring id in the `MULTIPOLYGON`, and `L3` to the simple
feature.

For `POLYGONS`, `L1` can be used to identify exterior rings and inner
holes. The exterior ring is when `L1` is equal to 1. Interior rings are
identified when `L1` is greater than 1. `L2` can be used to
differentiate between the feature. Whereas for `MULTIPOLYGON`, `L3`
refers to the `MULTIPOLYGON` feature and `L2` refers to the component
`POLYGON`.

## Examples

``` r
x = st_sfc(st_point(c(1.234, 1.234)), crs = 3003) # units: m
st_coordinates(x)
#>          X     Y
#> [1,] 1.234 1.234

st_precision(x) = 1e1
st_coordinates(x, round = TRUE) # round 1 decimal place 
#>        X   Y
#> [1,] 1.2 1.2

st_precision(x) = units::set_units(1, cm)
st_coordinates(x, round = TRUE) # round to cm
#>         X    Y
#> [1,] 1.23 1.23
```
