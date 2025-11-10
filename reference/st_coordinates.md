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
