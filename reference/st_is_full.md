# predicate whether a geometry is equal to a POLYGON FULL

predicate whether a geometry is equal to a POLYGON FULL

## Usage

``` r
st_is_full(x, ...)

# S3 method for class 'sfg'
st_is_full(x, ..., is_longlat = NULL)

# S3 method for class 'sfc'
st_is_full(x, ...)

# S3 method for class 'sf'
st_is_full(x, ...)

# S3 method for class 'bbox'
st_is_full(x, ...)
```

## Arguments

- x:

  object of class `sfg`, `sfc` or `sf`

- ...:

  ignored, except when it contains a `crs` argument to inform
  unspecified `is_longlat`

- is_longlat:

  logical; output of
  [st_is_longlat](https://r-spatial.github.io/sf/reference/st_is_longlat.md)
  of the parent `sfc` object

## Value

logical, indicating whether geometries are POLYGON FULL (a spherical
polygon covering the entire sphere)
