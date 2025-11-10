# Return 'z' range of a simple feature or simple feature set

Return 'z' range of a simple feature or simple feature set

## Usage

``` r
# S3 method for class 'z_range'
is.na(x)

st_z_range(obj, ...)

# S3 method for class 'POINT'
st_z_range(obj, ...)

# S3 method for class 'MULTIPOINT'
st_z_range(obj, ...)

# S3 method for class 'LINESTRING'
st_z_range(obj, ...)

# S3 method for class 'POLYGON'
st_z_range(obj, ...)

# S3 method for class 'MULTILINESTRING'
st_z_range(obj, ...)

# S3 method for class 'MULTIPOLYGON'
st_z_range(obj, ...)

# S3 method for class 'GEOMETRYCOLLECTION'
st_z_range(obj, ...)

# S3 method for class 'MULTISURFACE'
st_z_range(obj, ...)

# S3 method for class 'MULTICURVE'
st_z_range(obj, ...)

# S3 method for class 'CURVEPOLYGON'
st_z_range(obj, ...)

# S3 method for class 'COMPOUNDCURVE'
st_z_range(obj, ...)

# S3 method for class 'POLYHEDRALSURFACE'
st_z_range(obj, ...)

# S3 method for class 'TIN'
st_z_range(obj, ...)

# S3 method for class 'TRIANGLE'
st_z_range(obj, ...)

# S3 method for class 'CIRCULARSTRING'
st_z_range(obj, ...)

# S3 method for class 'sfc'
st_z_range(obj, ...)

# S3 method for class 'sf'
st_z_range(obj, ...)

# S3 method for class 'numeric'
st_z_range(obj, ..., crs = NA_crs_)

NA_z_range_
```

## Format

An object of class `z_range` of length 2.

## Arguments

- x:

  object of class `z_range`

- obj:

  object to compute the z range from

- ...:

  ignored

- crs:

  object of class `crs`, or argument to
  [st_crs](https://r-spatial.github.io/sf/reference/st_crs.md),
  specifying the CRS of this bounding box.

## Value

a numeric vector of length two, with `zmin` and `zmax` values; if `obj`
is of class `sf` or `sfc` the object returned has a class `z_range`

## Details

`NA_z_range_` represents the missing value for a `z_range` object

## Examples

``` r
a = st_sf(a = 1:2, geom = st_sfc(st_point(0:2), st_point(1:3)), crs = 4326)
st_z_range(a)
#> zmin zmax 
#>    2    3 
st_z_range(c(zmin = 16.1, zmax = 16.6), crs = st_crs(4326))
#> zmin zmax 
#> 16.1 16.6 
```
