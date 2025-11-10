# Return 'm' range of a simple feature or simple feature set

Return 'm' range of a simple feature or simple feature set

## Usage

``` r
# S3 method for class 'm_range'
is.na(x)

st_m_range(obj, ...)

# S3 method for class 'POINT'
st_m_range(obj, ...)

# S3 method for class 'MULTIPOINT'
st_m_range(obj, ...)

# S3 method for class 'LINESTRING'
st_m_range(obj, ...)

# S3 method for class 'POLYGON'
st_m_range(obj, ...)

# S3 method for class 'MULTILINESTRING'
st_m_range(obj, ...)

# S3 method for class 'MULTIPOLYGON'
st_m_range(obj, ...)

# S3 method for class 'GEOMETRYCOLLECTION'
st_m_range(obj, ...)

# S3 method for class 'MULTISURFACE'
st_m_range(obj, ...)

# S3 method for class 'MULTICURVE'
st_m_range(obj, ...)

# S3 method for class 'CURVEPOLYGON'
st_m_range(obj, ...)

# S3 method for class 'COMPOUNDCURVE'
st_m_range(obj, ...)

# S3 method for class 'POLYHEDRALSURFACE'
st_m_range(obj, ...)

# S3 method for class 'TIN'
st_m_range(obj, ...)

# S3 method for class 'TRIANGLE'
st_m_range(obj, ...)

# S3 method for class 'CIRCULARSTRING'
st_m_range(obj, ...)

# S3 method for class 'sfc'
st_m_range(obj, ...)

# S3 method for class 'sf'
st_m_range(obj, ...)

# S3 method for class 'numeric'
st_m_range(obj, ..., crs = NA_crs_)

NA_m_range_
```

## Format

An object of class `m_range` of length 2.

## Arguments

- x:

  object of class `m_range`

- obj:

  object to compute the m range from

- ...:

  ignored

- crs:

  object of class `crs`, or argument to
  [st_crs](https://r-spatial.github.io/sf/reference/st_crs.md),
  specifying the CRS of this bounding box.

## Value

a numeric vector of length two, with `mmin` and `mmax` values; if `obj`
is of class `sf` or `sfc` the object if `obj` is of class `sf` or `sfc`
the object returned has a class `m_range`

## Details

`NA_m_range_` represents the missing value for a `m_range` object

## Examples

``` r
a = st_sf(a = 1:2, geom = st_sfc(st_point(0:3), st_point(1:4)), crs = 4326)
st_m_range(a)
#> mmin mmax 
#>    3    4 
st_m_range(c(mmin = 16.1, mmax = 16.6), crs = st_crs(4326))
#> mmin mmax 
#> 16.1 16.6 
```
