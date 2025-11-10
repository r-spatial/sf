# Return bounding of a simple feature or simple feature set

Return bounding of a simple feature or simple feature set

## Usage

``` r
# S3 method for class 'bbox'
is.na(x)

st_bbox(obj, ...)

# S3 method for class 'POINT'
st_bbox(obj, ...)

# S3 method for class 'MULTIPOINT'
st_bbox(obj, ...)

# S3 method for class 'LINESTRING'
st_bbox(obj, ...)

# S3 method for class 'POLYGON'
st_bbox(obj, ...)

# S3 method for class 'MULTILINESTRING'
st_bbox(obj, ...)

# S3 method for class 'MULTIPOLYGON'
st_bbox(obj, ...)

# S3 method for class 'GEOMETRYCOLLECTION'
st_bbox(obj, ...)

# S3 method for class 'MULTISURFACE'
st_bbox(obj, ...)

# S3 method for class 'MULTICURVE'
st_bbox(obj, ...)

# S3 method for class 'CURVEPOLYGON'
st_bbox(obj, ...)

# S3 method for class 'COMPOUNDCURVE'
st_bbox(obj, ...)

# S3 method for class 'POLYHEDRALSURFACE'
st_bbox(obj, ...)

# S3 method for class 'TIN'
st_bbox(obj, ...)

# S3 method for class 'TRIANGLE'
st_bbox(obj, ...)

# S3 method for class 'CIRCULARSTRING'
st_bbox(obj, ...)

# S3 method for class 'sfc'
st_bbox(obj, ...)

# S3 method for class 'sf'
st_bbox(obj, ...)

# S3 method for class 'Spatial'
st_bbox(obj, ...)

# S3 method for class 'Raster'
st_bbox(obj, ...)

# S3 method for class 'Extent'
st_bbox(obj, ..., crs = NA_crs_)

# S3 method for class 'numeric'
st_bbox(obj, ..., crs = NA_crs_)

NA_bbox_

FULL_bbox_

# S3 method for class 'bbox'
format(x, ...)
```

## Format

An object of class `bbox` of length 4.

An object of class `bbox` of length 4.

## Arguments

- x:

  object of class `bbox`

- obj:

  object to compute the bounding box from

- ...:

  for format.bbox, passed on to
  [format](https://rdrr.io/r/base/format.html) to format individual
  numbers

- crs:

  object of class `crs`, or argument to
  [st_crs](https://r-spatial.github.io/sf/reference/st_crs.md),
  specifying the CRS of this bounding box.

## Value

a numeric vector of length four, with `xmin`, `ymin`, `xmax` and `ymax`
values; if `obj` is of class `sf`, `sfc`, `Spatial` or `Raster`, the
object returned has a class `bbox`, an attribute `crs` and a method to
print the bbox and an `st_crs` method to retrieve the coordinate
reference system corresponding to `obj` (and hence the bounding box).
[st_as_sfc](https://r-spatial.github.io/sf/reference/st_as_sfc.md) has a
methods for `bbox` objects to generate a polygon around the four
bounding box points.

## Details

`NA_bbox_` represents the missing value for a `bbox` object

`NA_bbox_` represents the missing value for a `bbox` object

## Examples

``` r
a = st_sf(a = 1:2, geom = st_sfc(st_point(0:1), st_point(1:2)), crs = 4326)
st_bbox(a)
#> xmin ymin xmax ymax 
#>    0    1    1    2 
st_as_sfc(st_bbox(a))
#> Geometry set for 1 feature 
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 1 xmax: 1 ymax: 2
#> Geodetic CRS:  WGS 84
#> POLYGON ((0 1, 1 1, 1 2, 0 2, 0 1))
st_bbox(c(xmin = 16.1, xmax = 16.6, ymax = 48.6, ymin = 47.9), crs = st_crs(4326))
#> xmin ymin xmax ymax 
#> 16.1 47.9 16.6 48.6 
```
