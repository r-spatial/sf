# Convert foreign geometry object to an sfc object

Convert foreign geometry object to an sfc object

## Usage

``` r
# S3 method for class 'pq_geometry'
st_as_sfc(
  x,
  ...,
  EWKB = TRUE,
  spatialite = FALSE,
  pureR = FALSE,
  crs = NA_crs_
)

# S3 method for class 'list'
st_as_sfc(x, ..., crs = NA_crs_)

# S3 method for class 'blob'
st_as_sfc(x, ...)

# S3 method for class 'bbox'
st_as_sfc(x, ...)

# S3 method for class 'WKB'
st_as_sfc(
  x,
  ...,
  EWKB = FALSE,
  spatialite = FALSE,
  pureR = FALSE,
  crs = NA_crs_
)

# S3 method for class 'raw'
st_as_sfc(x, ...)

# S3 method for class 'character'
st_as_sfc(x, crs = NA_integer_, ..., GeoJSON = FALSE)

# S3 method for class 'factor'
st_as_sfc(x, ...)

st_as_sfc(x, ...)

# S3 method for class 'SpatialPoints'
st_as_sfc(x, ..., precision = 0)

# S3 method for class 'SpatialPixels'
st_as_sfc(x, ..., precision = 0)

# S3 method for class 'SpatialMultiPoints'
st_as_sfc(x, ..., precision = 0)

# S3 method for class 'SpatialLines'
st_as_sfc(x, ..., precision = 0, forceMulti = FALSE)

# S3 method for class 'SpatialPolygons'
st_as_sfc(x, ..., precision = 0, forceMulti = FALSE)

# S3 method for class 'map'
st_as_sfc(x, ...)

# S3 method for class 's2_geography'
st_as_sfc(
  x,
  ...,
  crs = st_crs(4326),
  endian = match(.Platform$endian, c("big", "little")) - 1L
)
```

## Arguments

- x:

  object to convert

- ...:

  further arguments

- EWKB:

  logical; if `TRUE`, parse as EWKB (extended WKB; PostGIS: ST_AsEWKB),
  otherwise as ISO WKB (PostGIS: ST_AsBinary)

- spatialite:

  logical; if `TRUE`, WKB is assumed to be in the spatialite dialect,
  see <https://www.gaia-gis.it/gaia-sins/BLOB-Geometry.html>; this is
  only supported in native endian-ness (i.e., files written on system
  with the same endian-ness as that on which it is being read).

- pureR:

  logical; if `TRUE`, use only R code, if `FALSE`, use compiled (C++)
  code; use `TRUE` when the endian-ness of the binary differs from the
  host machine (`.Platform$endian`).

- crs:

  coordinate reference system to be assigned; object of class `crs`

- GeoJSON:

  logical; if `TRUE`, try to read geometries from GeoJSON text strings
  geometry, see
  [`st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.md)

- precision:

  precision value; see
  [st_as_binary](https://r-spatial.github.io/sf/reference/st_as_binary.md)

- forceMulti:

  logical; if `TRUE`, force coercion into `MULTIPOLYGON` or `MULTILINE`
  objects, else autodetect

- endian:

  integer; 0 or 1: defaults to the endian of the native machine

## Details

When converting from WKB, the object `x` is either a character vector
such as typically obtained from PostGIS (either with leading "0x" or
without), or a list with raw vectors representing the features in binary
(raw) form.

If `x` is a character vector, it should be a vector containing
[well-known-text](https://www.ogc.org/standards/wkt-crs/), or Postgis
EWKT or GeoJSON representations of a single geometry for each vector
element.

If `x` is a `factor`, it is converted to `character`.

## Examples

``` r
wkb = structure(list("01010000204071000000000000801A064100000000AC5C1441"), class = "WKB")
st_as_sfc(wkb, EWKB = TRUE)
#> Geometry set for 1 feature 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 181072 ymin: 333611 xmax: 181072 ymax: 333611
#> Projected CRS: Amersfoort / RD New
#> POINT (181072 333611)
wkb = structure(list("0x01010000204071000000000000801A064100000000AC5C1441"), class = "WKB")
st_as_sfc(wkb, EWKB = TRUE)
#> Geometry set for 1 feature 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 181072 ymin: 333611 xmax: 181072 ymax: 333611
#> Projected CRS: Amersfoort / RD New
#> POINT (181072 333611)
st_as_sfc(st_as_binary(st_sfc(st_point(0:1)))[[1]], crs = 4326)
#> Geometry set for 1 feature 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 1 xmax: 0 ymax: 1
#> Geodetic CRS:  WGS 84
#> POINT (0 1)
st_as_sfc("SRID=3978;LINESTRING(1663106 -105415,1664320 -104617)")
#> Geometry set for 1 feature 
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1663106 ymin: -105415 xmax: 1664320 ymax: -104617
#> Projected CRS: NAD83 / Canada Atlas Lambert
#> LINESTRING (1663106 -105415, 1664320 -104617)
```
