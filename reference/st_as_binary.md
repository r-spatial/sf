# Convert sfc object to an WKB object

Convert sfc object to an WKB object

## Usage

``` r
st_as_binary(x, ...)

# S3 method for class 'sfc'
st_as_binary(
  x,
  ...,
  EWKB = FALSE,
  endian = .Platform$endian,
  pureR = FALSE,
  precision = attr(x, "precision"),
  hex = FALSE
)

# S3 method for class 'sfg'
st_as_binary(
  x,
  ...,
  endian = .Platform$endian,
  EWKB = FALSE,
  pureR = FALSE,
  hex = FALSE,
  srid = 0
)
```

## Arguments

- x:

  object to convert

- ...:

  ignored

- EWKB:

  logical; use EWKB (PostGIS), or (default) ISO-WKB?

- endian:

  character; either "big" or "little"; default: use that of platform

- pureR:

  logical; use pure R solution, or C++?

- precision:

  numeric; if zero, do not modify; to reduce precision: negative values
  convert to float (4-byte real); positive values convert to
  round(x\*precision)/precision. See details.

- hex:

  logical; return as (unclassed) hexadecimal encoded character vector?

- srid:

  integer; override srid (can be used when the srid is unavailable
  locally).

## Details

`st_as_binary` is called on sfc objects on their way to the GDAL or GEOS
libraries, and hence does rounding (if requested) on the fly before e.g.
computing spatial predicates like
[st_intersects](https://r-spatial.github.io/sf/reference/geos_binary_pred.md).
The examples show a round-trip of an `sfc` to and from binary.

For the precision model used, see also
<https://locationtech.github.io/jts/javadoc/org/locationtech/jts/geom/PrecisionModel.html>.
There, it is written that: “... to specify 3 decimal places of
precision, use a scale factor of 1000. To specify -3 decimal places of
precision (i.e. rounding to the nearest 1000), use a scale factor of
0.001.”. Note that ALL coordinates, so also Z or M values (if present)
are affected.

## Examples

``` r
# examples of setting precision:
st_point(c(1/3, 1/6)) %>% st_sfc(precision = 1000) %>% st_as_binary %>% st_as_sfc
#> Geometry set for 1 feature 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0.333 ymin: 0.167 xmax: 0.333 ymax: 0.167
#> CRS:           NA
#> POINT (0.333 0.167)
st_point(c(1/3, 1/6)) %>% st_sfc(precision =  100) %>% st_as_binary %>% st_as_sfc
#> Geometry set for 1 feature 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0.33 ymin: 0.17 xmax: 0.33 ymax: 0.17
#> CRS:           NA
#> POINT (0.33 0.17)
st_point(1e6 * c(1/3, 1/6)) %>% st_sfc(precision = 0.01) %>% st_as_binary %>% st_as_sfc
#> Geometry set for 1 feature 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 333300 ymin: 166700 xmax: 333300 ymax: 166700
#> CRS:           NA
#> POINT (333300 166700)
st_point(1e6 * c(1/3, 1/6)) %>% st_sfc(precision = 0.001) %>% st_as_binary %>% st_as_sfc
#> Geometry set for 1 feature 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 333000 ymin: 167000 xmax: 333000 ymax: 167000
#> CRS:           NA
#> POINT (333000 167000)
```
