# Retrieve coordinate reference system from object

Retrieve coordinate reference system from sf or sfc object

Set or replace retrieve coordinate reference system from object

## Usage

``` r
st_crs(x, ...)

# S3 method for class 'sf'
st_crs(x, ...)

# S3 method for class 'numeric'
st_crs(x, ...)

# S3 method for class 'character'
st_crs(x, ...)

# S3 method for class 'sfc'
st_crs(x, ..., parameters = FALSE)

# S3 method for class 'bbox'
st_crs(x, ...)

# S3 method for class 'CRS'
st_crs(x, ...)

# S3 method for class 'crs'
st_crs(x, ...)

st_crs(x) <- value

# S3 method for class 'sf'
st_crs(x) <- value

# S3 method for class 'sfc'
st_crs(x) <- value

st_set_crs(x, value)

NA_crs_

# S3 method for class 'crs'
is.na(x)

# S3 method for class 'crs'
x$name

# S3 method for class 'crs'
format(x, ...)

st_axis_order(authority_compliant = logical(0))
```

## Format

An object of class `crs` of length 2.

## Arguments

- x:

  numeric, character, or object of class
  [sf](https://r-spatial.github.io/sf/reference/sf.md) or
  [sfc](https://r-spatial.github.io/sf/reference/sfc.md)

- ...:

  ignored

- parameters:

  logical; `FALSE` by default; if `TRUE` return a list of coordinate
  reference system parameters, with named elements `SemiMajor`,
  `InvFlattening`, `units_gdal`, `IsVertical`, `WktPretty`, and `Wkt`

- value:

  one of (i) character: a string accepted by GDAL, (ii) integer, a valid
  EPSG value (numeric), or (iii) an object of class `crs`.

- name:

  element name

- authority_compliant:

  logical; specify whether axis order should be handled compliant to the
  authority; if omitted, the current value is printed.

## Value

If `x` is numeric, return `crs` object for EPSG:`x`; if `x` is
character, return `crs` object for `x`; if `x` is of class `sf` or
`sfc`, return its `crs` object.

Object of class `crs`, which is a list with elements `input` (length-1
character) and `wkt` (length-1 character). Elements may be `NA` valued;
if all elements are `NA` the CRS is missing valued, and coordinates are
assumed to relate to an arbitrary Cartesian coordinate system.

`st_axis_order` returns the (logical) current value if called without
argument, or (invisibly) the previous value if it is being set.

## Details

The \*crs functions create, get, set or replace the `crs` attribute of a
simple feature geometry list-column. This attribute is of class `crs`,
and is a list consisting of `input` (user input, e.g. "EPSG:4326" or
"WGS84" or a proj4string), and `wkt`, an automatically generated wkt2
representation of the crs. If `x` is identical to the wkt2
representation, and the CRS has a name, this name is used for the
`input` field.

Comparison of two objects of class `crs` uses the GDAL function
`OGRSpatialReference::IsSame`.

In case a coordinate reference system is replaced, no transformation
takes place and a warning is raised to stress this.

`NA_crs_` is the `crs` object with missing values for `input` and `wkt`.

the `$` method for `crs` objects retrieves named elements using the GDAL
interface; named elements include `SemiMajor`, `SemiMinor`,
`InvFlattening`, `IsGeographic`, `units_gdal`, `IsVertical`,
`WktPretty`, `Wkt`, `Name`, `proj4string`, `epsg`, `yx`, `ud_unit`, and
`axes` (this may be subject to changes in future GDAL versions).

Note that not all valid CRS have a corresponding `proj4string`.

`ud_unit` returns a valid
[units](https://r-quantities.github.io/units/reference/units.html)
object or `NULL` if units are missing.

format.crs returns NA if the crs is missing valued, or else the name of
a crs if it is different from "unknown", or else the user input if it
was set, or else its "proj4string" representation;

`st_axis_order` can be used to get and set the axis order: `TRUE`
indicates axes order according to the authority (e.g. EPSG:4326 defining
coordinates to be latitude,longitude pairs), `FALSE` indicates the usual
GIS (display) order (longitude,latitude). This can be useful when data
are read, or have to be written, with coordinates in authority compliant
order. The return value is the current state of this (`FALSE`, by
default).

## Examples

``` r
sfc = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
sf = st_sf(a = 1:2, geom = sfc)
st_crs(sf) = 4326
st_geometry(sf)
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 1 ymax: 1
#> Geodetic CRS:  WGS 84
#> POINT (0 0)
#> POINT (1 1)
sfc = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
st_crs(sfc) = 4326
sfc
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 1 ymax: 1
#> Geodetic CRS:  WGS 84
#> POINT (0 0)
#> POINT (1 1)
sfc = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
sfc %>% st_set_crs(4326) %>% st_transform(3857)
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 111319.5 ymax: 111325.1
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> POINT (0 0)
#> POINT (111319.5 111325.1)
st_crs("EPSG:3857")$input
#> [1] "EPSG:3857"
st_crs(3857)$proj4string
#> [1] "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"
pt = st_sfc(st_point(c(0, 60)), crs = 4326)
# st_axis_order() only has effect in GDAL >= 2.5.0:
st_axis_order() # query default: FALSE means interpret pt as (longitude latitude)
#> [1] FALSE
st_transform(pt, 3857)[[1]]
#> POINT (0 8399738)
old_value = FALSE
if (compareVersion(sf_extSoftVersion()["GDAL"], "2.5.0") >= 0)
   (old_value = st_axis_order(TRUE))
#> [1] FALSE
# now interpret pt as (latitude longitude), as EPSG:4326 prescribes:
st_axis_order() # query current value
#> [1] TRUE
st_transform(pt, 3857)[[1]]
#> POINT (6679169 0)
st_axis_order(old_value) # set back to old value
```
