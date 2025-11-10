# Transform or convert coordinates of simple feature

Transform or convert coordinates of simple feature

## Usage

``` r
st_can_transform(src, dst)

st_transform(x, crs, ...)

# S3 method for class 'sfc'
st_transform(
  x,
  crs = st_crs(x),
  ...,
  aoi = numeric(0),
  pipeline = character(0),
  reverse = FALSE,
  desired_accuracy = -1,
  allow_ballpark = TRUE,
  partial = TRUE,
  check = FALSE
)

# S3 method for class 'sf'
st_transform(x, crs = st_crs(x), ...)

# S3 method for class 'sfg'
st_transform(x, crs = st_crs(x), ...)

# S3 method for class 'bbox'
st_transform(x, crs, ..., densify = 21)

st_wrap_dateline(x, options, quiet)

# S3 method for class 'sfc'
st_wrap_dateline(x, options = "WRAPDATELINE=YES", quiet = TRUE)

# S3 method for class 'sf'
st_wrap_dateline(x, options = "WRAPDATELINE=YES", quiet = TRUE)

# S3 method for class 'sfg'
st_wrap_dateline(x, options = "WRAPDATELINE=YES", quiet = TRUE)

sf_proj_info(type = "proj", path)
```

## Arguments

- src:

  source crs

- dst:

  destination crs

- x:

  object of class sf, sfc or sfg

- crs:

  target coordinate reference system: object of class `crs`, or input
  string for
  [st_crs](https://r-spatial.github.io/sf/reference/st_crs.md)

- ...:

  ignored

- aoi:

  area of interest, in degrees: WestLongitude, SouthLatitude,
  EastLongitude, NorthLatitude

- pipeline:

  character; coordinate operation pipeline, for overriding the default
  operation

- reverse:

  boolean; has only an effect when `pipeline` is defined: if `TRUE`, the
  inverse operation of the pipeline is applied

- desired_accuracy:

  numeric; Only coordinate operations that offer an accuracy of at least
  the one specified will be considered; a negative value disables this
  feature (requires GDAL \>= 3.3)

- allow_ballpark:

  logical; are ballpark (low accuracy) transformations allowed?
  (requires GDAL \>= 3.3)

- partial:

  logical; allow for partial projection, if not all points of a geometry
  can be projected (corresponds to setting environment variable
  `OGR_ENABLE_PARTIAL_REPROJECTION` to `TRUE`)

- check:

  logical; if `TRUE`, perform a sanity check on resulting polygons

- densify:

  integer, number of points for discretizing lines between bounding box
  corner points; see Details

- options:

  character; should have "WRAPDATELINE=YES" to function; another
  parameter that is used is "DATELINEOFFSET=10" (where 10 is the default
  value)

- quiet:

  logical; print options after they have been parsed?

- type:

  character; one of `have_datum_files`, `proj`, `ellps`, `datum`,
  `units`, `path`, or `prime_meridians`; see Details.

- path:

  character; PROJ search path to be set

## Details

`st_can_transform` returns a boolean indicating whether coordinates with
CRS src can be transformed into CRS dst

Transforms coordinates of object to new projection. Features that cannot
be transformed are returned as empty geometries. Transforms using the
`pipeline=` argument may fail if there is ambiguity in the axis order of
the specified coordinate reference system; if you need the traditional
GIS order, use `"OGC:CRS84"`, not `"EPSG:4326"`. Extra care is needed
with the ESRI Shapefile format, because WKT1 does not store axis order
unambiguously.

The `st_transform` method for `sfg` objects assumes that the CRS of the
object is available as an attribute of that name.

the method for `bbox` objects densifies lines for geographic coordinates
along Cartesian lines, not great circle arcs

For a discussion of using `options`, see
<https://github.com/r-spatial/sf/issues/280> and
<https://github.com/r-spatial/sf/issues/1983>

`sf_proj_info` lists the available projections, ellipses, datums, units,
or data search path of the PROJ library when `type` is equal to proj,
ellps, datum, units or path; when `type` equals `have_datum_files` a
boolean is returned indicating whether datum files are installed and
accessible (checking for `conus`). `path` returns the
`PROJ_INFO.searchpath` field directly, as a single string with path
separaters (`:` or `;`).

for PROJ \>= 6, `sf_proj_info` does not provide option
`type = "datums"`. PROJ \< 6 does not provide the option
`type = "prime_meridians"`.

for PROJ \>= 7.1.0, the "units" query of `sf_proj_info` returns the
`to_meter` variable as numeric, previous versions return a character
vector containing a numeric expression.

## See also

[st_transform_proj](https://r-spatial.github.io/lwgeom/reference/st_transform_proj.html),
part of package lwgeom.

[sf_project](https://r-spatial.github.io/sf/reference/sf_project.md)
projects a matrix of coordinates, bypassing GDAL altogether

[st_break_antimeridian](https://r-spatial.github.io/sf/reference/st_break_antimeridian.md)

## Examples

``` r
p1 = st_point(c(7,52))
p2 = st_point(c(-30,20))
sfc = st_sfc(p1, p2, crs = 4326)
sfc
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -30 ymin: 20 xmax: 7 ymax: 52
#> Geodetic CRS:  WGS 84
#> POINT (7 52)
#> POINT (-30 20)
st_transform(sfc, 3857)
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -3339585 ymin: 2273031 xmax: 779236.4 ymax: 6800125
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> POINT (779236.4 6800125)
#> POINT (-3339585 2273031)
st_transform(st_sf(a=2:1, geom=sfc), "EPSG:3857")
#> Simple feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -3339585 ymin: 2273031 xmax: 779236.4 ymax: 6800125
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   a                     geom
#> 1 2 POINT (779236.4 6800125)
#> 2 1 POINT (-3339585 2273031)
if (compareVersion(sf_extSoftVersion()["GDAL"], "3.0.0") >= 0) {
  st_transform(sfc, pipeline =
    "+proj=pipeline +step +proj=axisswap +order=2,1") # reverse axes
  st_transform(sfc, pipeline =
    "+proj=pipeline +step +proj=axisswap +order=2,1", reverse = TRUE) # also reverse axes
}
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -30 ymin: 20 xmax: 7 ymax: 52
#> CRS:           NA
#> POINT (7 52)
#> POINT (-30 20)
nc = st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
st_area(nc[1,]) # area from long/lat
#> 1137107793 [m^2]
st_area(st_transform(nc[1,], 32119)) # NC state plane, m
#> 1137590142 [m^2]
st_area(st_transform(nc[1,], 2264)) # NC state plane, US foot
#> 12244869403 [US_survey_foot^2]
library(units)
#> udunits database from /usr/share/xml/udunits/udunits2.xml
set_units(st_area(st_transform(nc[1,], 2264)), m^2)
#> 1137590142 [m^2]
st_transform(structure(p1, proj4string = "EPSG:4326"), "EPSG:3857")
#> POINT (779236.4 6800125)
st_wrap_dateline(st_sfc(st_linestring(rbind(c(-179,0),c(179,0))), crs = 4326))
#> Geometry set for 1 feature 
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -180 ymin: 0 xmax: 180 ymax: 0
#> Geodetic CRS:  WGS 84
#> MULTILINESTRING ((-179 0, -180 0), (180 0, 179 0))
sf_proj_info("datum")
#> data frame with 0 columns and 0 rows
```
