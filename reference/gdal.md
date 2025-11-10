# functions to interact with gdal not meant to be called directly by users (but e.g. by stars::read_stars)

functions to interact with gdal not meant to be called directly by users
(but e.g. by stars::read_stars)

## Usage

``` r
gdal_read(
  x,
  ...,
  options = character(0),
  driver = character(0),
  read_data = TRUE,
  NA_value = NA_real_,
  RasterIO_parameters = list()
)

gdal_write(
  x,
  ...,
  file,
  driver = "GTiff",
  options = character(0),
  type = "Float32",
  NA_value = NA_real_,
  geotransform,
  update = FALSE,
  scale_offset = c(1, 0)
)

gdal_inv_geotransform(gt)

gdal_crs(file, options = character(0))

gdal_metadata(
  file,
  domain_item = character(0),
  options = character(0),
  parse = TRUE
)

gdal_subdatasets(file, options = character(0), name = TRUE)

gdal_polygonize(
  x,
  mask = NULL,
  file = tempfile(),
  driver = "GTiff",
  use_integer = TRUE,
  geotransform,
  breaks = classInt::classIntervals(na.omit(as.vector(x[[1]])))$brks,
  use_contours = FALSE,
  contour_lines = FALSE,
  connect8 = FALSE,
  ...
)

gdal_rasterize(sf, x, gt, file, driver = "GTiff", options = character())

gdal_extract(
  f,
  pts,
  resampling = c("nearest", "bilinear", "cubic", "cubicspline")
)

gdal_read_mdim(
  file,
  array_name = character(0),
  options = character(0),
  offset = integer(0),
  count = integer(0),
  step = integer(0),
  proxy = FALSE,
  debug = FALSE
)

gdal_write_mdim(
  file,
  driver,
  dimx,
  cdl,
  wkt,
  xy,
  ...,
  root_group_options = character(0),
  options = character(0),
  as_float = TRUE
)

gdal_create(f, nxy, values, crs, xlim, ylim)
```

## Arguments

- x:

  character vector, possibly of length larger than 1 when more than one
  raster is read

- ...:

  ignored

- options:

  character; driver specific options regarding reading or creating the
  dataset

- driver:

  character; driver short name; when empty vector, driver is
  auto-detected.

- read_data:

  logical; if `FALSE`, only the imagery metadata is returned

- NA_value:

  (double) non-NA value to use for missing values; if `NA`, when writing
  missing values are not specially flagged in output dataset, when
  reading the default (dataset) missing values are used (if present /
  set).

- RasterIO_parameters:

  list with named parameters to GDAL's RasterIO; see the
  stars::read_stars documentation.

- file:

  file name

- type:

  gdal write type

- geotransform:

  length 6 numeric vector with GDAL geotransform parameters.

- update:

  logical; `TRUE` if in an existing raster file pixel values shall be
  updated.

- scale_offset:

  length 2 numeric; contains scale and offset values

- gt:

  double vector of length 6

- domain_item:

  character vector of length 0, 1 (with domain), or 2 (with domain and
  item); use `""` for the default domain, use `NA_character_` to query
  the domain names.

- parse:

  logical; should metadata be parsed into a named list (`TRUE`) or
  returned as character data?

- name:

  logical; retrieve name of subdataset? If `FALSE`, retrieve description

- mask:

  stars object with NA mask (0 where NA), or NULL

- use_integer:

  boolean; if `TRUE`, raster values are read as (and rounded to)
  unsigned 32-bit integers values; if `FALSE` they are read as 32-bit
  floating points numbers. The former is supposedly faster.

- breaks:

  numeric vector with break values for contour polygons (or lines)

- use_contours:

  logical;

- contour_lines:

  logical;

- connect8:

  logical; if `TRUE` use 8 connection algorithm, rather than 4

- sf:

  object of class `sf`

- f:

  character; file name

- pts:

  points matrix

- resampling:

  character; resampling method; for method cubic or cubicspline,
  `stars_proxy` objects should be used and GDAL should have version \>=
  3.10.0

- array_name:

  array name

- offset:

  offset (pixels)

- count:

  number of pixels to read

- step:

  step size (pixels)

- proxy:

  logical; return proxy object?

- debug:

  logical; print debug messages?

- dimx:

  integer named vector with dimensions of object

- cdl:

  list with variables, each having a named dim attribute

- wkt:

  character; WKT of crs

- xy:

  character; names of the spatial x and y dimension

- root_group_options:

  character; driver specific options regarding the creation of the root
  group

- as_float:

  logical; when `TRUE` write 4-byte floating point numbers, when `FALSE`
  write 8-byte doubles.

- nxy:

  integer vector of length 2

- values:

  fill value

- crs:

  object of class `crs`

- xlim:

  numeric

- ylim:

  numeric

## Value

object of class `crs`, see
[st_crs](https://r-spatial.github.io/sf/reference/st_crs.md).

named list with metadata items

`gdal_subdatasets` returns a zero-length list if `file` does not have
subdatasets, and else a named list with subdatasets.

## Details

These functions are exported for the single purpose of being used by
package stars, they are not meant to be used directly and may change or
disappear without prior notice or deprecation warnings.

gdal_inv_geotransform returns the inverse geotransform

gdal_crs reads coordinate reference system from GDAL data set

get_metadata gets metadata of a raster layer

gdal_subdatasets returns the subdatasets of a gdal dataset

## Examples

``` r
if (FALSE) { # \dontrun{
  f = system.file("tif/L7_ETMs.tif", package="stars")
  f = system.file("nc/avhrr-only-v2.19810901.nc", package = "stars")
  gdal_metadata(f)
  gdal_metadata(f, NA_character_)
  try(gdal_metadata(f, "wrongDomain"))
  gdal_metadata(f, c("", "AREA_OR_POINT"))
} # }
```
