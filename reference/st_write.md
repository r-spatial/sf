# Write simple features object to file or database

Write simple features object to file or database

## Usage

``` r
st_write(obj, dsn, layer, ...)

# S3 method for class 'sfc'
st_write(obj, dsn, layer, ...)

# S3 method for class 'sf'
st_write(
  obj,
  dsn,
  layer = NULL,
  ...,
  driver = guess_driver_can_write(dsn),
  dataset_options = NULL,
  layer_options = NULL,
  quiet = FALSE,
  factorsAsCharacter = TRUE,
  append = NA,
  delete_dsn = FALSE,
  delete_layer = !is.na(append) && !append,
  fid_column_name = NULL,
  config_options = character(0)
)

# S3 method for class 'data.frame'
st_write(obj, dsn, layer = NULL, ...)

write_sf(..., quiet = TRUE, append = FALSE, delete_layer = !append)

st_delete(
  dsn,
  layer = character(0),
  driver = guess_driver_can_write(dsn),
  quiet = FALSE
)
```

## Arguments

- obj:

  object of class `sf` or `sfc`

- dsn:

  data source name. Interpretation varies by driver: can be a filename,
  a folder, a database name, or a Database Connection (we officially
  test support for
  [`RPostgres::Postgres()`](https://rpostgres.r-dbi.org/reference/Postgres.html)
  connections).

- layer:

  layer name. Varies by driver, may be a file name without extension;
  for database connection, it is the name of the table. If layer is
  missing, the `basename` of `dsn` is taken.

- ...:

  other arguments passed to
  [dbWriteTable](https://dbi.r-dbi.org/reference/dbWriteTable.html) when
  `dsn` is a Database Connection

- driver:

  character; name of driver to be used; if missing and `dsn` is not a
  Database Connection, a driver name is guessed from `dsn`;
  [`st_drivers()`](https://r-spatial.github.io/sf/reference/st_drivers.md)
  returns the drivers that are available with their properties; links to
  full driver documentation are found at
  <https://gdal.org/en/latest/drivers/vector/index.html>

- dataset_options:

  character; driver dependent dataset creation options; multiple options
  supported.

- layer_options:

  character; driver dependent layer creation options; multiple options
  supported.

- quiet:

  logical; suppress info on name, driver, size and spatial reference

- factorsAsCharacter:

  logical; convert `factor` levels to character strings (`TRUE`,
  default), otherwise into numbers when factorsAsCharacter is `FALSE`.
  For database connections, `factorsAsCharacter` is always `TRUE`.

- append:

  logical; should we append to an existing layer, or replace it? if
  `TRUE` append, if `FALSE` replace. The default for `st_write` is `NA`
  which raises an error if the layer exists. The default for `write_sf`
  is `FALSE`, which overwrites any existing data. See also next two
  arguments for more control on overwrite behavior.

- delete_dsn:

  logical; delete data source `dsn` before attempting to write?

- delete_layer:

  logical; delete layer `layer` before attempting to write? The default
  for `st_write` is `FALSE` which raises an error if the layer exists.
  The default for `write_sf` is `TRUE`.

- fid_column_name:

  character, name of column with feature IDs; if specified, this column
  is no longer written as feature attribute.

- config_options:

  character, named vector with GDAL config options

## Value

`obj`, invisibly

## Details

Columns (variables) of a class not supported are dropped with a warning.

When updating an existing layer, records are appended to it if the
updating object has the right variable names and types. If names don't
match an error is raised. If types don't match, behaviour is undefined:
GDAL may raise warnings or errors or fail silently.

When deleting layers or data sources is not successful, no error is
emitted. `delete_dsn` and `delete_layer` should be handled with care;
the former may erase complete directories or databases.

`st_delete()` deletes layer(s) in a data source, or a data source if
layers are omitted; it returns `TRUE` on success, `FALSE` on failure,
invisibly.

## See also

[st_drivers](https://r-spatial.github.io/sf/reference/st_drivers.md),
[dbWriteTable](https://dbi.r-dbi.org/reference/dbWriteTable.html)

## Examples

``` r
nc = st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
st_write(nc, paste0(tempdir(), "/", "nc.shp"))
#> Writing layer `nc' to data source 
#>   `/tmp/RtmpvfIYqN/nc.shp' using driver `ESRI Shapefile'
#> Writing 100 features with 14 fields and geometry type Multi Polygon.
st_write(nc, paste0(tempdir(), "/", "nc.shp"), delete_layer = TRUE) # overwrites
#> Deleting layer `nc' using driver `ESRI Shapefile'
#> Writing layer `nc' to data source 
#>   `/tmp/RtmpvfIYqN/nc.shp' using driver `ESRI Shapefile'
#> Writing 100 features with 14 fields and geometry type Multi Polygon.
if (require(sp, quietly = TRUE)) {
 data(meuse, package = "sp") # loads data.frame from sp
 meuse_sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
 # writes X and Y as columns:
 st_write(meuse_sf, paste0(tempdir(), "/", "meuse.csv"), layer_options = "GEOMETRY=AS_XY")
 st_write(meuse_sf, paste0(tempdir(), "/", "meuse.csv"), layer_options = "GEOMETRY=AS_WKT",
   delete_dsn=TRUE) # overwrites
if (FALSE) { # \dontrun{
 library(sp)
 example(meuse, ask = FALSE, echo = FALSE)
 try(st_write(st_as_sf(meuse), "PG:dbname=postgis", "meuse_sf",
     layer_options = c("OVERWRITE=yes", "LAUNDER=true")))
 demo(nc, ask = FALSE)
 try(st_write(nc, "PG:dbname=postgis", "sids", layer_options = "OVERWRITE=true"))
} # }
}
#> Writing layer `meuse' to data source `/tmp/RtmpvfIYqN/meuse.csv' using driver `CSV'
#> options:        GEOMETRY=AS_XY 
#> Writing 155 features with 12 fields and geometry type Point.
#> Deleting source `/tmp/RtmpvfIYqN/meuse.csv' using driver `CSV'
#> Writing layer `meuse' to data source `/tmp/RtmpvfIYqN/meuse.csv' using driver `CSV'
#> options:        GEOMETRY=AS_WKT 
#> Writing 155 features with 12 fields and geometry type Point.
```
