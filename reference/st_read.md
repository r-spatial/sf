# Read simple features or layers from file or database

Read simple features from file or database, or retrieve layer names and
their geometry type(s)

Read PostGIS table directly through DBI and RPostgreSQL interface,
converting Well-Know Binary geometries to sfc

## Usage

``` r
st_read(dsn, layer, ...)

# S3 method for class 'character'
st_read(
  dsn,
  layer,
  ...,
  query = NA,
  options = NULL,
  quiet = FALSE,
  geometry_column = 1L,
  type = 0,
  promote_to_multi = TRUE,
  stringsAsFactors = sf_stringsAsFactors(),
  int64_as_string = FALSE,
  check_ring_dir = FALSE,
  fid_column_name = character(0),
  drivers = character(0),
  wkt_filter = character(0),
  optional = FALSE,
  use_stream = default_st_read_use_stream()
)

read_sf(..., quiet = TRUE, stringsAsFactors = FALSE, as_tibble = TRUE)

# S3 method for class 'DBIObject'
st_read(
  dsn = NULL,
  layer = NULL,
  query = NULL,
  EWKB = TRUE,
  quiet = TRUE,
  as_tibble = FALSE,
  geometry_column = NULL,
  ...
)
```

## Arguments

- dsn:

  data source name (interpretation varies by driver - for some drivers,
  `dsn` is a file name, but may also be a folder, or contain the name
  and access credentials of a database); in case of GeoJSON, `dsn` may
  be the character string holding the geojson data. It can also be an
  open database connection.

- layer:

  layer name (varies by driver, may be a file name without extension);
  in case `layer` is missing, `st_read` will read the first layer of
  `dsn`, give a warning and (unless `quiet = TRUE`) print a message when
  there are multiple layers, or give an error if there are no layers in
  `dsn`. If `dsn` is a database connection, then `layer` can be a table
  name or a database identifier (see
  [`Id`](https://dbi.r-dbi.org/reference/Id.html)). It is also possible
  to omit `layer` and rather use the `query` argument.

- ...:

  parameter(s) passed on to
  [st_as_sf](https://r-spatial.github.io/sf/reference/st_as_sf.md)

- query:

  SQL query to select records; see details

- options:

  character; driver dependent dataset open options, multiple options
  supported. For possible values, see the "Open options" section of the
  GDAL documentation of the corresponding driver, and
  <https://github.com/r-spatial/sf/issues/1157> for an example.

- quiet:

  logical; suppress info on name, driver, size and spatial reference, or
  signaling no or multiple layers

- geometry_column:

  integer or character; in case of multiple geometry fields, which one
  to take?

- type:

  integer; ISO number of desired simple feature type; see details. If
  left zero, and `promote_to_multi` is `TRUE`, in case of mixed feature
  geometry types, conversion to the highest numeric type value found
  will be attempted. A vector with different values for each geometry
  column can be given.

- promote_to_multi:

  logical; in case of a mix of Point and MultiPoint, or of LineString
  and MultiLineString, or of Polygon and MultiPolygon, convert all to
  the Multi variety; defaults to `TRUE`

- stringsAsFactors:

  logical; logical: should character vectors be converted to factors?
  Default for `read_sf` or R version \>= 4.1.0 is `FALSE`, for `st_read`
  and R version \< 4.1.0 equal to
  [`default.stringsAsFactors()`](https://rdrr.io/r/base/base-defunct.html)

- int64_as_string:

  logical; if `TRUE`, Int64 attributes are returned as string; if
  `FALSE`, they are returned as double and a warning is given when
  precision is lost (i.e., values are larger than 2^53).

- check_ring_dir:

  logical; if `TRUE`, polygon ring directions are checked and if
  necessary corrected (when seen from above: exterior ring counter
  clockwise, holes clockwise)

- fid_column_name:

  character; name of column to write feature IDs to; defaults to not
  doing this

- drivers:

  character; limited set of driver short names to be tried (default: try
  all)

- wkt_filter:

  character; WKT representation of a spatial filter (may be used as
  bounding box, selecting overlapping geometries); see examples

- optional:

  logical; passed to
  [as.data.frame](https://rdrr.io/r/base/as.data.frame.html); always
  `TRUE` when `as_tibble` is `TRUE`

- use_stream:

  Use `TRUE` to use the experimental columnar interface introduced in
  GDAL 3.6.

- as_tibble:

  logical; should the returned table be of class tibble or data.frame?

- EWKB:

  logical; is the WKB of type EWKB? if missing, defaults to `TRUE`

## Value

object of class [sf](https://r-spatial.github.io/sf/reference/sf.md)
when a layer was successfully read; in case argument `layer` is missing
and data source `dsn` does not contain a single layer, an object of
class `sf_layers` is returned with the layer names, each with their
geometry type(s). Note that the number of layers may also be zero.

## Details

for `geometry_column`, see also
<https://gdal.org/en/latest/development/rfc/rfc41_multiple_geometry_fields.html>

for values for `type` see
<https://en.wikipedia.org/wiki/Well-known_text#Well-known_binary>, but
note that not every target value may lead to successful conversion. The
typical conversion from POLYGON (3) to MULTIPOLYGON (6) should work; the
other way around (type=3), secondary rings from MULTIPOLYGONS may be
dropped without warnings. `promote_to_multi` is handled on a
per-geometry column basis; `type` may be specified for each geometry
column.

Note that stray files in data source directories (such as `*.dbf`) may
lead to spurious errors that accompanying `*.shp` are missing.

In case of problems reading shapefiles from USB drives on OSX, please
see <https://github.com/r-spatial/sf/issues/252>. Reading shapefiles (or
other data sources) directly from zip files can be done by prepending
the path with `/vsizip/`. This is part of the GDAL Virtual File Systems
interface that also supports .gz, curl, and other operations, including
chaining; see
<https://gdal.org/en/latest/user/virtual_file_systems.html> for a
complete description and examples.

For `query` with a character `dsn` the query text is handed to
'ExecuteSQL' on the GDAL/OGR data set and will result in the creation of
a new layer (and `layer` is ignored). See 'OGRSQL'
<https://gdal.org/en/latest/user/ogr_sql_dialect.html> for details.
Please note that the 'FID' special field is driver-dependent, and may be
either 0-based (e.g. ESRI Shapefile), 1-based (e.g. MapInfo) or
arbitrary (e.g. OSM). Other features of OGRSQL are also likely to be
driver dependent. The available layer names may be obtained with
[st_layers](https://r-spatial.github.io/sf/reference/st_layers.md). Care
will be required to properly escape the use of some layer names.

`read_sf` and `write_sf` are aliases for `st_read` and `st_write`,
respectively, with some modified default arguments. `read_sf` and
`write_sf` are quiet by default: they do not print information about the
data source. `read_sf` returns an sf-tibble rather than an
sf-data.frame. `write_sf` delete layers by default: it overwrites
existing files without asking or warning.

if `table` is not given but `query` is, the spatial reference system
(crs) of the table queried is only available in case it has been stored
into each geometry record (e.g., by PostGIS, when using EWKB)

The function will automatically find the `geometry` type columns for
drivers that support it. For the other drivers, it will try to cast all
the character columns, which can be slow for very wide tables.

## Note

The use of `system.file` in examples make sure that examples run
regardless where R is installed: typical users will not use
`system.file` but give the file name directly, either with full path or
relative to the current working directory (see
[getwd](https://rdrr.io/r/base/getwd.html)). "Shapefiles" consist of
several files with the same basename that reside in the same directory,
only one of them having extension `.shp`.

## See also

[st_layers](https://r-spatial.github.io/sf/reference/st_layers.md),
[st_drivers](https://r-spatial.github.io/sf/reference/st_drivers.md)

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
summary(nc) # note that AREA was computed using Euclidian area on lon/lat degrees
#>       AREA          PERIMETER         CNTY_         CNTY_ID    
#>  Min.   :0.0420   Min.   :0.999   Min.   :1825   Min.   :1825  
#>  1st Qu.:0.0910   1st Qu.:1.324   1st Qu.:1902   1st Qu.:1902  
#>  Median :0.1205   Median :1.609   Median :1982   Median :1982  
#>  Mean   :0.1263   Mean   :1.673   Mean   :1986   Mean   :1986  
#>  3rd Qu.:0.1542   3rd Qu.:1.859   3rd Qu.:2067   3rd Qu.:2067  
#>  Max.   :0.2410   Max.   :3.640   Max.   :2241   Max.   :2241  
#>      NAME               FIPS               FIPSNO         CRESS_ID     
#>  Length:100         Length:100         Min.   :37001   Min.   :  1.00  
#>  Class :character   Class :character   1st Qu.:37050   1st Qu.: 25.75  
#>  Mode  :character   Mode  :character   Median :37100   Median : 50.50  
#>                                        Mean   :37100   Mean   : 50.50  
#>                                        3rd Qu.:37150   3rd Qu.: 75.25  
#>                                        Max.   :37199   Max.   :100.00  
#>      BIR74           SID74          NWBIR74           BIR79      
#>  Min.   :  248   Min.   : 0.00   Min.   :   1.0   Min.   :  319  
#>  1st Qu.: 1077   1st Qu.: 2.00   1st Qu.: 190.0   1st Qu.: 1336  
#>  Median : 2180   Median : 4.00   Median : 697.5   Median : 2636  
#>  Mean   : 3300   Mean   : 6.67   Mean   :1050.8   Mean   : 4224  
#>  3rd Qu.: 3936   3rd Qu.: 8.25   3rd Qu.:1168.5   3rd Qu.: 4889  
#>  Max.   :21588   Max.   :44.00   Max.   :8027.0   Max.   :30757  
#>      SID79          NWBIR79                 geometry  
#>  Min.   : 0.00   Min.   :    3.0   MULTIPOLYGON :100  
#>  1st Qu.: 2.00   1st Qu.:  250.5   epsg:4267    :  0  
#>  Median : 5.00   Median :  874.5   +proj=long...:  0  
#>  Mean   : 8.36   Mean   : 1352.8                      
#>  3rd Qu.:10.25   3rd Qu.: 1406.8                      
#>  Max.   :57.00   Max.   :11631.0                      

## only three fields by select clause
## only two features by where clause
nc_sql = st_read(system.file("shape/nc.shp", package="sf"),
                     query = "SELECT NAME, SID74, FIPS FROM \"nc\" WHERE BIR74 > 20000")
#> Reading query `SELECT NAME, SID74, FIPS FROM "nc" WHERE BIR74 > 20000'
#> from data source `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 2 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -81.06555 ymin: 34.82742 xmax: -78.49929 ymax: 35.50912
#> Geodetic CRS:  NAD27
if (FALSE) { # \dontrun{
  library(sp)
  example(meuse, ask = FALSE, echo = FALSE)
  try(st_write(st_as_sf(meuse), "PG:dbname=postgis", "meuse",
       layer_options = "OVERWRITE=true"))
  try(st_meuse <- st_read("PG:dbname=postgis", "meuse"))
  if (exists("st_meuse"))
    summary(st_meuse)
} # }

if (FALSE) { # \dontrun{
## note that we need special escaping of layer  within single quotes (nc.gpkg)
## and that geom needs to be included in the select, otherwise we don't detect it
layer <- st_layers(system.file("gpkg/nc.gpkg", package = "sf"))$name[1]
nc_gpkg_sql = st_read(system.file("gpkg/nc.gpkg", package = "sf"),
   query = sprintf("SELECT NAME, SID74, FIPS, geom  FROM \"%s\" WHERE BIR74 > 20000", layer))
} # }
# spatial filter, as wkt:
wkt = st_as_text(st_geometry(nc[1,]))
# filter by (bbox overlaps of) first feature geometry:
st_read(system.file("gpkg/nc.gpkg", package="sf"), wkt_filter = wkt)
#> Reading layer `nc.gpkg' from data source 
#>   `/home/runner/work/_temp/Library/sf/gpkg/nc.gpkg' using driver `GPKG'
#> Simple feature collection with 4 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -81.91116 ymin: 35.98933 xmax: -80.87086 ymax: 36.58965
#> Geodetic CRS:  NAD27
# read geojson from string:
geojson_txt <- paste("{\"type\":\"MultiPoint\",\"coordinates\":",
   "[[3.2,4],[3,4.6],[3.8,4.4],[3.5,3.8],[3.4,3.6],[3.9,4.5]]}")
x = st_read(geojson_txt)
#> Reading layer `OGRGeoJSON' from data source 
#>   `{"type":"MultiPoint","coordinates": [[3.2,4],[3,4.6],[3.8,4.4],[3.5,3.8],[3.4,3.6],[3.9,4.5]]}' 
#>   using driver `GeoJSON'
#> Simple feature collection with 1 feature and 0 fields
#> Geometry type: MULTIPOINT
#> Dimension:     XY
#> Bounding box:  xmin: 3 ymin: 3.6 xmax: 3.9 ymax: 4.6
#> Geodetic CRS:  WGS 84
x
#> Simple feature collection with 1 feature and 0 fields
#> Geometry type: MULTIPOINT
#> Dimension:     XY
#> Bounding box:  xmin: 3 ymin: 3.6 xmax: 3.9 ymax: 4.6
#> Geodetic CRS:  WGS 84
#>                         geometry
#> 1 MULTIPOINT ((3.2 4), (3 4.6...
if (FALSE) { # \dontrun{
library(RPostgreSQL)
try(conn <- dbConnect(PostgreSQL(), dbname = "postgis"))
if (exists("conn") && !inherits(conn, "try-error")) {
  x = st_read(conn, "meuse", query = "select * from meuse limit 3;")
  x = st_read(conn, table = "public.meuse")
  print(st_crs(x)) # SRID resolved by the database, not by GDAL!
  dbDisconnect(conn)
 }
} # }
```
