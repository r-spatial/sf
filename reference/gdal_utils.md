# Native interface to gdal utils

Native interface to gdal utils

## Usage

``` r
gdal_utils(
  util = "info",
  source,
  destination,
  options = character(0),
  quiet = !(util %in% c("info", "gdalinfo", "ogrinfo", "vectorinfo", "mdiminfo")) ||
    ("-multi" %in% options),
  processing = character(0),
  colorfilename = character(0),
  config_options = character(0),
  read_only = FALSE
)
```

## Arguments

- util:

  character; one of `info`, `warp`, `rasterize`, `translate`,
  `vectortranslate` (for ogr2ogr), `buildvrt`, `demprocessing`,
  `nearblack`, `grid`, `mdiminfo` and `mdimtranslate` (the last two
  requiring GDAL 3.1), `ogrinfo` (requiring GDAL 3.7), `footprint`
  (requiring GDAL 3.8)

- source:

  character; name of input layer(s); for `warp`, `buidvrt` or
  `mdimtranslate` this can be more than one

- destination:

  character; name of output layer

- options:

  character; options for the utility

- quiet:

  logical; if `TRUE`, suppress printing the output for `info` and
  `mdiminfo`, and suppress printing progress

- processing:

  character; processing options for `demprocessing`

- colorfilename:

  character; name of color file for `demprocessing` (mandatory if
  `processing="color-relief"`)

- config_options:

  named character vector with GDAL config options, like
  `c(option1=value1, option2=value2)`

- read_only:

  logical; only for `ogrinfo`: if `TRUE`, source is opened in read-only
  mode

## Value

`info` returns a character vector with the raster metadata; all other
utils return (invisibly) a logical indicating success (i.e., `TRUE`); in
case of failure, an error is raised.

## See also

[gdal_addo](https://r-spatial.github.io/sf/reference/gdal_addo.md) for
adding overlays to a raster file;
[st_layers](https://r-spatial.github.io/sf/reference/st_layers.md) to
query geometry type(s) and crs from layers in a (vector) data source

## Examples

``` r
if (compareVersion(sf_extSoftVersion()["GDAL"], "2.1.0") == 1) {
# info utils can be used to list information about a raster
# dataset. More info: https://gdal.org/programs/gdalinfo.html
in_file <- system.file("tif/geomatrix.tif", package = "sf")
gdal_utils("info", in_file, options = c("-mm", "-proj4"))

# vectortranslate utils can be used to convert simple features data between
# file formats. More info: https://gdal.org/programs/ogr2ogr.html
in_file <- system.file("shape/storms_xyz.shp", package="sf")
out_file <- paste0(tempfile(), ".gpkg")
gdal_utils(
  util = "vectortranslate",
  source = in_file,
  destination = out_file, # output format must be specified for GDAL < 2.3
  options = c("-f", "GPKG")
)
# The parameters can be specified as c("name") or c("name", "value"). The
# vectortranslate utils can perform also various operations during the
# conversion process. For example, we can reproject the features during the
# translation.
gdal_utils(
  util = "vectortranslate",
  source = in_file,
  destination = out_file,
  options = c(
  "-f", "GPKG", # output file format for GDAL < 2.3
  "-s_srs", "EPSG:4326", # input file SRS
  "-t_srs", "EPSG:2264", # output file SRS
  "-overwrite"
  )
)
st_read(out_file)
# The parameter s_srs had to be specified because, in this case, the in_file
# has no associated SRS.
st_read(in_file)
}
#> Driver: GTiff/GeoTIFF
#> Files: /home/runner/work/_temp/Library/sf/tif/geomatrix.tif
#> Size is 20, 20
#> Coordinate System is:
#> PROJCRS["WGS 84 / UTM zone 11N",
#>     BASEGEOGCRS["WGS 84",
#>         ENSEMBLE["World Geodetic System 1984 ensemble",
#>             MEMBER["World Geodetic System 1984 (Transit)"],
#>             MEMBER["World Geodetic System 1984 (G730)"],
#>             MEMBER["World Geodetic System 1984 (G873)"],
#>             MEMBER["World Geodetic System 1984 (G1150)"],
#>             MEMBER["World Geodetic System 1984 (G1674)"],
#>             MEMBER["World Geodetic System 1984 (G1762)"],
#>             MEMBER["World Geodetic System 1984 (G2139)"],
#>             ELLIPSOID["WGS 84",6378137,298.257223563,
#>                 LENGTHUNIT["metre",1]],
#>             ENSEMBLEACCURACY[2.0]],
#>         PRIMEM["Greenwich",0,
#>             ANGLEUNIT["degree",0.0174532925199433]],
#>         ID["EPSG",4326]],
#>     CONVERSION["UTM zone 11N",
#>         METHOD["Transverse Mercator",
#>             ID["EPSG",9807]],
#>         PARAMETER["Latitude of natural origin",0,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8801]],
#>         PARAMETER["Longitude of natural origin",-117,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8802]],
#>         PARAMETER["Scale factor at natural origin",0.9996,
#>             SCALEUNIT["unity",1],
#>             ID["EPSG",8805]],
#>         PARAMETER["False easting",500000,
#>             LENGTHUNIT["metre",1],
#>             ID["EPSG",8806]],
#>         PARAMETER["False northing",0,
#>             LENGTHUNIT["metre",1],
#>             ID["EPSG",8807]]],
#>     CS[Cartesian,2],
#>         AXIS["(E)",east,
#>             ORDER[1],
#>             LENGTHUNIT["metre",1]],
#>         AXIS["(N)",north,
#>             ORDER[2],
#>             LENGTHUNIT["metre",1]],
#>     USAGE[
#>         SCOPE["Navigation and medium accuracy spatial referencing."],
#>         AREA["Between 120°W and 114°W, northern hemisphere between equator and 84°N, onshore and offshore. Canada - Alberta; British Columbia (BC); Northwest Territories (NWT); Nunavut. Mexico. United States (USA)."],
#>         BBOX[0,-120,84,-114]],
#>     ID["EPSG",32611]]
#> Data axis to CRS axis mapping: 1,2
#> PROJ.4 string is:
#> '+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs'
#> GeoTransform =
#>   1841001.75, 1.5, -5
#>   1144003.25, -5, -1.5
#> Metadata:
#>   AREA_OR_POINT=Point
#> Image Structure Metadata:
#>   INTERLEAVE=BAND
#> Corner Coordinates:
#> Upper Left  ( 1841001.750, 1144003.250) (104d50'47.45"W, 10d 7'13.55"N)
#> Lower Left  ( 1840901.750, 1143973.250) (104d50'50.69"W, 10d 7'12.72"N)
#> Upper Right ( 1841031.750, 1143903.250) (104d50'46.60"W, 10d 7'10.33"N)
#> Lower Right ( 1840931.750, 1143873.250) (104d50'49.85"W, 10d 7' 9.50"N)
#> Center      ( 1840966.750, 1143938.250) (104d50'48.65"W, 10d 7'11.53"N)
#> Band 1 Block=20x20 Type=Byte, ColorInterp=Gray
#>     Computed Min/Max=74.000,255.000
#> Reading layer `storms_xyz' from data source `/tmp/Rtmp1cUzFS/file2264e122fdc.gpkg' using driver `GPKG'
#> Simple feature collection with 71 features and 0 fields
#> Geometry type: LINESTRING
#> Dimension:     XYZ
#> Bounding box:  xmin: -5785269 ymin: -8509454 xmax: 25097160 ymax: 15846560
#> z_range:       zmin: 924 zmax: 1017
#> Projected CRS: NAD83 / North Carolina (ftUS)
#> Reading layer `storms_xyz' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/storms_xyz.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 71 features and 0 fields
#> Geometry type: LINESTRING
#> Dimension:     XYZ
#> Bounding box:  xmin: -102.2 ymin: 8.3 xmax: 0 ymax: 59.5
#> z_range:       zmin: 924 zmax: 1017
#> CRS:           NA
```
