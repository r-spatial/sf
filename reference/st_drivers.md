# Get GDAL drivers

Get a list of the available GDAL drivers

## Usage

``` r
st_drivers(what = "vector", regex)
```

## Arguments

- what:

  character: `"vector"` or `"raster"`, anything else will return all
  drivers.

- regex:

  character; regular expression to filter the `name` and `long_name`
  fields on

## Value

A `data.frame` with driver metadata.

## Details

The drivers available will depend on the installation of GDAL/OGR, and
can vary; the `st_drivers()` function shows all the drivers that are
readable, and which may be written. The field `vsi` refers to the
driver's capability to read/create datasets through the VSI\*L API. [See
GDAL website for additional details on driver
support](https://gdal.org/en/latest/drivers/vector/index.html)

## Examples

``` r
# The following driver lists depend on the GDAL setup and platform used:
st_drivers()
#>                          name
#> FITS                     FITS
#> PCIDSK                 PCIDSK
#> netCDF                 netCDF
#> PDS4                     PDS4
#> VICAR                   VICAR
#> JP2OpenJPEG       JP2OpenJPEG
#> PDF                       PDF
#> MBTiles               MBTiles
#> BAG                       BAG
#> EEDA                     EEDA
#> OGCAPI                 OGCAPI
#> ESRI Shapefile ESRI Shapefile
#> MapInfo File     MapInfo File
#> UK .NTF               UK .NTF
#> LVBAG                   LVBAG
#> OGR_SDTS             OGR_SDTS
#> S57                       S57
#> DGN                       DGN
#> OGR_VRT               OGR_VRT
#> Memory                 Memory
#> CSV                       CSV
#> NAS                       NAS
#> GML                       GML
#> GPX                       GPX
#> LIBKML                 LIBKML
#> KML                       KML
#> GeoJSON               GeoJSON
#> GeoJSONSeq         GeoJSONSeq
#> ESRIJSON             ESRIJSON
#> TopoJSON             TopoJSON
#> Interlis 1         Interlis 1
#> Interlis 2         Interlis 2
#> OGR_GMT               OGR_GMT
#> GPKG                     GPKG
#> SQLite                 SQLite
#> ODBC                     ODBC
#> WAsP                     WAsP
#> PGeo                     PGeo
#> MSSQLSpatial     MSSQLSpatial
#> OGR_OGDI             OGR_OGDI
#> PostgreSQL         PostgreSQL
#> MySQL                   MySQL
#> OpenFileGDB       OpenFileGDB
#> DXF                       DXF
#> CAD                       CAD
#> FlatGeobuf         FlatGeobuf
#> Geoconcept         Geoconcept
#> GeoRSS                 GeoRSS
#> VFK                       VFK
#> PGDUMP                 PGDUMP
#> OSM                       OSM
#> GPSBabel             GPSBabel
#> OGR_PDS               OGR_PDS
#> WFS                       WFS
#> OAPIF                   OAPIF
#> SOSI                     SOSI
#> EDIGEO                 EDIGEO
#> SVG                       SVG
#> Idrisi                 Idrisi
#> XLS                       XLS
#> ODS                       ODS
#> XLSX                     XLSX
#> Elasticsearch   Elasticsearch
#> Carto                   Carto
#> AmigoCloud         AmigoCloud
#> SXF                       SXF
#> Selafin               Selafin
#> JML                       JML
#> PLSCENES             PLSCENES
#> CSW                       CSW
#> VDV                       VDV
#> GMLAS                   GMLAS
#> MVT                       MVT
#> NGW                       NGW
#> MapML                   MapML
#> GTFS                     GTFS
#> PMTiles               PMTiles
#> JSONFG                 JSONFG
#> TIGER                   TIGER
#> AVCBin                 AVCBin
#> AVCE00                 AVCE00
#> HTTP                     HTTP
#>                                                                    long_name
#> FITS                                         Flexible Image Transport System
#> PCIDSK                                                  PCIDSK Database File
#> netCDF                                            Network Common Data Format
#> PDS4                                            NASA Planetary Data System 4
#> VICAR                                                        MIPL VICAR file
#> JP2OpenJPEG                       JPEG-2000 driver based on OpenJPEG library
#> PDF                                                           Geospatial PDF
#> MBTiles                                                              MBTiles
#> BAG                                               Bathymetry Attributed Grid
#> EEDA                                                   Earth Engine Data API
#> OGCAPI                                                                OGCAPI
#> ESRI Shapefile                                                ESRI Shapefile
#> MapInfo File                                                    MapInfo File
#> UK .NTF                                                              UK .NTF
#> LVBAG                                            Kadaster LV BAG Extract 2.0
#> OGR_SDTS                                                                SDTS
#> S57                                                           IHO S-57 (ENC)
#> DGN                                                         Microstation DGN
#> OGR_VRT                                             VRT - Virtual Datasource
#> Memory                                                                Memory
#> CSV                                             Comma Separated Value (.csv)
#> NAS                                                              NAS - ALKIS
#> GML                                          Geography Markup Language (GML)
#> GPX                                                                      GPX
#> LIBKML                                      Keyhole Markup Language (LIBKML)
#> KML                                            Keyhole Markup Language (KML)
#> GeoJSON                                                              GeoJSON
#> GeoJSONSeq                                                  GeoJSON Sequence
#> ESRIJSON                                                            ESRIJSON
#> TopoJSON                                                            TopoJSON
#> Interlis 1                                                        Interlis 1
#> Interlis 2                                                        Interlis 2
#> OGR_GMT                                             GMT ASCII Vectors (.gmt)
#> GPKG                                                              GeoPackage
#> SQLite                                                   SQLite / Spatialite
#> ODBC                                                                        
#> WAsP                                                        WAsP .map format
#> PGeo                                               ESRI Personal GeoDatabase
#> MSSQLSpatial                           Microsoft SQL Server Spatial Database
#> OGR_OGDI                                       OGDI Vectors (VPF, VMAP, DCW)
#> PostgreSQL                                                PostgreSQL/PostGIS
#> MySQL                                                                  MySQL
#> OpenFileGDB                                                     ESRI FileGDB
#> DXF                                                              AutoCAD DXF
#> CAD                                                           AutoCAD Driver
#> FlatGeobuf                                                        FlatGeobuf
#> Geoconcept                                                        Geoconcept
#> GeoRSS                                                                GeoRSS
#> VFK                                     Czech Cadastral Exchange Data Format
#> PGDUMP                                                   PostgreSQL SQL dump
#> OSM                                                OpenStreetMap XML and PBF
#> GPSBabel                                                            GPSBabel
#> OGR_PDS                                         Planetary Data Systems TABLE
#> WFS                                            OGC WFS (Web Feature Service)
#> OAPIF                                                     OGC API - Features
#> SOSI                                                 Norwegian SOSI Standard
#> EDIGEO                                         French EDIGEO exchange format
#> SVG                                                 Scalable Vector Graphics
#> Idrisi                                                  Idrisi Vector (.vct)
#> XLS                                                          MS Excel format
#> ODS                     Open Document/ LibreOffice / OpenOffice Spreadsheet 
#> XLSX                                          MS Office Open XML spreadsheet
#> Elasticsearch                                                 Elastic Search
#> Carto                                                                  Carto
#> AmigoCloud                                                        AmigoCloud
#> SXF                                              Storage and eXchange Format
#> Selafin                                                              Selafin
#> JML                                                             OpenJUMP JML
#> PLSCENES                                              Planet Labs Scenes API
#> CSW                                   OGC CSW (Catalog  Service for the Web)
#> VDV                                      VDV-451/VDV-452/INTREST Data Format
#> GMLAS          Geography Markup Language (GML) driven by application schemas
#> MVT                                                      Mapbox Vector Tiles
#> NGW                                                              NextGIS Web
#> MapML                                                                  MapML
#> GTFS                                      General Transit Feed Specification
#> PMTiles                                                       ProtoMap Tiles
#> JSONFG                                      OGC Features and Geometries JSON
#> TIGER                                                 U.S. Census TIGER/Line
#> AVCBin                                              Arc/Info Binary Coverage
#> AVCE00                                         Arc/Info E00 (ASCII) Coverage
#> HTTP                                                   HTTP Fetching Wrapper
#>                write  copy is_raster is_vector   vsi
#> FITS            TRUE FALSE      TRUE      TRUE FALSE
#> PCIDSK          TRUE FALSE      TRUE      TRUE  TRUE
#> netCDF          TRUE  TRUE      TRUE      TRUE  TRUE
#> PDS4            TRUE  TRUE      TRUE      TRUE  TRUE
#> VICAR           TRUE  TRUE      TRUE      TRUE  TRUE
#> JP2OpenJPEG    FALSE  TRUE      TRUE      TRUE  TRUE
#> PDF             TRUE  TRUE      TRUE      TRUE  TRUE
#> MBTiles         TRUE  TRUE      TRUE      TRUE  TRUE
#> BAG             TRUE  TRUE      TRUE      TRUE  TRUE
#> EEDA           FALSE FALSE     FALSE      TRUE FALSE
#> OGCAPI         FALSE FALSE      TRUE      TRUE  TRUE
#> ESRI Shapefile  TRUE FALSE     FALSE      TRUE  TRUE
#> MapInfo File    TRUE FALSE     FALSE      TRUE  TRUE
#> UK .NTF        FALSE FALSE     FALSE      TRUE  TRUE
#> LVBAG          FALSE FALSE     FALSE      TRUE  TRUE
#> OGR_SDTS       FALSE FALSE     FALSE      TRUE  TRUE
#> S57             TRUE FALSE     FALSE      TRUE  TRUE
#> DGN             TRUE FALSE     FALSE      TRUE  TRUE
#> OGR_VRT        FALSE FALSE     FALSE      TRUE  TRUE
#> Memory          TRUE FALSE     FALSE      TRUE FALSE
#> CSV             TRUE FALSE     FALSE      TRUE  TRUE
#> NAS            FALSE FALSE     FALSE      TRUE  TRUE
#> GML             TRUE FALSE     FALSE      TRUE  TRUE
#> GPX             TRUE FALSE     FALSE      TRUE  TRUE
#> LIBKML          TRUE FALSE     FALSE      TRUE  TRUE
#> KML             TRUE FALSE     FALSE      TRUE  TRUE
#> GeoJSON         TRUE FALSE     FALSE      TRUE  TRUE
#> GeoJSONSeq      TRUE FALSE     FALSE      TRUE  TRUE
#> ESRIJSON       FALSE FALSE     FALSE      TRUE  TRUE
#> TopoJSON       FALSE FALSE     FALSE      TRUE  TRUE
#> Interlis 1      TRUE FALSE     FALSE      TRUE  TRUE
#> Interlis 2      TRUE FALSE     FALSE      TRUE  TRUE
#> OGR_GMT         TRUE FALSE     FALSE      TRUE  TRUE
#> GPKG            TRUE  TRUE      TRUE      TRUE  TRUE
#> SQLite          TRUE FALSE     FALSE      TRUE  TRUE
#> ODBC           FALSE FALSE     FALSE      TRUE FALSE
#> WAsP            TRUE FALSE     FALSE      TRUE  TRUE
#> PGeo           FALSE FALSE     FALSE      TRUE FALSE
#> MSSQLSpatial    TRUE FALSE     FALSE      TRUE FALSE
#> OGR_OGDI       FALSE FALSE     FALSE      TRUE FALSE
#> PostgreSQL      TRUE FALSE     FALSE      TRUE FALSE
#> MySQL           TRUE FALSE     FALSE      TRUE FALSE
#> OpenFileGDB     TRUE FALSE      TRUE      TRUE  TRUE
#> DXF             TRUE FALSE     FALSE      TRUE  TRUE
#> CAD            FALSE FALSE      TRUE      TRUE  TRUE
#> FlatGeobuf      TRUE FALSE     FALSE      TRUE  TRUE
#> Geoconcept      TRUE FALSE     FALSE      TRUE  TRUE
#> GeoRSS          TRUE FALSE     FALSE      TRUE  TRUE
#> VFK            FALSE FALSE     FALSE      TRUE FALSE
#> PGDUMP          TRUE FALSE     FALSE      TRUE  TRUE
#> OSM            FALSE FALSE     FALSE      TRUE  TRUE
#> GPSBabel        TRUE FALSE     FALSE      TRUE FALSE
#> OGR_PDS        FALSE FALSE     FALSE      TRUE  TRUE
#> WFS            FALSE FALSE     FALSE      TRUE  TRUE
#> OAPIF          FALSE FALSE     FALSE      TRUE FALSE
#> SOSI           FALSE FALSE     FALSE      TRUE FALSE
#> EDIGEO         FALSE FALSE     FALSE      TRUE  TRUE
#> SVG            FALSE FALSE     FALSE      TRUE  TRUE
#> Idrisi         FALSE FALSE     FALSE      TRUE  TRUE
#> XLS            FALSE FALSE     FALSE      TRUE FALSE
#> ODS             TRUE FALSE     FALSE      TRUE  TRUE
#> XLSX            TRUE FALSE     FALSE      TRUE  TRUE
#> Elasticsearch   TRUE FALSE     FALSE      TRUE FALSE
#> Carto           TRUE FALSE     FALSE      TRUE FALSE
#> AmigoCloud      TRUE FALSE     FALSE      TRUE FALSE
#> SXF            FALSE FALSE     FALSE      TRUE  TRUE
#> Selafin         TRUE FALSE     FALSE      TRUE  TRUE
#> JML             TRUE FALSE     FALSE      TRUE  TRUE
#> PLSCENES       FALSE FALSE      TRUE      TRUE FALSE
#> CSW            FALSE FALSE     FALSE      TRUE FALSE
#> VDV             TRUE FALSE     FALSE      TRUE  TRUE
#> GMLAS          FALSE  TRUE     FALSE      TRUE  TRUE
#> MVT             TRUE FALSE     FALSE      TRUE  TRUE
#> NGW             TRUE  TRUE      TRUE      TRUE FALSE
#> MapML           TRUE FALSE     FALSE      TRUE  TRUE
#> GTFS           FALSE FALSE     FALSE      TRUE  TRUE
#> PMTiles         TRUE FALSE     FALSE      TRUE  TRUE
#> JSONFG          TRUE FALSE     FALSE      TRUE  TRUE
#> TIGER          FALSE FALSE     FALSE      TRUE  TRUE
#> AVCBin         FALSE FALSE     FALSE      TRUE  TRUE
#> AVCE00         FALSE FALSE     FALSE      TRUE  TRUE
#> HTTP           FALSE FALSE      TRUE      TRUE FALSE
st_drivers("raster", "GeoT")
#>        name                         long_name write copy is_raster is_vector
#> GTiff GTiff                           GeoTIFF  TRUE TRUE      TRUE     FALSE
#> COG     COG Cloud optimized GeoTIFF generator FALSE TRUE      TRUE     FALSE
#>        vsi
#> GTiff TRUE
#> COG   TRUE
```
