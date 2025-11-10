# Methods to coerce simple features to `Spatial*` and `Spatial*DataFrame` objects

`as_Spatial()` allows to convert `sf` and `sfc` to `Spatial*DataFrame`
and `Spatial*` for `sp` compatibility. You can also use
`as(x, "Spatial")` To transform `sp` objects to `sf` and `sfc` with
`as(x, "sf")`.

## Usage

``` r
as_Spatial(from, cast = TRUE, IDs = paste0("ID", seq_along(from)))
```

## Arguments

- from:

  object of class `sf`, `sfc_POINT`, `sfc_MULTIPOINT`, `sfc_LINESTRING`,
  `sfc_MULTILINESTRING`, `sfc_POLYGON`, or `sfc_MULTIPOLYGON`.

- cast:

  logical; if `TRUE`,
  [`st_cast()`](https://r-spatial.github.io/sf/reference/st_cast.md)
  `from` before converting, so that e.g. `GEOMETRY` objects with a mix
  of `POLYGON` and `MULTIPOLYGON` are cast to `MULTIPOLYGON`.

- IDs:

  character vector with IDs for the `Spatial*` geometries

## Value

geometry-only object deriving from `Spatial`, of the appropriate class

## Details

Package `sp` supports three dimensions for `POINT` and `MULTIPOINT`
(`SpatialPoint*`). Other geometries must be two-dimensional (`XY`).
Dimensions can be dropped using
[`st_zm()`](https://r-spatial.github.io/sf/reference/st_zm.md) with
`what = "M"` or `what = "ZM"`.

For converting simple features (i.e., `sf` objects) to their `Spatial`
counterpart, use `as(obj, "Spatial")`

## Examples

``` r
nc <- st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
if (require(sp, quietly = TRUE)) {
# convert to SpatialPolygonsDataFrame
spdf <- as_Spatial(nc)
# identical to
spdf <- as(nc, "Spatial")
# convert to SpatialPolygons
as(st_geometry(nc), "Spatial")
# back to sf
as(spdf, "sf")
}
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
#> First 10 features:
#>     AREA PERIMETER CNTY_ CNTY_ID        NAME  FIPS FIPSNO CRESS_ID BIR74 SID74
#> 1  0.114     1.442  1825    1825        Ashe 37009  37009        5  1091     1
#> 2  0.061     1.231  1827    1827   Alleghany 37005  37005        3   487     0
#> 3  0.143     1.630  1828    1828       Surry 37171  37171       86  3188     5
#> 4  0.070     2.968  1831    1831   Currituck 37053  37053       27   508     1
#> 5  0.153     2.206  1832    1832 Northampton 37131  37131       66  1421     9
#> 6  0.097     1.670  1833    1833    Hertford 37091  37091       46  1452     7
#> 7  0.062     1.547  1834    1834      Camden 37029  37029       15   286     0
#> 8  0.091     1.284  1835    1835       Gates 37073  37073       37   420     0
#> 9  0.118     1.421  1836    1836      Warren 37185  37185       93   968     4
#> 10 0.124     1.428  1837    1837      Stokes 37169  37169       85  1612     1
#>    NWBIR74 BIR79 SID79 NWBIR79                       geometry
#> 1       10  1364     0      19 MULTIPOLYGON (((-81.47276 3...
#> 2       10   542     3      12 MULTIPOLYGON (((-81.23989 3...
#> 3      208  3616     6     260 MULTIPOLYGON (((-80.45634 3...
#> 4      123   830     2     145 MULTIPOLYGON (((-76.00897 3...
#> 5     1066  1606     3    1197 MULTIPOLYGON (((-77.21767 3...
#> 6      954  1838     5    1237 MULTIPOLYGON (((-76.74506 3...
#> 7      115   350     2     139 MULTIPOLYGON (((-76.00897 3...
#> 8      254   594     2     371 MULTIPOLYGON (((-76.56251 3...
#> 9      748  1190     2     844 MULTIPOLYGON (((-78.30876 3...
#> 10     160  2038     5     176 MULTIPOLYGON (((-80.02567 3...
```
