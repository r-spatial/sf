# North Carolina SIDS data

Sudden Infant Death Syndrome (SIDS) sample data for North Carolina
counties, two time periods (1974-78 and 1979-84). The details of the
columns can be found in a [spdep package
vignette](https://r-spatial.github.io/spdep/articles/sids.html). Please
note that, though this is basically the same as `nc.sids` dataset in
spData package, `nc` only contains a subset of variables. The
differences are also discussed on the vignette.

## Format

A `sf` object

## See also

<https://r-spatial.github.io/spdep/articles/sids.html>

## Examples

``` r
# \donttest{
nc <- st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
# }
```
