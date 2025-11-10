# Create viewport from sf, sfc or sfg object

Create viewport from sf, sfc or sfg object

## Usage

``` r
st_viewport(x, ..., bbox = st_bbox(x), asp)
```

## Arguments

- x:

  object of class sf, sfc or sfg object

- ...:

  parameters passed on to
  [viewport](https://rdrr.io/r/grid/viewport.html)

- bbox:

  the bounding box used for aspect ratio

- asp:

  numeric; target aspect ratio (y/x), see Details

## Value

The output of the call to
[viewport](https://rdrr.io/r/grid/viewport.html)

## Details

parameters `width`, `height`, `xscale` and `yscale` are set such that
aspect ratio is honoured and plot size is maximized in the current
viewport; others can be passed as `...`

If `asp` is missing, it is taken as 1, except when
`isTRUE(st_is_longlat(x))`, in which case it is set to `1.0 /cos(y)`,
with `y` the middle of the latitude bounding box.

## Examples

``` r
library(grid)
#> 
#> Attaching package: ‘grid’
#> The following object is masked from ‘package:spatstat.geom’:
#> 
#>     as.mask
nc = st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
grid.newpage()
pushViewport(viewport(width = 0.8, height = 0.8))
pushViewport(st_viewport(nc))
invisible(lapply(st_geometry(nc), function(x) grid.draw(st_as_grob(x, gp = gpar(fill = 'red')))))
```
