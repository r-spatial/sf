# 6. Miscellaneous

This vignette describes a number of issues that did not come up in the
previous vignettes, and that may or may not be categorized as
“frequently asked questions”. Readers are encouraged to provide entries
for this vignette (as for the others).

## What is this EPSG code all about?

EPSG stands for a maintained, well-understood registry of spatial
reference systems, maintained by the International Association of Oil &
Gas Producers (IOGP). `EPSG` stands for the authority, e.g. `EPSG:4326`
stands for spatial reference system with ID 4326 as it is maintained by
the EPSG authority. The website for the EPSG registry is found at the
epsg.org domain. Using `4326` instead of `EPSG:4326` is allowed (`EPSG`
is the default authority) but the latter form, `EPSG:4326` is better
(less ambiguous).

## Why should we use `OGC:CRS84` instead of `EPSG:4326`?

EPSG:4326 formally defines coordinate axes to be in the order
latitude-longitude, but practically all data sources and software
environments use longitude-latitude axis order. OGC:CRS84 is equivalent
to EPSG:4326 except that it defines coordinate axis order
longitude-latitude, removing this ambiguity so to speak. See also
[`st_axis_order()`](https://r-spatial.github.io/sf/reference/st_crs.md)

## How does `sf` deal with secondary geometry columns?

`sf` objects can have more than one geometry list-column, but always
only one geometry column is considered *active*, and returned by
[`st_geometry()`](https://r-spatial.github.io/sf/reference/st_geometry.md).
When there are multiple geometry columns, the default `print` methods
reports which one is active:

``` r
library(sf)
## Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
demo(nc, ask = FALSE, echo = FALSE)
nc$geom2 = st_centroid(st_geometry(nc))
print(nc, n = 2)
## Simple feature collection with 100 features and 14 fields
## Attribute-geometry relationships: aggregate (8), identity (6), NA's (1)
## Active geometry column: geom
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
## Geodetic CRS:  NAD27
## First 2 features:
##    AREA PERIMETER CNTY_ CNTY_ID      NAME  FIPS FIPSNO CRESS_ID BIR74 SID74
## 1 0.114     1.442  1825    1825      Ashe 37009  37009        5  1091     1
## 2 0.061     1.231  1827    1827 Alleghany 37005  37005        3   487     0
##   NWBIR74 BIR79 SID79 NWBIR79                           geom
## 1      10  1364     0      19 MULTIPOLYGON (((-81.47276 3...
## 2      10   542     3      12 MULTIPOLYGON (((-81.23989 3...
##                        geom2
## 1  POINT (-81.49823 36.4314)
## 2 POINT (-81.12513 36.49111)
```

We can switch the active geometry by using `st_geometry<-` or
[`st_set_geometry()`](https://r-spatial.github.io/sf/reference/st_geometry.md),
as in

``` r
plot(st_geometry(nc))
```

![](sf6_files/figure-html/unnamed-chunk-3-1.png)

``` r
st_geometry(nc) <- "geom2"
plot(st_geometry(nc))
```

![](sf6_files/figure-html/unnamed-chunk-3-2.png)

## Does `st_simplify` preserve topology?

[`st_simplify()`](https://r-spatial.github.io/sf/reference/geos_unary.md)
is a topology-preserving function, but does this on the level of
individual feature geometries. That means, simply said, that after
applying it, a polygon will still be a polygon. However when two
features have a longer shared boundary, applying `st_simplify` to the
object does not guarantee that in the resulting object these two
polygons still have the same boundary in common, since the
simplification is done independently, *for each feature geometry*.

## Why do my dplyr verbs not work for `sf` objects?

They do! However, many developers like to write scripts that never load
packages but address all functions by the `sf::` prefix, as in

``` r
i = sf::st_intersects(sf1, sf2)
```

This works up to the moment that a `dplyr` generic like `select` for an
`sf` object is needed: should one call
[`dplyr::select`](https://dplyr.tidyverse.org/reference/select.html)
(won’t know it should search in package `sf`) or
[`sf::select`](https://dplyr.tidyverse.org/reference/select.html) (which
doesn’t exist)? Neither works. One should in this case simply load `sf`,
e.g. by

``` r
library(sf)
```
