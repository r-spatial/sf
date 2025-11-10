# Bind rows (features) of sf objects

Bind rows (features) of sf objects

Bind columns (variables) of sf objects

## Usage

``` r
# S3 method for class 'sf'
rbind(..., deparse.level = 1)

# S3 method for class 'sf'
cbind(..., deparse.level = 1, sf_column_name = NULL)

st_bind_cols(...)
```

## Arguments

- ...:

  objects to bind; note that for the rbind and cbind methods, all
  objects have to be of class `sf`; see dotsMethods

- deparse.level:

  integer; see [rbind](https://rdrr.io/r/base/cbind.html)

- sf_column_name:

  character; specifies active geometry; passed on to
  [st_sf](https://r-spatial.github.io/sf/reference/sf.md)

## Value

`cbind` called with multiple `sf` objects warns about multiple geometry
columns present when the geometry column to use is not specified by
using argument `sf_column_name`; see also
[st_sf](https://r-spatial.github.io/sf/reference/sf.md).

## Details

both `rbind` and `cbind` have non-standard method dispatch (see
[cbind](https://rdrr.io/r/base/cbind.html)): the `rbind` or `cbind`
method for `sf` objects is only called when all arguments to be binded
are of class `sf`.

If you need to `cbind` e.g. a `data.frame` to an `sf`, use
[data.frame](https://rdrr.io/r/base/data.frame.html) directly and use
[st_sf](https://r-spatial.github.io/sf/reference/sf.md) on its result,
or use
[bind_cols](https://dplyr.tidyverse.org/reference/bind_rows.html); see
examples.

`st_bind_cols` is deprecated; use `cbind` instead.

## Examples

``` r
crs = st_crs(3857)
a = st_sf(a=1, geom = st_sfc(st_point(0:1)), crs = crs)
b = st_sf(a=1, geom = st_sfc(st_linestring(matrix(1:4,2))), crs = crs)
c = st_sf(a=4, geom = st_sfc(st_multilinestring(list(matrix(1:4,2)))), crs = crs)
rbind(a,b,c)
#> Simple feature collection with 3 features and 1 field
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 1 xmax: 2 ymax: 4
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   a                         geom
#> 1 1                  POINT (0 1)
#> 2 1        LINESTRING (1 3, 2 4)
#> 3 4 MULTILINESTRING ((1 3, 2 4))
rbind(a,b)
#> Simple feature collection with 2 features and 1 field
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 1 xmax: 2 ymax: 4
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   a                  geom
#> 1 1           POINT (0 1)
#> 2 1 LINESTRING (1 3, 2 4)
rbind(a,b)
#> Simple feature collection with 2 features and 1 field
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 1 xmax: 2 ymax: 4
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   a                  geom
#> 1 1           POINT (0 1)
#> 2 1 LINESTRING (1 3, 2 4)
rbind(b,c)
#> Simple feature collection with 2 features and 1 field
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 3 xmax: 2 ymax: 4
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   a                         geom
#> 1 1        LINESTRING (1 3, 2 4)
#> 2 4 MULTILINESTRING ((1 3, 2 4))
cbind(a,b,c) # warns
#> Simple feature collection with 1 feature and 3 fields
#> Active geometry column: geom
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 1 xmax: 0 ymax: 1
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   a a.1 a.2        geom                geom.1                       geom.2
#> 1 1   1   4 POINT (0 1) LINESTRING (1 3, 2 4) MULTILINESTRING ((1 3, 2 4))
if (require(dplyr, quietly = TRUE))
  dplyr::bind_cols(a,b)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#> New names:
#> • `a` -> `a...1`
#> • `geom` -> `geom...2`
#> • `a` -> `a...3`
#> • `geom` -> `geom...4`
#>   a...1    geom...2 a...3              geom...4
#> 1     1 POINT (0 1)     1 LINESTRING (1 3, 2 4)
c = st_sf(a=4, geomc = st_sfc(st_multilinestring(list(matrix(1:4,2)))), crs = crs)
cbind(a,b,c, sf_column_name = "geomc")
#> Simple feature collection with 1 feature and 3 fields
#> Active geometry column: geomc
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 3 xmax: 2 ymax: 4
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   a a.1 a.2        geom                geom.1                        geomc
#> 1 1   1   4 POINT (0 1) LINESTRING (1 3, 2 4) MULTILINESTRING ((1 3, 2 4))
df = data.frame(x=3)
st_sf(data.frame(c, df))
#> Simple feature collection with 1 feature and 2 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 3 xmax: 2 ymax: 4
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   a x                        geomc
#> 1 4 3 MULTILINESTRING ((1 3, 2 4))
if (require(dplyr, quietly = TRUE))
  dplyr::bind_cols(c, df)
#> Simple feature collection with 1 feature and 2 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 3 xmax: 2 ymax: 4
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   a x                        geomc
#> 1 4 3 MULTILINESTRING ((1 3, 2 4))
```
