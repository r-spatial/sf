# Drop or add Z and/or M dimensions from feature geometries

Drop Z and/or M dimensions from feature geometries, resetting classes
appropriately

## Usage

``` r
st_zm(x, ..., drop = TRUE, what = "ZM")
```

## Arguments

- x:

  object of class `sfg`, `sfc` or `sf`

- ...:

  ignored

- drop:

  logical; drop, or (`FALSE`) add?

- what:

  character which dimensions to drop or add

## Details

Only combinations `drop=TRUE`, `what = "ZM"`, and `drop=FALSE`,
`what="Z"` are supported so far. In the latter case, `x` should have
`XY` geometry, and zero values are added for the `Z` dimension.

## Examples

``` r
st_zm(st_linestring(matrix(1:32,8)))
#> LINESTRING (1 9, 2 10, 3 11, 4 12, 5 13, 6 14, 7 15, 8 16)
x = st_sfc(st_linestring(matrix(1:32,8)), st_linestring(matrix(1:8,2)))
st_zm(x)
#> Geometry set for 2 features 
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 3 xmax: 8 ymax: 16
#> CRS:           NA
#> LINESTRING (1 9, 2 10, 3 11, 4 12, 5 13, 6 14, ...
#> LINESTRING (1 3, 2 4)
a = st_sf(a = 1:2, geom=x)
st_zm(a)
#> Simple feature collection with 2 features and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 3 xmax: 8 ymax: 16
#> CRS:           NA
#>   a                           geom
#> 1 1 LINESTRING (1 9, 2 10, 3 11...
#> 2 2          LINESTRING (1 3, 2 4)
```
