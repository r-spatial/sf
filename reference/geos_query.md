# Dimension, simplicity, validity or is_empty queries on simple feature geometries

Dimension, simplicity, validity or is_empty queries on simple feature
geometries

## Usage

``` r
st_dimension(x, NA_if_empty = TRUE)

st_is_simple(x)

st_is_empty(x)
```

## Arguments

- x:

  object of class `sf`, `sfc` or `sfg`

- NA_if_empty:

  logical; if TRUE, return NA for empty geometries

## Value

st_dimension returns a numeric vector with 0 for points, 1 for lines, 2
for surfaces, and, if `NA_if_empty` is `TRUE`, `NA` for empty
geometries.

st_is_simple returns a logical vector, indicating for each geometry
whether it is simple (e.g., not self-intersecting)

st_is_empty returns for each geometry whether it is empty

## Examples

``` r
x = st_sfc(
  st_point(0:1),
  st_linestring(rbind(c(0,0),c(1,1))),
  st_polygon(list(rbind(c(0,0),c(1,0),c(0,1),c(0,0)))),
  st_multipoint(),
  st_linestring(),
  st_geometrycollection())
st_dimension(x)
#> [1]  0  1  2 NA NA NA
st_dimension(x, FALSE)
#> [1] 0 1 2 0 1 0
ls = st_linestring(rbind(c(0,0), c(1,1), c(1,0), c(0,1)))
st_is_simple(st_sfc(ls, st_point(c(0,0))))
#> [1] FALSE  TRUE
ls = st_linestring(rbind(c(0,0), c(1,1), c(1,0), c(0,1)))
st_is_empty(st_sfc(ls, st_point(), st_linestring()))
#> [1] FALSE  TRUE  TRUE
```
