# Get, set, replace or rename geometry from an sf object

Get, set, replace or rename geometry from an sf object

## Usage

``` r
# S3 method for class 'sfc'
st_geometry(obj, ...)

st_geometry(obj, ...)

# S3 method for class 'sf'
st_geometry(obj, ...)

# S3 method for class 'sfc'
st_geometry(obj, ...)

# S3 method for class 'sfg'
st_geometry(obj, ...)

st_geometry(x) <- value

st_set_geometry(x, value)

st_drop_geometry(x, ...)

# S3 method for class 'sf'
st_drop_geometry(x, ...)

# Default S3 method
st_drop_geometry(x, ...)
```

## Arguments

- obj:

  object of class `sf` or `sfc`

- ...:

  ignored

- x:

  object of class `data.frame` or `sf`

- value:

  object of class `sfc`, or `character` to set, replace, or rename the
  geometry of `x`

## Value

st_geometry returns an object of class
[sfc](https://r-spatial.github.io/sf/reference/sfc.md), a list-column
with geometries

`st_geometry` returns an object of class
[sfc](https://r-spatial.github.io/sf/reference/sfc.md). Assigning
geometry to a `data.frame` creates an
[sf](https://r-spatial.github.io/sf/reference/sf.md) object, assigning
it to an [sf](https://r-spatial.github.io/sf/reference/sf.md) object
replaces the geometry list-column.

## Details

when applied to a `data.frame` and when `value` is an object of class
`sfc`, `st_set_geometry` and `st_geometry<-` will first check for the
existence of an attribute `sf_column` and overwrite that, or else look
for list-columns of class `sfc` and overwrite the first of that, or else
write the geometry list-column to a column named `geometry`. In case
`value` is character and `x` is of class `sf`, the "active" geometry
column is set to `x[[value]]`.

the replacement function applied to `sf` objects will overwrite the
geometry list-column, if `value` is `NULL`, it will remove it and coerce
`x` to a `data.frame`.

if `x` is of class `sf`, `st_drop_geometry` drops the geometry of its
argument, and reclasses it accordingly; otherwise it returns `x`
unmodified.

## Examples

``` r
df = data.frame(a = 1:2)
sfc = st_sfc(st_point(c(3,4)), st_point(c(10,11)))
st_geometry(sfc)
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 3 ymin: 4 xmax: 10 ymax: 11
#> CRS:           NA
#> POINT (3 4)
#> POINT (10 11)
st_geometry(df) <- sfc
class(df)
#> [1] "sf"         "data.frame"
st_geometry(df)
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 3 ymin: 4 xmax: 10 ymax: 11
#> CRS:           NA
#> POINT (3 4)
#> POINT (10 11)
st_geometry(df) <- sfc # replaces
st_geometry(df) <- NULL # remove geometry, coerce to data.frame
sf <- st_set_geometry(df, sfc) # set geometry, return sf
st_set_geometry(sf, NULL) # remove geometry, coerce to data.frame
#>   a
#> 1 1
#> 2 2
```
