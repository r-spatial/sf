# Create sf object

Create sf, which extends data.frame-like objects with a simple feature
list column. To convert a data frame object to `sf`, use
[`st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.md)

## Usage

``` r
st_sf(
  ...,
  agr = NA_agr_,
  row.names,
  stringsAsFactors = sf_stringsAsFactors(),
  crs,
  precision,
  sf_column_name = NULL,
  check_ring_dir = FALSE,
  sfc_last = TRUE
)

# S3 method for class 'sf'
x[i, j, ..., drop = FALSE, op = st_intersects]

# S3 method for class 'sf'
print(x, ..., n = getOption("sf_max_print", default = 10))
```

## Arguments

- ...:

  column elements to be binded into an `sf` object or a single `list` or
  `data.frame` with such columns; at least one of these columns shall be
  a geometry list-column of class `sfc` or be a list-column that can be
  converted into an `sfc` by
  [st_as_sfc](https://r-spatial.github.io/sf/reference/st_as_sfc.md).

- agr:

  character vector; see details below.

- row.names:

  row.names for the created `sf` object

- stringsAsFactors:

  logical; see
  [st_read](https://r-spatial.github.io/sf/reference/st_read.md)

- crs:

  coordinate reference system, something suitable as input to
  [st_crs](https://r-spatial.github.io/sf/reference/st_crs.md)

- precision:

  numeric; see
  [st_as_binary](https://r-spatial.github.io/sf/reference/st_as_binary.md)

- sf_column_name:

  character; name of the active list-column with simple feature
  geometries; in case there is more than one and `sf_column_name` is
  `NULL`, the first one is taken.

- check_ring_dir:

  see [st_read](https://r-spatial.github.io/sf/reference/st_read.md)

- sfc_last:

  logical; if `TRUE`, `sfc` columns are always put last, otherwise
  column order is left unmodified.

- x:

  object of class `sf`

- i:

  record selection, see
  [\[.data.frame](https://rdrr.io/r/base/Extract.data.frame.html), or a
  `sf` object to work with the `op` argument

- j:

  variable selection, see
  [\[.data.frame](https://rdrr.io/r/base/Extract.data.frame.html)

- drop:

  logical, default `FALSE`; if `TRUE` drop the geometry column and
  return a `data.frame`, else make the geometry sticky and return a `sf`
  object.

- op:

  function; geometrical binary predicate function to apply when `i` is a
  simple feature object

- n:

  maximum number of features to print; can be set globally by
  `options(sf_max_print=...)`

## Details

`agr`, attribute-geometry-relationship, specifies for each non-geometry
attribute column how it relates to the geometry, and can have one of
following values: "constant", "aggregate", "identity". "constant" is
used for attributes that are constant throughout the geometry (e.g. land
use), "aggregate" where the attribute is an aggregate value over the
geometry (e.g. population density or population count), "identity" when
the attributes uniquely identifies the geometry of particular "thing",
such as a building ID or a city name. The default value, `NA_agr_`,
implies we don't know.

When a single value is provided to `agr`, it is cascaded across all
input columns; otherwise, a named vector like
`c(feature1='constant', ...)` will set `agr` value to `'constant'` for
the input column named `feature1`. See `demo(nc)` for a worked example
of this.

When confronted with a data.frame-like object, `st_sf` will try to find
a geometry column of class `sfc`, and otherwise try to convert
list-columns when available into a geometry column, using
[st_as_sfc](https://r-spatial.github.io/sf/reference/st_as_sfc.md).

`[.sf` will return a `data.frame` or vector if the geometry column (of
class `sfc`) is dropped (`drop=TRUE`), an `sfc` object if only the
geometry column is selected, and otherwise return an `sf` object; see
also [\[.data.frame](https://rdrr.io/r/base/Extract.data.frame.html);
for `[.sf` `...` arguments are passed to `op`.

## Examples

``` r
g = st_sfc(st_point(1:2))
st_sf(a=3,g)
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#>   a           g
#> 1 3 POINT (1 2)
st_sf(g, a=3)
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#>   a           g
#> 1 3 POINT (1 2)
st_sf(a=3, st_sfc(st_point(1:2))) # better to name it!
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#>   a st_sfc.st_point.1.2..
#> 1 3           POINT (1 2)
# create empty structure with preallocated empty geometries:
nrows <- 10
geometry = st_sfc(lapply(1:nrows, function(x) st_geometrycollection()))
df <- st_sf(id = 1:nrows, geometry = geometry)
g = st_sfc(st_point(1:2), st_point(3:4))
s = st_sf(a=3:4, g)
s[1,]
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#>   a           g
#> 1 3 POINT (1 2)
class(s[1,])
#> [1] "sf"         "data.frame"
s[,1]
#> Simple feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 3 ymax: 4
#> CRS:           NA
#>   a           g
#> 1 3 POINT (1 2)
#> 2 4 POINT (3 4)
class(s[,1])
#> [1] "sf"         "data.frame"
s[,2]
#> Simple feature collection with 2 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 3 ymax: 4
#> CRS:           NA
#>             g
#> 1 POINT (1 2)
#> 2 POINT (3 4)
class(s[,2])
#> [1] "sf"         "data.frame"
g = st_sf(a=2:3, g)
pol = st_sfc(st_polygon(list(cbind(c(0,3,3,0,0),c(0,0,3,3,0)))))
h = st_sf(r = 5, pol)
g[h,]
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#>   a           g
#> 1 2 POINT (1 2)
h[g,]
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 3 ymax: 3
#> CRS:           NA
#>   r                            pol
#> 1 5 POLYGON ((0 0, 3 0, 3 3, 0 ...
```
