# Cast geometry to another type: either simplify, or cast explicitly

Cast geometry to another type: either simplify, or cast explicitly

## Usage

``` r
# S3 method for class 'MULTIPOLYGON'
st_cast(x, to, ...)

# S3 method for class 'MULTILINESTRING'
st_cast(x, to, ...)

# S3 method for class 'MULTIPOINT'
st_cast(x, to, ...)

# S3 method for class 'POLYGON'
st_cast(x, to, ...)

# S3 method for class 'LINESTRING'
st_cast(x, to, ...)

# S3 method for class 'POINT'
st_cast(x, to, ...)

# S3 method for class 'GEOMETRYCOLLECTION'
st_cast(x, to, ...)

# S3 method for class 'CIRCULARSTRING'
st_cast(x, to, ...)

# S3 method for class 'MULTISURFACE'
st_cast(x, to, ...)

# S3 method for class 'COMPOUNDCURVE'
st_cast(x, to, ...)

# S3 method for class 'MULTICURVE'
st_cast(x, to, ...)

# S3 method for class 'CURVE'
st_cast(x, to, ...)

st_cast(x, to, ...)

# S3 method for class 'sfc'
st_cast(x, to, ..., ids = seq_along(x), group_or_split = TRUE)

# S3 method for class 'sf'
st_cast(x, to, ..., warn = TRUE, do_split = TRUE)

# S3 method for class 'sfc_CIRCULARSTRING'
st_cast(x, to, ...)
```

## Arguments

- x:

  object of class `sfg`, `sfc` or `sf`

- to:

  character; target type, if missing, simplification is tried; when `x`
  is of type `sfg` (i.e., a single geometry) then `to` needs to be
  specified.

- ...:

  ignored

- ids:

  integer vector, denoting how geometries should be grouped (default: no
  grouping)

- group_or_split:

  logical; if TRUE, group or split geometries; if FALSE, carry out a 1-1
  per-geometry conversion.

- warn:

  logical; if `TRUE`, warn if attributes are assigned to sub-geometries

- do_split:

  logical; if `TRUE`, allow splitting of geometries in sub-geometries

## Value

object of class `to` if successful, or unmodified object if
unsuccessful. If information gets lost while type casting, a warning is
raised.

In case `to` is missing, `st_cast.sfc` will coerce combinations of
"POINT" and "MULTIPOINT", "LINESTRING" and "MULTILINESTRING", "POLYGON"
and "MULTIPOLYGON" into their "MULTI..." form, or in case all geometries
are "GEOMETRYCOLLECTION" will return a list of all the contents of the
"GEOMETRYCOLLECTION" objects, or else do nothing. In case `to` is
specified, if `to` is "GEOMETRY", geometries are not converted, else,
`st_cast` will try to coerce all elements into `to`; `ids` may be
specified to group e.g. "POINT" objects into a "MULTIPOINT", if not
specified no grouping takes place. If e.g. a "sfc_MULTIPOINT" is cast to
a "sfc_POINT", the objects are split, so no information gets lost,
unless `group_or_split` is `FALSE`.

## Details

When converting a GEOMETRYCOLLECTION to COMPOUNDCURVE, MULTISURFACE or
CURVEPOLYGON, the user is responsible for the validity of the resulting
object: no checks are being carried out by the software.

When converting mixed, GEOMETRY sets, it may help to first convert to
the MULTI-type, see examples

the `st_cast` method for `sf` objects can only split geometries, e.g.
cast `MULTIPOINT` into multiple `POINT` features. In case of splitting,
attributes are repeated and a warning is issued when non-constant
attributes are assigned to sub-geometries. To merge feature geometries
and attribute values, use
[aggregate](https://r-spatial.github.io/sf/reference/aggregate.sf.md) or
[summarise](https://r-spatial.github.io/sf/reference/tidyverse.md).

## Examples

``` r
# example(st_read)
nc = st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
mpl <- st_geometry(nc)[[4]]
#st_cast(x) ## error 'argument "to" is missing, with no default'
cast_all <- function(xg) {
  lapply(c("MULTIPOLYGON", "MULTILINESTRING", "MULTIPOINT", "POLYGON", "LINESTRING", "POINT"), 
      function(x) st_cast(xg, x))
}
st_sfc(cast_all(mpl))
#> Warning: polygon from first part only
#> Warning: line from first ring only
#> Warning: point from first coordinate only
#> Geometry set for 6 features 
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -76.33025 ymin: 36.07282 xmax: -75.77316 ymax: 36.55716
#> CRS:           NA
#> First 5 geometries:
#> MULTIPOLYGON (((-76.00897 36.3196, -76.01735 36...
#> MULTILINESTRING ((-76.00897 36.3196, -76.01735 ...
#> MULTIPOINT ((-76.00897 36.3196), (-76.01735 36....
#> POLYGON ((-76.00897 36.3196, -76.01735 36.33773...
#> LINESTRING (-76.00897 36.3196, -76.01735 36.337...
## no closing coordinates should remain for multipoint
any(duplicated(unclass(st_cast(mpl, "MULTIPOINT"))))  ## should be FALSE
#> [1] FALSE
## number of duplicated coordinates in the linestrings should equal the number of polygon rings 
## (... in this case, won't always be true)
sum(duplicated(do.call(rbind, unclass(st_cast(mpl, "MULTILINESTRING"))))
     ) == sum(unlist(lapply(mpl, length)))  ## should be TRUE
#> [1] TRUE

p1 <- structure(c(0, 1, 3, 2, 1, 0, 0, 0, 2, 4, 4, 0), .Dim = c(6L, 2L))
p2 <- structure(c(1, 1, 2, 1, 1, 2, 2, 1), .Dim = c(4L, 2L))
st_polygon(list(p1, p2))
#> POLYGON ((0 0, 1 0, 3 2, 2 4, 1 4, 0 0), (1 1, 1 2, 2 2, 1 1))
mls <- st_cast(st_geometry(nc)[[4]], "MULTILINESTRING")
st_sfc(cast_all(mls))
#> Warning: keeping first linestring only
#> Warning: keeping first coordinate only
#> Geometry set for 6 features 
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -76.33025 ymin: 36.07282 xmax: -75.77316 ymax: 36.55716
#> CRS:           NA
#> First 5 geometries:
#> MULTIPOLYGON (((-76.00897 36.3196, -76.01735 36...
#> MULTILINESTRING ((-76.00897 36.3196, -76.01735 ...
#> MULTIPOINT ((-76.00897 36.3196), (-76.01735 36....
#> POLYGON ((-76.00897 36.3196, -76.01735 36.33773...
#> LINESTRING (-76.00897 36.3196, -76.01735 36.337...
mpt <- st_cast(st_geometry(nc)[[4]], "MULTIPOINT")
st_sfc(cast_all(mpt))
#> Warning: point from first coordinate only
#> Geometry set for 6 features 
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -76.33025 ymin: 36.07282 xmax: -75.77316 ymax: 36.55716
#> CRS:           NA
#> First 5 geometries:
#> MULTIPOLYGON (((-76.00897 36.3196, -76.01735 36...
#> MULTILINESTRING ((-76.00897 36.3196, -76.01735 ...
#> MULTIPOINT ((-76.00897 36.3196), (-76.01735 36....
#> POLYGON ((-76.00897 36.3196, -76.01735 36.33773...
#> LINESTRING (-76.00897 36.3196, -76.01735 36.337...
pl <- st_cast(st_geometry(nc)[[4]], "POLYGON")
#> Warning: polygon from first part only
st_sfc(cast_all(pl))
#> Warning: point from first coordinate only
#> Geometry set for 6 features 
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -76.33025 ymin: 36.07282 xmax: -75.79885 ymax: 36.55716
#> CRS:           NA
#> First 5 geometries:
#> MULTIPOLYGON (((-76.00897 36.3196, -76.01735 36...
#> MULTILINESTRING ((-76.00897 36.3196, -76.01735 ...
#> MULTIPOINT ((-76.00897 36.3196), (-76.01735 36....
#> POLYGON ((-76.00897 36.3196, -76.01735 36.33773...
#> LINESTRING (-76.00897 36.3196, -76.01735 36.337...
ls <- st_cast(st_geometry(nc)[[4]], "LINESTRING")
#> Warning: line from first ring only
st_sfc(cast_all(ls))
#> Warning: point from first coordinate only
#> Geometry set for 6 features 
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -76.33025 ymin: 36.07282 xmax: -75.79885 ymax: 36.55716
#> CRS:           NA
#> First 5 geometries:
#> MULTIPOLYGON (((-76.00897 36.3196, -76.01735 36...
#> MULTILINESTRING ((-76.00897 36.3196, -76.01735 ...
#> MULTIPOINT ((-76.00897 36.3196), (-76.01735 36....
#> POLYGON ((-76.00897 36.3196, -76.01735 36.33773...
#> LINESTRING (-76.00897 36.3196, -76.01735 36.337...
pt <- st_cast(st_geometry(nc)[[4]], "POINT")
#> Warning: point from first coordinate only
## st_sfc(cast_all(pt))  ## Error: cannot create MULTIPOLYGON from POINT 
st_sfc(lapply(c("POINT", "MULTIPOINT"), function(x) st_cast(pt, x)))
#> Geometry set for 2 features 
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -76.00897 ymin: 36.3196 xmax: -76.00897 ymax: 36.3196
#> CRS:           NA
#> POINT (-76.00897 36.3196)
#> MULTIPOINT ((-76.00897 36.3196))
s = st_multipoint(rbind(c(1,0)))
st_cast(s, "POINT")
#> Warning: point from first coordinate only
#> POINT (1 0)
# https://github.com/r-spatial/sf/issues/1930:
pt1 <- st_point(c(0,1))
pt23 <- st_multipoint(matrix(c(1,2,3,4), ncol = 2, byrow = TRUE))
d <- st_sf(geom = st_sfc(pt1, pt23))
st_cast(d, "POINT") # will not convert the entire MULTIPOINT, and warns
#> Warning: point from first coordinate only
#> Simple feature collection with 2 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 1 xmax: 1 ymax: 2
#> CRS:           NA
#>          geom
#> 1 POINT (0 1)
#> 2 POINT (1 2)
st_cast(d, "MULTIPOINT") %>% st_cast("POINT")
#> Simple feature collection with 3 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 1 xmax: 3 ymax: 4
#> CRS:           NA
#>          geom
#> 1 POINT (0 1)
#> 2 POINT (1 2)
#> 3 POINT (3 4)
```
