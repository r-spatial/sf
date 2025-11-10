# Given an object with geometries of type `GEOMETRY` or `GEOMETRYCOLLECTION`, return an object consisting only of elements of the specified type.

Similar to ST_CollectionExtract in PostGIS. If there are no
sub-geometries of the specified type, an empty geometry is returned.

## Usage

``` r
st_collection_extract(
  x,
  type = c("POLYGON", "POINT", "LINESTRING"),
  warn = FALSE
)

# S3 method for class 'sfg'
st_collection_extract(
  x,
  type = c("POLYGON", "POINT", "LINESTRING"),
  warn = FALSE
)

# S3 method for class 'sfc'
st_collection_extract(
  x,
  type = c("POLYGON", "POINT", "LINESTRING"),
  warn = FALSE
)

# S3 method for class 'sf'
st_collection_extract(
  x,
  type = c("POLYGON", "POINT", "LINESTRING"),
  warn = FALSE
)
```

## Arguments

- x:

  an object of class `sf`, `sfc` or `sfg` that has mixed geometry
  (`GEOMETRY` or `GEOMETRYCOLLECTION`).

- type:

  character; one of "POLYGON", "POINT", "LINESTRING"

- warn:

  logical; if `TRUE`, warn if attributes are assigned to sub-geometries
  when casting (see
  [`st_cast`](https://r-spatial.github.io/sf/reference/st_cast.md))

## Value

An object having the same class as `x`, with geometries consisting only
of elements of the specified type. For `sfg` objects, an `sfg` object is
returned if there is only one geometry of the specified type, otherwise
the geometries are combined into an `sfc` object of the relevant type.
If any subgeometries in the input are MULTI, then all of the
subgeometries in the output will be MULTI.

## Examples

``` r
pt <- st_point(c(1, 0))
ls <- st_linestring(matrix(c(4, 3, 0, 0), ncol = 2))
poly1 <- st_polygon(list(matrix(c(5.5, 7, 7, 6, 5.5, 0, 0, -0.5, -0.5, 0), ncol = 2)))
poly2 <- st_polygon(list(matrix(c(6.6, 8, 8, 7, 6.6, 1, 1, 1.5, 1.5, 1), ncol = 2)))
multipoly <- st_multipolygon(list(poly1, poly2))

i <- st_geometrycollection(list(pt, ls, poly1, poly2))
j <- st_geometrycollection(list(pt, ls, poly1, poly2, multipoly))

st_collection_extract(i, "POLYGON")
#> Geometry set for 2 features 
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 5.5 ymin: -0.5 xmax: 8 ymax: 1.5
#> CRS:           NA
#> POLYGON ((5.5 0, 7 0, 7 -0.5, 6 -0.5, 5.5 0))
#> POLYGON ((6.6 1, 8 1, 8 1.5, 7 1.5, 6.6 1))
st_collection_extract(i, "POINT")
#> POINT (1 0)
st_collection_extract(i, "LINESTRING")
#> LINESTRING (4 0, 3 0)

## A GEOMETRYCOLLECTION
aa <- rbind(st_sf(a=1, geom = st_sfc(i)),
      st_sf(a=2, geom = st_sfc(j)))

## With sf objects
st_collection_extract(aa, "POLYGON")
#> Simple feature collection with 5 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 5.5 ymin: -0.5 xmax: 8 ymax: 1.5
#> CRS:           NA
#>     a                           geom
#> 1.2 1 MULTIPOLYGON (((5.5 0, 7 0,...
#> 1.3 1 MULTIPOLYGON (((6.6 1, 8 1,...
#> 2.2 2 MULTIPOLYGON (((5.5 0, 7 0,...
#> 2.3 2 MULTIPOLYGON (((6.6 1, 8 1,...
#> 2.4 2 MULTIPOLYGON (((5.5 0, 7 0,...
st_collection_extract(aa, "LINESTRING")
#> Simple feature collection with 2 features and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 3 ymin: 0 xmax: 4 ymax: 0
#> CRS:           NA
#>     a                  geom
#> 1.1 1 LINESTRING (4 0, 3 0)
#> 2.1 2 LINESTRING (4 0, 3 0)
st_collection_extract(aa, "POINT")
#> Simple feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 0 xmax: 1 ymax: 0
#> CRS:           NA
#>   a        geom
#> 1 1 POINT (1 0)
#> 2 2 POINT (1 0)

## With sfc objects
st_collection_extract(st_geometry(aa), "POLYGON")
#> Geometry set for 5 features 
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 5.5 ymin: -0.5 xmax: 8 ymax: 1.5
#> CRS:           NA
#> MULTIPOLYGON (((5.5 0, 7 0, 7 -0.5, 6 -0.5, 5.5...
#> MULTIPOLYGON (((6.6 1, 8 1, 8 1.5, 7 1.5, 6.6 1)))
#> MULTIPOLYGON (((5.5 0, 7 0, 7 -0.5, 6 -0.5, 5.5...
#> MULTIPOLYGON (((6.6 1, 8 1, 8 1.5, 7 1.5, 6.6 1)))
#> MULTIPOLYGON (((5.5 0, 7 0, 7 -0.5, 6 -0.5, 5.5...
st_collection_extract(st_geometry(aa), "LINESTRING")
#> Geometry set for 2 features 
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 3 ymin: 0 xmax: 4 ymax: 0
#> CRS:           NA
#> LINESTRING (4 0, 3 0)
#> LINESTRING (4 0, 3 0)
st_collection_extract(st_geometry(aa), "POINT")
#> Geometry set for 2 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 0 xmax: 1 ymax: 0
#> CRS:           NA
#> POINT (1 0)
#> POINT (1 0)

## A GEOMETRY of single types
bb <- rbind(
  st_sf(a = 1, geom = st_sfc(pt)),
  st_sf(a = 2, geom = st_sfc(ls)),
  st_sf(a = 3, geom = st_sfc(poly1)),
  st_sf(a = 4, geom = st_sfc(multipoly))
)

st_collection_extract(bb, "POLYGON")
#> Simple feature collection with 2 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 5.5 ymin: -0.5 xmax: 8 ymax: 1.5
#> CRS:           NA
#>   a                           geom
#> 3 3 MULTIPOLYGON (((5.5 0, 7 0,...
#> 4 4 MULTIPOLYGON (((5.5 0, 7 0,...

## A GEOMETRY of mixed single types and GEOMETRYCOLLECTIONS
cc <- rbind(aa, bb)

st_collection_extract(cc, "POLYGON")
#> Simple feature collection with 7 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 5.5 ymin: -0.5 xmax: 8 ymax: 1.5
#> CRS:           NA
#>     a                           geom
#> 1.2 1 MULTIPOLYGON (((5.5 0, 7 0,...
#> 1.3 1 MULTIPOLYGON (((6.6 1, 8 1,...
#> 2.2 2 MULTIPOLYGON (((5.5 0, 7 0,...
#> 2.3 2 MULTIPOLYGON (((6.6 1, 8 1,...
#> 2.4 2 MULTIPOLYGON (((5.5 0, 7 0,...
#> 5   3 MULTIPOLYGON (((5.5 0, 7 0,...
#> 6   4 MULTIPOLYGON (((5.5 0, 7 0,...
```
