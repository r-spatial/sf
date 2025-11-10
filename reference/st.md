# Create simple feature from a numeric vector, matrix or list

Create simple feature from a numeric vector, matrix or list

## Usage

``` r
st_point(x = c(NA_real_, NA_real_), dim = "XYZ")

st_multipoint(x = matrix(numeric(0), 0, 2), dim = "XYZ")

st_linestring(x = matrix(numeric(0), 0, 2), dim = "XYZ")

st_polygon(x = list(), dim = if (length(x)) "XYZ" else "XY")

st_multilinestring(x = list(), dim = if (length(x)) "XYZ" else "XY")

st_multipolygon(x = list(), dim = if (length(x)) "XYZ" else "XY")

st_geometrycollection(x = list(), dims = "XY")

# S3 method for class 'sfg'
print(x, ..., width = 0)

# S3 method for class 'sfg'
head(x, n = 10L, ...)

# S3 method for class 'sfg'
format(x, ..., width = 30)

# S3 method for class 'sfg'
c(..., recursive = FALSE, flatten = TRUE)

# S3 method for class 'sfg'
as.matrix(x, ...)
```

## Arguments

- x:

  for `st_point`, numeric vector (or one-row-matrix) of length 2, 3 or
  4; for `st_linestring` and `st_multipoint`, numeric matrix with points
  in rows; for `st_polygon` and `st_multilinestring`, list with numeric
  matrices with points in rows; for `st_multipolygon`, list of lists
  with numeric matrices; for `st_geometrycollection` list with
  (non-geometrycollection) simple feature geometry (sfg) objects; see
  examples below

- dim:

  character, indicating dimensions: "XY", "XYZ", "XYM", or "XYZM"; only
  really needed for three-dimensional points (which can be either XYZ or
  XYM) or empty geometries; see details

- dims:

  character; specify dimensionality in case of an empty (NULL)
  geometrycollection, in which case `x` is the empty
  [`list()`](https://rdrr.io/r/base/list.html).

- ...:

  objects to be pasted together into a single simple feature

- width:

  integer; number of characters to be printed (max 30; 0 means print
  everything)

- n:

  integer; number of elements to be selected

- recursive:

  logical; ignored

- flatten:

  logical; if `TRUE`, try to simplify results; if `FALSE`, return
  geometrycollection containing all objects

## Value

object of the same nature as `x`, but with appropriate class attribute
set

as.matrix returns the set of points that form a geometry as a single
matrix, where each point is a row; use `unlist(x, recursive = FALSE)` to
get sets of matrices.

## Details

"XYZ" refers to coordinates where the third dimension represents
altitude, "XYM" refers to three-dimensional coordinates where the third
dimension refers to something else ("M" for measure); checking of the
sanity of `x` may be only partial.

When `flatten=TRUE`, this method may merge points into a multipoint
structure, and may not preserve order, and hence cannot be reverted.
When given fish, it returns fish soup.

## Examples

``` r
(p1 = st_point(c(1,2)))
#> POINT (1 2)
class(p1)
#> [1] "XY"    "POINT" "sfg"  
st_bbox(p1)
#> xmin ymin xmax ymax 
#>    1    2    1    2 
(p2 = st_point(c(1,2,3)))
#> POINT Z (1 2 3)
class(p2)
#> [1] "XYZ"   "POINT" "sfg"  
(p3 = st_point(c(1,2,3), "XYM"))
#> POINT M (1 2 3)
pts = matrix(1:10, , 2)
(mp1 = st_multipoint(pts))
#> MULTIPOINT ((1 6), (2 7), (3 8), (4 9), (5 10))
pts = matrix(1:15, , 3)
(mp2 = st_multipoint(pts))
#> MULTIPOINT Z ((1 6 11), (2 7 12), (3 8 13), (4 9 14), (5 10 15))
(mp3 = st_multipoint(pts, "XYM"))
#> MULTIPOINT M ((1 6 11), (2 7 12), (3 8 13), (4 9 14), (5 10 15))
pts = matrix(1:20, , 4)
(mp4 = st_multipoint(pts))
#> MULTIPOINT ZM ((1 6 11 16), (2 7 12 17), (3 8 13 18), (4 9 14 19), (5 10 15 20))
pts = matrix(1:10, , 2)
(ls1 = st_linestring(pts))
#> LINESTRING (1 6, 2 7, 3 8, 4 9, 5 10)
pts = matrix(1:15, , 3)
(ls2 = st_linestring(pts))
#> LINESTRING Z (1 6 11, 2 7 12, 3 8 13, 4 9 14, 5 10 15)
(ls3 = st_linestring(pts, "XYM"))
#> LINESTRING M (1 6 11, 2 7 12, 3 8 13, 4 9 14, 5 10 15)
pts = matrix(1:20, , 4)
(ls4 = st_linestring(pts))
#> LINESTRING ZM (1 6 11 16, 2 7 12 17, 3 8 13 18, 4 9 14 19, 5 10 15 20)
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pts = list(outer, hole1, hole2)
(ml1 = st_multilinestring(pts))
#> MULTILINESTRING ((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 2, 2 2, 2 1, 1 1), (5 5, 5 6, 6 6, 6 5, 5 5))
pts3 = lapply(pts, function(x) cbind(x, 0))
(ml2 = st_multilinestring(pts3))
#> MULTILINESTRING Z ((0 0 0, 10 0 0, 10 10 0, 0 10 0, 0 0 0), (1 1 0, 1 2 0, 2 2 0, 2 1 0, 1 1 0), (5 5 0, 5 6 0, 6 6 0, 6 5 0, 5 5 0))
(ml3 = st_multilinestring(pts3, "XYM"))
#> MULTILINESTRING M ((0 0 0, 10 0 0, 10 10 0, 0 10 0, 0 0 0), (1 1 0, 1 2 0, 2 2 0, 2 1 0, 1 1 0), (5 5 0, 5 6 0, 6 6 0, 6 5 0, 5 5 0))
pts4 = lapply(pts3, function(x) cbind(x, 0))
(ml4 = st_multilinestring(pts4))
#> MULTILINESTRING ZM ((0 0 0 0, 10 0 0 0, 10 10 0 0, 0 10 0 0, 0 0 0 0), (1 1 0 0, 1 2 0 0, 2 2 0 0, 2 1 0 0, 1 1 0 0), (5 5 0 0, 5 6 0 0, 6 6 0 0, 6 5 0 0, 5 5 0 0))
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pts = list(outer, hole1, hole2)
(pl1 = st_polygon(pts))
#> POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 2, 2 2, 2 1, 1 1), (5 5, 5 6, 6 6, 6 5, 5 5))
pts3 = lapply(pts, function(x) cbind(x, 0))
(pl2 = st_polygon(pts3))
#> POLYGON Z ((0 0 0, 10 0 0, 10 10 0, 0 10 0, 0 0 0), (1 1 0, 1 2 0, 2 2 0, 2 1 0, 1 1 0), (5 5 0, 5 6 0, 6 6 0, 6 5 0, 5 5 0))
(pl3 = st_polygon(pts3, "XYM"))
#> POLYGON M ((0 0 0, 10 0 0, 10 10 0, 0 10 0, 0 0 0), (1 1 0, 1 2 0, 2 2 0, 2 1 0, 1 1 0), (5 5 0, 5 6 0, 6 6 0, 6 5 0, 5 5 0))
pts4 = lapply(pts3, function(x) cbind(x, 0))
(pl4 = st_polygon(pts4))
#> POLYGON ZM ((0 0 0 0, 10 0 0 0, 10 10 0 0, 0 10 0 0, 0 0 0 0), (1 1 0 0, 1 2 0 0, 2 2 0 0, 2 1 0 0, 1 1 0 0), (5 5 0 0, 5 6 0 0, 6 6 0 0, 6 5 0 0, 5 5 0 0))
pol1 = list(outer, hole1, hole2)
pol2 = list(outer + 12, hole1 + 12)
pol3 = list(outer + 24)
mp = list(pol1,pol2,pol3)
(mp1 = st_multipolygon(mp))
#> MULTIPOLYGON (((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 2, 2 2, 2 1, 1 1), (5 5, 5 6, 6 6, 6 5, 5 5)), ((12 12, 22 12, 22 22, 12 22, 12 12), (13 13, 13 14, 14 14, 14 13, 13 13)), ((24 24, 34 24, 34 34, 24 34, 24 24)))
pts3 = lapply(mp, function(x) lapply(x, function(y) cbind(y, 0)))
(mp2 = st_multipolygon(pts3))
#> MULTIPOLYGON Z (((0 0 0, 10 0 0, 10 10 0, 0 10 0, 0 0 0), (1 1 0, 1 2 0, 2 2 0, 2 1 0, 1 1 0), (5 5 0, 5 6 0, 6 6 0, 6 5 0, 5 5 0)), ((12 12 0, 22 12 0, 22 22 0, 12 22 0, 12 12 0), (13 13 0, 13 14 0, 14 14 0, 14 13 0, 13 13 0)), ((24 24 0, 34 24 0, 34 34 0, 24 34 0, 24 24 0)))
(mp3 = st_multipolygon(pts3, "XYM"))
#> MULTIPOLYGON M (((0 0 0, 10 0 0, 10 10 0, 0 10 0, 0 0 0), (1 1 0, 1 2 0, 2 2 0, 2 1 0, 1 1 0), (5 5 0, 5 6 0, 6 6 0, 6 5 0, 5 5 0)), ((12 12 0, 22 12 0, 22 22 0, 12 22 0, 12 12 0), (13 13 0, 13 14 0, 14 14 0, 14 13 0, 13 13 0)), ((24 24 0, 34 24 0, 34 34 0, 24 34 0, 24 24 0)))
pts4 = lapply(mp2, function(x) lapply(x, function(y) cbind(y, 0)))
(mp4 = st_multipolygon(pts4))
#> MULTIPOLYGON ZM (((0 0 0 0, 10 0 0 0, 10 10 0 0, 0 10 0 0, 0 0 0 0), (1 1 0 0, 1 2 0 0, 2 2 0 0, 2 1 0 0, 1 1 0 0), (5 5 0 0, 5 6 0 0, 6 6 0 0, 6 5 0 0, 5 5 0 0)), ((12 12 0 0, 22 12 0 0, 22 22 0 0, 12 22 0 0, 12 12 0 0), (13 13 0 0, 13 14 0 0, 14 14 0 0, 14 13 0 0, 13 13 0 0)), ((24 24 0 0, 34 24 0 0, 34 34 0 0, 24 34 0 0, 24 24 0 0)))
(gc = st_geometrycollection(list(p1, ls1, pl1, mp1)))
#> GEOMETRYCOLLECTION (POINT (1 2), LINESTRING (1 6, 2 7, 3 8, 4 9, 5 10), POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 2, 2 2, 2 1, 1 1), (5 5, 5 6, 6 6, 6 5, 5 5)), MULTIPOLYGON (((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 2, 2 2, 2 1, 1 1), (5 5, 5 6, 6 6, 6 5, 5 5)), ((12 12, 22 12, 22 22, 12 22, 12 12), (13 13, 13 14, 14 14, 14 13, 13 13)), ((24 24, 34 24, 34 34, 24 34, 24 24))))
st_geometrycollection() # empty geometry
#> GEOMETRYCOLLECTION EMPTY
c(st_point(1:2), st_point(5:6))
#> MULTIPOINT ((1 2), (5 6))
c(st_point(1:2), st_multipoint(matrix(5:8,2)))
#> MULTIPOINT ((1 2), (5 7), (6 8))
c(st_multipoint(matrix(1:4,2)), st_multipoint(matrix(5:8,2)))
#> MULTIPOINT ((1 3), (2 4), (5 7), (6 8))
c(st_linestring(matrix(1:6,3)), st_linestring(matrix(11:16,3)))
#> MULTILINESTRING ((1 4, 2 5, 3 6), (11 14, 12 15, 13 16))
c(st_multilinestring(list(matrix(1:6,3))), st_multilinestring(list(matrix(11:16,3))))
#> MULTILINESTRING ((1 4, 2 5, 3 6), (11 14, 12 15, 13 16))
pl = list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))
c(st_polygon(pl), st_polygon(pl))
#> MULTIPOLYGON (((0 0, 1 0, 1 1, 0 1, 0 0)), ((0 0, 1 0, 1 1, 0 1, 0 0)))
c(st_polygon(pl), st_multipolygon(list(pl)))
#> MULTIPOLYGON (((0 0, 1 0, 1 1, 0 1, 0 0)), ((0 0, 1 0, 1 1, 0 1, 0 0)))
c(st_linestring(matrix(1:6,3)), st_point(1:2))
#> GEOMETRYCOLLECTION (LINESTRING (1 4, 2 5, 3 6), POINT (1 2))
c(st_geometrycollection(list(st_point(1:2), st_linestring(matrix(1:6,3)))),
  st_geometrycollection(list(st_multilinestring(list(matrix(11:16,3))))))
#> GEOMETRYCOLLECTION (POINT (1 2), LINESTRING (1 4, 2 5, 3 6), MULTILINESTRING ((11 14, 12 15, 13 16)))
c(st_geometrycollection(list(st_point(1:2), st_linestring(matrix(1:6,3)))),
  st_multilinestring(list(matrix(11:16,3))), st_point(5:6),
  st_geometrycollection(list(st_point(10:11))))
#> GEOMETRYCOLLECTION (MULTILINESTRING ((11 14, 12 15, 13 16)), POINT (5 6), POINT (1 2), LINESTRING (1 4, 2 5, 3 6), POINT (10 11))
```
