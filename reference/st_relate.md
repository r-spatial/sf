# Compute DE9-IM relation between pairs of geometries, or match it to a given pattern

Compute DE9-IM relation between pairs of geometries, or match it to a
given pattern

## Usage

``` r
st_relate(x, y, pattern = NA_character_, sparse = !is.na(pattern))
```

## Arguments

- x:

  object of class `sf`, `sfc` or `sfg`

- y:

  object of class `sf`, `sfc` or `sfg`

- pattern:

  character; define the pattern to match to, see details.

- sparse:

  logical; should a sparse matrix be returned (`TRUE`) or a dense
  matrix?

## Value

In case `pattern` is not given, `st_relate` returns a dense `character`
matrix; element `[i,j]` has nine characters, referring to the DE9-IM
relationship between `x[i]` and `y[j]`, encoded as
IxIy,IxBy,IxEy,BxIy,BxBy,BxEy,ExIy,ExBy,ExEy where I refers to interior,
B to boundary, and E to exterior, and e.g. BxIy the dimensionality of
the intersection of the the boundary of `x[i]` and the interior of
`y[j]`, which is one of: 0, 1, 2, or F; digits denoting dimensionality
of intersection, F denoting no intersection. When `pattern` is given, a
dense logical matrix or sparse index list returned with matches to the
given pattern; see
[st_intersects](https://r-spatial.github.io/sf/reference/geos_binary_pred.md)
for a description of the returned matrix or list. See also
<https://en.wikipedia.org/wiki/DE-9IM> for further explanation.

## Examples

``` r
p1 = st_point(c(0,0))
p2 = st_point(c(2,2))
pol1 = st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0)))) - 0.5
pol2 = pol1 + 1
pol3 = pol1 + 2
st_relate(st_sfc(p1, p2), st_sfc(pol1, pol2, pol3))
#>      [,1]        [,2]        [,3]       
#> [1,] "0FFFFF212" "FF0FFF212" "FF0FFF212"
#> [2,] "FF0FFF212" "FF0FFF212" "0FFFFF212"
sfc = st_sfc(st_point(c(0,0)), st_point(c(3,3)))
grd = st_make_grid(sfc, n = c(3,3))
st_intersects(grd)
#> Sparse geometry binary predicate list of length 9, where the predicate
#> was `intersects'
#>  1: 1, 2, 4, 5
#>  2: 1, 2, 3, 4, 5, 6
#>  3: 2, 3, 5, 6
#>  4: 1, 2, 4, 5, 7, 8
#>  5: 1, 2, 3, 4, 5, 6, 7, 8, 9
#>  6: 2, 3, 5, 6, 8, 9
#>  7: 4, 5, 7, 8
#>  8: 4, 5, 6, 7, 8, 9
#>  9: 5, 6, 8, 9
st_relate(grd, pattern = "****1****") # sides, not corners, internals
#> Sparse geometry binary predicate list of length 9, where the predicate
#> was `relate_pattern'
#>  1: 1, 2, 4
#>  2: 1, 2, 3, 5
#>  3: 2, 3, 6
#>  4: 1, 4, 5, 7
#>  5: 2, 4, 5, 6, 8
#>  6: 3, 5, 6, 9
#>  7: 4, 7, 8
#>  8: 5, 7, 8, 9
#>  9: 6, 8, 9
st_relate(grd, pattern = "****0****") # only corners touch
#> Sparse geometry binary predicate list of length 9, where the predicate
#> was `relate_pattern'
#>  1: 5
#>  2: 4, 6
#>  3: 5
#>  4: 2, 8
#>  5: 1, 3, 7, 9
#>  6: 2, 8
#>  7: 5
#>  8: 4, 6
#>  9: 5
st_rook = function(a, b = a) st_relate(a, b, pattern = "F***1****")
st_rook(grd)
#> Sparse geometry binary predicate list of length 9, where the predicate
#> was `relate_pattern'
#>  1: 2, 4
#>  2: 1, 3, 5
#>  3: 2, 6
#>  4: 1, 5, 7
#>  5: 2, 4, 6, 8
#>  6: 3, 5, 9
#>  7: 4, 8
#>  8: 5, 7, 9
#>  9: 6, 8
# queen neighbours, see \url{https://github.com/r-spatial/sf/issues/234#issuecomment-300511129}
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
```
