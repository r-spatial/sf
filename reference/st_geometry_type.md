# Return geometry type of an object

Return geometry type of an object, as a factor

## Usage

``` r
st_geometry_type(x, by_geometry = TRUE)
```

## Arguments

- x:

  object of class [sf](https://r-spatial.github.io/sf/reference/sf.md)
  or [sfc](https://r-spatial.github.io/sf/reference/sfc.md)

- by_geometry:

  logical; if `TRUE`, return geometry type of each geometry, else return
  geometry type of the set

## Value

a factor with the geometry type of each simple feature geometry in `x`,
or that of the whole set
