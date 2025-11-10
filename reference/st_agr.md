# get or set relation_to_geometry attribute of an `sf` object

get or set relation_to_geometry attribute of an `sf` object

## Usage

``` r
NA_agr_

st_agr(x, ...)

st_agr(x) <- value

st_set_agr(x, value)
```

## Format

An object of class `factor` of length 1.

## Arguments

- x:

  object of class `sf`

- ...:

  ignored

- value:

  character, or factor with appropriate levels; if named, names should
  correspond to the non-geometry list-column columns of `x`

## Details

`NA_agr_` is the `agr` object with a missing value.
