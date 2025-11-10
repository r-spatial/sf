# Get precision

Get precision

Set precision

## Usage

``` r
st_precision(x)

st_set_precision(x, precision)

st_precision(x) <- value
```

## Arguments

- x:

  object of class `sfc` or `sf`

- precision:

  numeric, or object of class `units` with distance units (but see
  details); see
  [st_as_binary](https://r-spatial.github.io/sf/reference/st_as_binary.md)
  for how to do this.

- value:

  precision value

## Details

If `precision` is a `units` object, the object on which we set precision
must have a coordinate reference system with compatible distance units.

Setting a `precision` has no direct effect on coordinates of geometries,
but merely set an attribute tag to an `sfc` object. The effect takes
place in
[st_as_binary](https://r-spatial.github.io/sf/reference/st_as_binary.md)
or, more precise, in the C++ function `CPL_write_wkb`, where simple
feature geometries are being serialized to well-known-binary (WKB). This
happens always when routines are called in GEOS library (geometrical
operations or predicates), for writing geometries using
[st_write](https://r-spatial.github.io/sf/reference/st_write.md) or
[write_sf](https://r-spatial.github.io/sf/reference/st_write.md),
`st_make_valid` in package `lwgeom`; also
[aggregate](https://r-spatial.github.io/sf/reference/aggregate.sf.md)
and [summarise](https://r-spatial.github.io/sf/reference/tidyverse.md)
by default union geometries, which calls a GEOS library function.
Routines in these libraries receive rounded coordinates, and possibly
return results based on them.
[st_as_binary](https://r-spatial.github.io/sf/reference/st_as_binary.md)
contains an example of a roundtrip of `sfc` geometries through WKB, in
order to see the rounding happening to R data.

The reason to support precision is that geometrical operations in GEOS
or liblwgeom may work better at reduced precision. For writing data from
R to external resources it is harder to think of a good reason to
limiting precision.

## See also

[st_as_binary](https://r-spatial.github.io/sf/reference/st_as_binary.md)
for an explanation of what setting precision does, and the examples
therein.

## Examples

``` r
x <- st_sfc(st_point(c(pi, pi)))
st_precision(x)
#> [1] 0
st_precision(x) <- 0.01
st_precision(x)
#> [1] 0.01
```
