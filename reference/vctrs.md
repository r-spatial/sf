# vctrs methods for sf objects

vctrs methods for sf objects

## Usage

``` r
vec_ptype2.sfc(x, y, ...)

# Default S3 method
vec_ptype2.sfc(x, y, ..., x_arg = "x", y_arg = "y")

# S3 method for class 'sfc'
vec_ptype2.sfc(x, y, ...)

vec_cast.sfc(x, to, ...)

# S3 method for class 'sfc'
vec_cast.sfc(x, to, ...)

# Default S3 method
vec_cast.sfc(x, to, ...)
```

## Arguments

- x, y:

  Vector types.

- ...:

  These dots are for future extensions and must be empty.

- x_arg, y_arg:

  Argument names for `x` and `y`.

- to:

  Type to cast to. If `NULL`, `x` will be returned as is.
