# Add or remove overviews to/from a raster image

add or remove overviews to/from a raster image

## Usage

``` r
gdal_addo(
  file,
  overviews = c(2, 4, 8, 16),
  method = "NEAREST",
  layers = integer(0),
  options = character(0),
  config_options = character(0),
  clean = FALSE,
  read_only = FALSE
)
```

## Arguments

- file:

  character; file name

- overviews:

  integer; overview levels

- method:

  character; method to create overview; one of: nearest, average, rms,
  gauss, cubic, cubicspline, lanczos, average_mp, average_magphase, mode

- layers:

  integer; layers to create overviews for (default: all)

- options:

  character; dataset opening options

- config_options:

  named character vector with GDAL config options, like
  `c(option1=value1, option2=value2)`

- clean:

  logical; if `TRUE` only remove overviews, do not add

- read_only:

  logical; if `TRUE`, add overviews to another file with extension
  `.ovr` added to `file`

## Value

`TRUE`, invisibly, on success

## See also

[gdal_utils](https://r-spatial.github.io/sf/reference/gdal_utils.md) for
access to other gdal utilities that have a C API
