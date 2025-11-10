# Return properties of layers in a datasource

Return properties of layers in a datasource

## Usage

``` r
st_layers(dsn, options = character(0), do_count = FALSE)
```

## Arguments

- dsn:

  data source name (interpretation varies by driver - for some drivers,
  `dsn` is a file name, but may also be a folder, or contain the name
  and access credentials of a database)

- options:

  character; driver dependent dataset open options, multiple options
  supported.

- do_count:

  logical; if TRUE, count the features by reading them, even if their
  count is not reported by the driver

## Value

list object of class `sf_layers` with elements

- name:

  name of the layer

- geomtype:

  list with for each layer the geometry types

- features:

  number of features (if reported; see `do_count`)

- fields:

  number of fields

- crs:

  list with for each layer the `crs` object
