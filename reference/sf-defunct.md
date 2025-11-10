# Deprecated functions in `sf`

These functions are provided for compatibility with older version of
`sf`. They will eventually be completely removed.

- Use [`st_read()`](https://r-spatial.github.io/sf/reference/st_read.md)
  instead of `st_read_db()`.

- Use
  [`st_write()`](https://r-spatial.github.io/sf/reference/st_write.md)
  instead_of `st_write_db()`

## Usage

``` r
st_read_db(
  conn = NULL,
  table = NULL,
  query = NULL,
  geom_column = NULL,
  EWKB = TRUE,
  ...
)

st_write_db(
  conn = NULL,
  obj,
  table = deparse(substitute(obj)),
  ...,
  drop = FALSE,
  append = FALSE
)
```

## Arguments

- conn:

  open database connection

- table:

  table name

- query:

  SQL query to select records; see details

- geom_column:

  deprecated. Geometry column name

- EWKB:

  logical; is the WKB of type EWKB? if missing, defaults to `TRUE`

- ...:

  parameter(s) passed on to
  [st_as_sf](https://r-spatial.github.io/sf/reference/st_as_sf.md)

## Details

The `geom_column` argument is deprecated. The function will
automatically find the `geometry` type columns. For the `RPostgreSQL`
drivers it will try to cast all the character columns, which can be long
for very wide tables.
