# Write `sf` object to Database

Write `sf` object to Database

Write `sf` object to Database

## Usage

``` r
# S4 method for class 'PostgreSQLConnection,character,sf'
dbWriteTable(
  conn,
  name,
  value,
  ...,
  row.names = FALSE,
  overwrite = FALSE,
  append = FALSE,
  field.types = NULL,
  binary = TRUE
)

# S4 method for class 'DBIObject,character,sf'
dbWriteTable(
  conn,
  name,
  value,
  ...,
  row.names = FALSE,
  overwrite = FALSE,
  append = FALSE,
  field.types = NULL,
  binary = TRUE
)
```

## Arguments

- conn:

  DBIObject

- name:

  character vector of names (table names, fields, keywords).

- value:

  a data.frame.

- ...:

  placeholder for future use.

- row.names:

  Add a `row.name` column, or a vector of length `nrow(obj)` containing
  row.names; default `FALSE`.

- overwrite:

  Will try to `drop` table before writing; default `FALSE`.

- append:

  Append rows to existing table; default `FALSE`.

- field.types:

  default `NULL`. Allows to override type conversion from R to
  PostgreSQL. See `dbDataType()` for details.

- binary:

  Send geometries serialized as Well-Known Binary (WKB); if `FALSE`,
  uses Well-Known Text (WKT). Defaults to `TRUE` (WKB).
