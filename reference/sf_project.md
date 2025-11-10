# directly transform a set of coordinates

directly transform a set of coordinates

## Usage

``` r
sf_add_proj_units()

sf_project(
  from = character(0),
  to = character(0),
  pts,
  keep = FALSE,
  warn = TRUE,
  authority_compliant = st_axis_order()
)
```

## Arguments

- from:

  character description of source CRS, or object of class `crs`, or
  pipeline describing a transformation

- to:

  character description of target CRS, or object of class `crs`

- pts:

  two-, three- or four-column numeric matrix, or object that can be
  coerced into a matrix; columns 3 and 4 contain z and t values.

- keep:

  logical value controlling the handling of unprojectable points. If
  `keep` is `TRUE`, then such points will yield `Inf` or `-Inf` in the
  return value; otherwise an error is reported and nothing is returned.

- warn:

  logical; if `TRUE`, warn when non-finite values are generated

- authority_compliant:

  logical; `TRUE` means handle axis order authority compliant (e.g.
  EPSG:4326 implying x=lat, y=lon), `FALSE` means use visualisation
  order (i.e. always x=lon, y=lat)

## Value

two-column numeric matrix with transformed/converted coordinates,
returning invalid values as `Inf`

## Details

`sf_add_proj_units` loads the PROJ units `link`, `us_in`, `ind_yd`,
`ind_ft`, and `ind_ch` into the udunits database, and returns `TRUE`
invisibly on success.

## Examples

``` r
sf_add_proj_units()
```
