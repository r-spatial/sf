# Return Well-known Text representation of simple feature geometry or coordinate reference system

Return Well-known Text representation of simple feature geometry or
coordinate reference system

## Usage

``` r
# S3 method for class 'crs'
st_as_text(x, ..., projjson = FALSE, pretty = FALSE)

st_as_text(x, ...)

# S3 method for class 'sfg'
st_as_text(x, ...)

# S3 method for class 'sfc'
st_as_text(x, ..., EWKT = FALSE)
```

## Arguments

- x:

  object of class `sfg`, `sfc` or `crs`

- ...:

  modifiers; in particular `digits` can be passed to control the number
  of digits used

- projjson:

  logical; if TRUE, return projjson form (requires GDAL 3.1 and PROJ
  6.2), else return well-known-text form

- pretty:

  logical; if TRUE, print human-readable well-known-text representation
  of a coordinate reference system

- EWKT:

  logical; if TRUE, print SRID=xxx; before the WKT string if `epsg` is
  available

## Details

The returned WKT representation of simple feature geometry conforms to
the [simple features access](https://www.ogc.org/standards/sfa/)
specification and extensions (known as EWKT, supported by PostGIS and
other simple features implementations for addition of a SRID to a WKT
string).

## Note

To improve conversion performance, the lwgeom package can be used (it
must be installed beforehand) and set the
`Sys.setenv("LWGEOM_WKT" = "true")` environment variable. This will also
result in faster printing of complex geometries. Note that the
representation as WKT is different from the sf package and may cause
reproducibility problems. An alternative solution is to use the
[`lwgeom::st_astext()`](https://r-spatial.github.io/lwgeom/reference/st_astext.html)
or [`wk::as_wkt()`](https://paleolimbot.github.io/wk/reference/wkt.html)
functions.

## Examples

``` r
st_as_text(st_point(1:2))
#> [1] "POINT (1 2)"
st_as_text(st_sfc(st_point(c(-90,40)), crs = 4326), EWKT = TRUE)
#> [1] "SRID=4326;POINT (-90 40)"
```
