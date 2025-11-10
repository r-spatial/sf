# Coerce geometry to MULTI\* geometry

Mixes of POINTS and MULTIPOINTS, LINESTRING and MULTILINESTRING, POLYGON
and MULTIPOLYGON are returned as MULTIPOINTS, MULTILINESTRING and
MULTIPOLYGONS respectively

## Usage

``` r
st_cast_sfc_default(x)
```

## Arguments

- x:

  list of geometries or simple features

## Details

Geometries that are already MULTI\* are left unchanged. Features that
can't be cast to a single MULTI\* geometry are return as a
GEOMETRYCOLLECTION
