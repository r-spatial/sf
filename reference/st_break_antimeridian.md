# Break antimeridian for plotting not centred on Greenwich

Longitudes can be broken at the antimeridian of a target central
longitude to permit plotting of (usually world) line or polygon objects
centred on the chosen central longitude. The method may only be used
with non-projected, geographical coordinates and linestring or polygon
objects. s2 is turned off internally to permit the use of a rectangular
bounding box. If the input geometries go outside `[-180, 180]` degrees
longitude, the protruding geometries will also be split using the same
`tol=` values; in this case empty geometries will be dropped first.

## Usage

``` r
st_break_antimeridian(x, lon_0 = 0, tol = 1e-04, ...)

# S3 method for class 'sf'
st_break_antimeridian(x, lon_0 = 0, tol = 1e-04, ...)

# S3 method for class 'sfc'
st_break_antimeridian(x, lon_0 = 0, tol = 1e-04, ...)
```

## Arguments

- x:

  object of class `sf` or `sfc`

- lon_0:

  target central longitude (degrees)

- tol:

  half of break width (degrees, default 0.0001)

- ...:

  ignored here

## Examples

``` r
# \donttest{
if (require("maps", quietly=TRUE)) {
 opar = par(mfrow=c(3, 2))
 wld = st_as_sf(map(fill=FALSE, interior=FALSE, plot=FALSE), fill=FALSE)
 for (lon_0 in c(-170, -90, -10, 10, 90, 170)) {
   br = st_break_antimeridian(wld, lon_0 = lon_0)
   tr = st_transform(br, paste0("+proj=natearth +lon_0=", lon_0))
   plot(st_geometry(tr), main=lon_0)
 }
 par(opar)
}
#> Warning: st_break_antimeridian: longitude coordinates outside [-180, 180]
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Spherical geometry (s2) switched on
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Spherical geometry (s2) switched on
#> Warning: st_break_antimeridian: longitude coordinates outside [-180, 180]
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Spherical geometry (s2) switched on
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Spherical geometry (s2) switched on
#> Warning: st_break_antimeridian: longitude coordinates outside [-180, 180]
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Spherical geometry (s2) switched on
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Spherical geometry (s2) switched on
#> Warning: st_break_antimeridian: longitude coordinates outside [-180, 180]
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Spherical geometry (s2) switched on
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Spherical geometry (s2) switched on
#> Warning: st_break_antimeridian: longitude coordinates outside [-180, 180]
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Spherical geometry (s2) switched on
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Spherical geometry (s2) switched on
#> Warning: st_break_antimeridian: longitude coordinates outside [-180, 180]
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Spherical geometry (s2) switched on
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Spherical geometry (s2) switched on

# }
```
