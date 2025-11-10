# crop an sf object to a specific rectangle

crop an sf object to a specific rectangle

## Usage

``` r
st_crop(x, y, ...)

# S3 method for class 'sfc'
st_crop(x, y, ..., xmin, ymin, xmax, ymax)

# S3 method for class 'sf'
st_crop(x, y, ...)
```

## Arguments

- x:

  object of class `sf` or `sfc`

- y:

  numeric vector with named elements `xmin`, `ymin`, `xmax` and `ymax`,
  or object of class `bbox`, or object for which there is an
  [st_bbox](https://r-spatial.github.io/sf/reference/st_bbox.md) method
  to convert it to a `bbox` object

- ...:

  ignored

- xmin:

  minimum x extent of cropping area

- ymin:

  minimum y extent of cropping area

- xmax:

  maximum x extent of cropping area

- ymax:

  maximum y extent of cropping area

## Details

setting arguments `xmin`, `ymin`, `xmax` and `ymax` implies that
argument `y` gets ignored.

## Examples

``` r
box = c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
pol = st_sfc(st_buffer(st_point(c(.5, .5)), .6))
pol_sf = st_sf(a=1, geom=pol)
plot(st_crop(pol, box))

plot(st_crop(pol_sf, st_bbox(box)))
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries

# alternative:
plot(st_crop(pol, xmin = 0, ymin = 0, xmax = 1, ymax = 1))
```
