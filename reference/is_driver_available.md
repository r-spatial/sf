# Check if driver is available

Search through the driver table if driver is listed

## Usage

``` r
is_driver_available(drv, drivers = st_drivers())
```

## Arguments

- drv:

  character. Name of driver

- drivers:

  data.frame. Table containing driver names and support. Default is from
  [`st_drivers`](https://r-spatial.github.io/sf/reference/st_drivers.md)
