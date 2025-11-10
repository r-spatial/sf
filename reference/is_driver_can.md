# Check if a driver can perform an action

Search through the driver table to match a driver name with an action
(e.g. `"write"`) and check if the action is supported.

## Usage

``` r
is_driver_can(drv, drivers = st_drivers(), operation = "write")
```

## Arguments

- drv:

  character. Name of driver

- drivers:

  data.frame. Table containing driver names and support. Default is from
  [`st_drivers`](https://r-spatial.github.io/sf/reference/st_drivers.md)

- operation:

  character. What action to check
