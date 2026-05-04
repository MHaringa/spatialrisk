# Automatically create a plot for objects obtained from `find_highest_concentration()`

Automatically create a plot for objects obtained from
[`find_highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/find_highest_concentration.md).

## Usage

``` r
# S3 method for class 'concentration'
plot(
  x,
  type = c("concentration", "focal", "rasterized", "updated_focal"),
  color1 = NULL,
  max.rad = 20,
  ...
)
```

## Arguments

- x:

  x object of class `concentration` obtained from
  [`highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/highest_concentration.md)

- type:

  is one of "concentration" (default), "rasterized", "focal",
  "updated_focal". See details for more information.

- color1:

  color when one concentration is plotted (default is "#4B0055").

- max.rad:

  maximal radius for size of circles in plot (default is 20).

- ...:

  additional arguments.

## Details

More info for type:

1.  "concentration": this is..

2.  "focal": this is..

3.  "rasterized": this is..

4.  "updated_focal": this is..

## Author

Martin Haringa

## Examples

``` r
x <- find_highest_concentration(Groningen, "amount")
plot(x, "concentration")
plot(x, "rasterized")
plot(x, "focal")
plot(x, "updated_focal")
```
