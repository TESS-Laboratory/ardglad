
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ardglad

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of ardglad is to provide access to the Global Land Analysis &
Discovery (GLAD) Landsat Analysis Ready Data (ARD). The [GLAD
ARD](https://www.glad.umd.edu/ard/home#format) represents a 16-day time
series of globally consistent, tiled Landsat normalized surface
reflectance from 1997 to the present operationally updated every 16
days.

This package interacts with the [GLAD ARD
API](https://www.glad.umd.edu/ard/home#download) to download and process
the data. **The package is currently in development and only provides
functionality for returning the tile URLs**.

## Installation

You can install the development version of ardglad like so:

``` r
# install.packages("pak")
pak::pkg_install("TESS-Laboratory/ardglad")
```

## Example

Here is an example of how to generate the urls for the GLAD ARD tiles
for an example location in Rondônia, Brazil.

``` r
library(ardglad)

rondonia_eg <- sf::st_point(c(-61.604, -12.864)) |>
  sf::st_sfc(crs = 4326) |>
  sf::st_sf() |>
  sf::st_buffer(20000)

rondonia_urls <- ard_glad_urls(rondonia_eg, "2023-07-01", "2023-07-30")

print(rondonia_urls)
#> 
#> ── < ARD GLAD urls >
#> 
#> ── [[1]] 2023-06-26:2023-07-11 ─────────────────────────────────────────────────
#> https://glad.umd.edu/dataset/glad_ard2/12S/061W_12S/1001.tif
#> https://glad.umd.edu/dataset/glad_ard2/13S/061W_13S/1001.tif
#> 
#> ── [[2]] 2023-07-12:2023-07-27 ─────────────────────────────────────────────────
#> https://glad.umd.edu/dataset/glad_ard2/12S/061W_12S/1002.tif
#> https://glad.umd.edu/dataset/glad_ard2/13S/061W_13S/1002.tif
#> 
#> ── [[3]] 2023-07-28:2023-08-12 ─────────────────────────────────────────────────
#> https://glad.umd.edu/dataset/glad_ard2/12S/061W_12S/1003.tif
#> https://glad.umd.edu/dataset/glad_ard2/13S/061W_13S/1003.tif
```

And now let’s download the data and plot it.

``` r
rg <- ard_glad_download(rondonia_urls, "rondonia_glad")
#> ℹ Downloading files for time period: 2023-06-26:2023-07-11
#> ℹ Downloading files for time period: 2023-07-12:2023-07-27
#> ℹ Downloading files for time period: 2023-07-28:2023-08-12

# plot the result
par(mfrow = c(1, 3))
p <- mapply(
  \(x, y) {
    terra::plotRGB(x,
      r = 3, g = 2, b = 1,
      zlim = c(0, 7000),
      zcol = TRUE,
      stretch = "lin",
      axes = TRUE,
      mar = c(2, 2, 2, 1),
      main = y
    )
  },
  rg,
  names(rg)
)
```

<img src="man/figures/README-download-data-1.png" width="100%" />
