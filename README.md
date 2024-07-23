
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
the data. The package is currently in development and only provides
functionality for returning the tile URLs.

## Installation

You can install the development version of ardglad like so:

``` r
# install.packages("pak")
pak::pkg_install("ardglad")
```

## Example

Here is an example of how to generate the urls for the GLAD ARD tiles
for an example location in Rondonia, Brazil.

``` r
library(ardglad)

rondonia_eg <- sf::st_point(c(-61.604, -12.864)) |>
  sf::st_sfc(crs = 4326) |>
  sf::st_sf() |>
  sf::st_buffer(50000)

ard_glad_urls(rondonia_eg, "2023-01-01", "2023-03-30")
#> $`2023-01-01:2023-01-16`
#> [1] "https://glad.umd.edu/dataset/glad_ard2/13S/062W_13S/990.tif"
#> [2] "https://glad.umd.edu/dataset/glad_ard2/12S/062W_12S/990.tif"
#> [3] "https://glad.umd.edu/dataset/glad_ard2/12S/061W_12S/990.tif"
#> [4] "https://glad.umd.edu/dataset/glad_ard2/13S/061W_13S/990.tif"
#> 
#> $`2023-01-17:2023-02-01`
#> [1] "https://glad.umd.edu/dataset/glad_ard2/13S/062W_13S/991.tif"
#> [2] "https://glad.umd.edu/dataset/glad_ard2/12S/062W_12S/991.tif"
#> [3] "https://glad.umd.edu/dataset/glad_ard2/12S/061W_12S/991.tif"
#> [4] "https://glad.umd.edu/dataset/glad_ard2/13S/061W_13S/991.tif"
#> 
#> $`2023-02-02:2023-02-17`
#> [1] "https://glad.umd.edu/dataset/glad_ard2/13S/062W_13S/992.tif"
#> [2] "https://glad.umd.edu/dataset/glad_ard2/12S/062W_12S/992.tif"
#> [3] "https://glad.umd.edu/dataset/glad_ard2/12S/061W_12S/992.tif"
#> [4] "https://glad.umd.edu/dataset/glad_ard2/13S/061W_13S/992.tif"
#> 
#> $`2023-02-18:2023-03-05`
#> [1] "https://glad.umd.edu/dataset/glad_ard2/13S/062W_13S/993.tif"
#> [2] "https://glad.umd.edu/dataset/glad_ard2/12S/062W_12S/993.tif"
#> [3] "https://glad.umd.edu/dataset/glad_ard2/12S/061W_12S/993.tif"
#> [4] "https://glad.umd.edu/dataset/glad_ard2/13S/061W_13S/993.tif"
#> 
#> $`2023-03-06:2023-03-21`
#> [1] "https://glad.umd.edu/dataset/glad_ard2/13S/062W_13S/994.tif"
#> [2] "https://glad.umd.edu/dataset/glad_ard2/12S/062W_12S/994.tif"
#> [3] "https://glad.umd.edu/dataset/glad_ard2/12S/061W_12S/994.tif"
#> [4] "https://glad.umd.edu/dataset/glad_ard2/13S/061W_13S/994.tif"
#> 
#> $`2023-03-22:2023-04-06`
#> [1] "https://glad.umd.edu/dataset/glad_ard2/13S/062W_13S/995.tif"
#> [2] "https://glad.umd.edu/dataset/glad_ard2/12S/062W_12S/995.tif"
#> [3] "https://glad.umd.edu/dataset/glad_ard2/12S/061W_12S/995.tif"
#> [4] "https://glad.umd.edu/dataset/glad_ard2/13S/061W_13S/995.tif"
```
