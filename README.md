
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggxmean

<!-- badges: start -->

<!-- badges: end -->

The goal of ggxmean is plot the mean of x - and some other things like
y\!

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EvaMaeRey/ggxmean")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 3.6.2
library(ggxmean)

## basic example code
cars %>% 
  ggplot() +
  aes(x = speed,
      y = dist) +
  geom_point() + 
  geom_xvlines(alpha = .1,
               linetype = "dashed") +
  geom_xmean(color = "firebrick",
             size = 2,
             linetype = "dotted")
#> Warning: Duplicated aesthetics after name standardisation: y
```

<img src="man/figures/README-example-1.png" width="100%" />
