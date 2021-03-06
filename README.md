
![](https://images.unsplash.com/photo-1572291720677-d8d28ac52a5b?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1556&q=80)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggxmean

<!-- badges: start -->
<!-- badges: end -->

The goal of ggxmean is plot the mean of x - and some other things like
y!

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EvaMaeRey/ggxmean")
```

-   [In-progress long form rationale and package
    exploration](https://evamaerey.github.io/ggxmean/manuscript.pdf)

-   [Talk MAA Metro
    NY](https://evamaerey.github.io/ggxmean/talk_maa_metro_ny.html)

## Examples

<img src="https://github.com/EvaMaeRey/ggxmean/blob/master/docs/flipbook_preview.gif?raw=true" width="100%" />

<a href="https://evamaerey.github.io/ggxmean/flipbook.html" target="_blank">Open
flipbook in a new tab</a>

``` r
# knitr::opts_chunk$set(eval = F)
options(gganimate.nframes = 60)
library(tidyverse)
library(ggxmean)
#library(transformr) might help w/ animate

## basic example code
cars %>% 
  ggplot() +
  aes(x = speed,
      y = dist) +
  geom_point() + 
  ggxmean::geom_x_mean() +
  ggxmean::geom_x_mean_label() +
  ggxmean::geom_y_mean() +
  ggxmean::geom_xy_means(color = "red",
                size = 5) +
  ggxmean::geom_lm_fitted(color = "goldenrod3",
                          size = 3) +
  ggxmean::geom_lm_pred_int() +
  ggxmean::geom_lm() +
  ggxmean::geom_lm_residuals(linetype = "dashed") +
  ggxmean::geom_lm_conf_int() +
  ggxmean::geom_lm_formula()
```

<img src="man/figures/README-example-1.png" width="100%" />

------------------------------------------------------------------------

``` r
palmerpenguins::penguins %>% 
  ggplot() +
  aes(x = bill_length_mm) +
  geom_rug(alpha = .3) +
  geom_histogram(alpha = .4) +
  geom_x_mean() +
  aes(fill = species) + # unexpected behavior here
  aes(color = species) +
  facet_wrap(facets = vars(species)) +
  gganimate::transition_layers()
```

<img src="man/figures/README-unnamed-chunk-3-1.gif" width="100%" />

``` r
library(ggxmean)
palmerpenguins::penguins %>% 
  drop_na() %>% 
  ggplot() +
  aes(x = bill_length_mm) +
  aes(y = flipper_length_mm) +
  geom_point() +
  ggxmean:::geom_x_mean() +
  ggxmean:::geom_y_mean() +
  ggxmean:::geom_y_line(alpha = .2) +
  ggxmean:::geom_x_line(alpha = .2) +
  ggxmean:::geom_xdiff() +
  ggxmean:::geom_ydiff() +
  ggxmean:::geom_x1sd(linetype = "dashed") +
  ggxmean:::geom_y1sd(linetype = "dashed") +
  ggxmean:::geom_diffsmultiplied() +
  ggxmean:::geom_xydiffsmean(alpha = 1) +
  ggxmean:::geom_rsq1() +
  ggxmean:::geom_corrlabel() +
  gganimate::transition_layers()
```

<img src="man/figures/README-unnamed-chunk-4-1.gif" width="100%" />
