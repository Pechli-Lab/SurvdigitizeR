
# SurvdigtizeR

<!-- badges: start -->
<!-- badges: end -->

The goal of SurvdigtizeR is to facilitate the digitization of kaplan meir survival curves.

## Installation

You can install the released version of SurvdigtizeR from github.

``` r
# Install development version from GitHub
devtools::install_github("Pechli-Lab/SurvdigitizeR")
```

Or by opening the folder encompassing the project. 

``` r

library(devtools)
install()

```

## Example

This is a basic example digitizing the survival curve found in the vignettes folder.

``` r
library(SurvdigtizeR)

#parent_digitizer(curve_location = here::here("vignettes","FALSE_250_base_3.jpeg"),
#               sen = 0.1,
#               Wsen_i = 30,
#               OCR_words_i = F,
#               num_curves1 = 3,
#               x_start_i = 0,
#               x_end_i = 500,
#               x_incr = 100,
#               y_start_i = 0,
#               y_end_i = 1,
#               y_incr = 0.2,
#               Y_values_vertical = F)
```

This R package was developed with the support of CADTH. 
