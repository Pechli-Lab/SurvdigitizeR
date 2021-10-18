# SurvdigtizeR

<!-- badges: start -->
<!-- badges: end -->

The survdigitzerR package takes as input JPEG or PNG kaplan-meier curves and provides time of events. Time of events can be used as input to generate pseudo patient level data that resembles the data used to generate the kaplan-meier curve. The survdigitzeR package is currently under development.  

## Installation

You can install the released version of SurvdigtizeR from github.

``` r
# Install development version from GitHub
devtools::install_github("Pechli-Lab/SurvdigitizeR")
```

Or by opening the folder encompassing this project. 

``` r
library(devtools)
install()
```

## Example

This is a basic example digitizing the survival curve found in the vignettes folder.

``` r
library(SurvdigtizeR)

# parent_digitizer(curve_location = here("vignettes","KMcurve.png"),
#                                   sen = 0.3,
#                                   Wsen_i = 30,
#                                   OCR_words_i = T,
#                                   num_curves1 = 2,
#                                   x_start_i = 0,
#                                   x_end_i = 10,
#                                   x_incr = 1,
#                                   y_start_i = 0,
#                                   y_end_i = 1,y_incr = 0.25,
#                                   Y_values_vertical =   T,censoring_i = F )
```

This R package was developed with the support of CADTH. 
