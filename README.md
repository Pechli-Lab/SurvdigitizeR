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
library(here)

survival_digitize(img_path = here("vignettes","KMcurve.png"),
                                  num_curves = 2,
                                  x_start = 0,
                                  x_end = 10,
                                  x_increment = 1,
                                  y_start = 0,
                                  y_end = 1,
                                  y_increment = 0.25,
                                  y_text_vertical =  T,
                                  censoring = F)
```

This R package was developed with the support of CADTH. 
