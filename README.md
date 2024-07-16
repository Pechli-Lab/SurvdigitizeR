# SurvdigitizeR

<!-- badges: start -->
<!-- badges: end -->

SurvdigitizeR is an R package that automates the extraction of survival probabilities from Kaplan-Meier (KM) curves in JPEG or PNG format. It simplifies the process of digitizing KM curves, which is typically time-consuming and prone to error when done manually. This package provides an efficient and accurate way to digitize KM curves.

* **Link to the paper for a detailed user guide:** [SurvdigitizeR: an algorithm for automated survival curve digitization](https://doi.org/10.1186/s12874-024-02273-8)

* **Link to the R Shiny Application:** https://pechlilab.shinyapps.io/SurvdigitizeR/



## Installation

You can install the released version of SurvdigitizeR from github.

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
library(SurvdigitizeR)
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




## Publication Information and How to Cite Us

We appreciate your interest in SurvdigitizeR. If you use SurvdigitizeR in your research, please cite our paper to help others find and benefit from our work.

### Authors: 

Jasper Zhongyuan Zhang, Juan David Rios, Tilemanchos Pechlivanoglou, Alan Yang, Qiyue Zhang, Dimitrios Deris, Ian Cromwell & Petros Pechlivanoglou

### Link to the paper
[SurvdigitizeR: an algorithm for automated survival curve digitization](https://doi.org/10.1186/s12874-024-02273-8)

### Citation:

Jasper Zhongyuan Zhang, Juan David Rios, Tilemanchos Pechlivanoglou, Alan Yang, Qiyue Zhang, Dimitrios Deris, Ian Cromwell & Petros Pechlivanoglou. "SurvdigitizeR: an algorithm for automated survival curve digitization". BMC Medical Research Methodology, volume 24, Article number: 147 (2024). https://doi.org/10.1186/s12874-024-02273-8

### Bibtex

For those who use **BibTeX** to manage their references, here is the citation in BibTeX format:

```bibtex
@article{zhang2024survdigitizer,
  title={SurvdigitizeR: an algorithm for automated survival curve digitization},
  author={Zhang, Jasper Zhongyuan and Rios, Juan David and Pechlivanoglou, Tilemanchos and Yang, Alan and Zhang, Qiyue and Deris, Dimitrios and Cromwell, Ian and Pechlivanoglou, Petros},
  journal={BMC Medical Research Methodology},
  volume={24},
  number={1},
  pages={147},
  year={2024},
  publisher={BioMed Central},
  doi={10.1186/s12874-024-02273-8}
}
```

