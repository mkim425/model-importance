
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Determination of model importance as an ensemble member

<!-- badges: start -->
<!-- badges: end -->

This is the data repository for the following paper:

*Title: Beyond forecast leaderboards: Measuring individual model
importance based on contribution to ensemble accuracy*

*Authors: Minsu Kim, Evan L. Ray, Nicholas G. Reich*

## Computational environment

We use conda for managing a Python environment. If you’re just getting
started, you can use the following commands to create your environment.

Enter this command in your terminal to create a new conda environment
for the project:

    conda create -n importance python=3.11

Enter these commands in an R session:

``` r
library(reticulate)
library(qens)
reticulate::use_condaenv("importance")
qens::install_py_dependencies()
```

Whenever you’re using this package, you’ll need to tell reticulate to
use the conda environment you created:

``` r
library(reticulate)
reticulate::use_condaenv("importance")
```
