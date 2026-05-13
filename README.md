
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bean 🫛

Paanwaris Paansri and Luis E. Escobar
<img src="man/figures/logo.png" align="right" height="150" alt="Hexagon sticker for the bean package featuring a green bean pod in environmental space" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/paanwaris/bean/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/paanwaris/bean/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

## Ecological Motivation

The `bean` package provides a tool to address a fundamental challenge in
species distribution modeling (SDM, or ecological niche modeling, ENM):
**sampling bias**. Occurrence records for species are rarely collected
through a systematic, stratified process. Instead, they often cluster in
easily accessible areas (like roads and cities) or in well-studied
research sites. This spatial bias can translate into an **environmental
bias**, where the model incorrectly learns that the species is
associated with the environmental conditions of those heavily sampled
areas, rather than its true ecological requirements.

`bean` tackles this problem by thinning occurrence data in
**environmental space**. The goal is to create a more uniform
distribution of points across the species’ observed environmental niche,
reducing the influence of densely clustered records. This allows for the
construction of a more accurate **fundamental niche** volume, which can
then be projected into geographic space to create a less biased
prediction of area with environmental suitability.

The name `bean` reflects the core principle of the method: ensuring that
each “pod” (a grid cell in environmental space) contains only a
specified number of “beans” (occurrence points).

## Package Description

**bean** operates by shifting the focus from geographic space to
environmental space:

- **Environmental Gridding**: Divides the environmental hypercube into
  “pods”.

- **Objective Thinning**: Reduces clusters to a specified density per
  pod.

- **Niche Delineation**: Fits ellipsoids to thinned data to define the
  fundamental niche.

- **Projection**: Maps the corrected niche back into geographic space
  for less biased predictions.

## Installing the Package

The development version of `bean` can be installed from GitHub:

``` r
# Install devtools if needed
if (!require("devtools")) install.packages("devtools")

# Install bean
devtools::install_github("paanwaris/bean")
```

To load the package:

``` r
library(bean)
```

## The bean Protocol: Step-by-Step

A typical `bean` workflow consists of these key steps:

### 1. Data Preparation

The `prepare_bean()` function cleans raw occurrence data by removing
missing coordinates and extracting environmental values from raster
layers. This ensures all subsequent analyses use a clean, scaled
dataset.

### 2. Objective Grid Resolution

Instead of arbitrary thinning, `find_env_resolution()` uses a geometric
**“elbow” method** based on nearest-neighbor distances in E-space. This
identifies the exact distance where dense artificial clustering
transitions into natural data spacing.

### 3. Apply Thinning

`bean` offers two core thinning methods:

- Stochastic (`thin_env_nd`): Randomly samples one “bean” from each
  occupied “pod”.

- Deterministic (`thin_env_center`): Generates a new point at the exact
  center of every occupied grid cell.

### 4. Niche Delineation

The `fit_ellipsoid()` function formalizes the environmental niche by
fitting a bivariate or multivariate ellipse around the thinned points.

### 5. Prediction and Mapping

Using the learned niche, `predict()` projects the results back to
geographic space. This step emphasizes that algorithms like **MaxEnt**
are used to calculate suitability scores from the delineated niche
boundaries.

## Checking the Vignettes

For full demonstrations of the protocol, check the package vignettes:

``` r
# Data Preparation & Visualization
vignette("data-preparation")
#> Warning: vignette 'data-preparation' not found

# Objective Thinning in Environmental Space
vignette("environmental-thinning")
#> Warning: vignette 'environmental-thinning' not found

# Niche Delineation & Suitability Mapping
vignette("niche-modeling")
#> Warning: vignette 'niche-modeling' not found
```

The End ❤️
