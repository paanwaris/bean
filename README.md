
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bean

<!-- badges: start -->

[![R-CMD-check](https://github.com/paanwaris/bean/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/paanwaris/bean/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of bean is to …

## Installation

You can install the development version of bean from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("paanwaris/bean")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo paanwaris/bean@HEAD
#> Warning in untar2(tarfile, files, list, exdir, restore_times): skipping pax
#> global extended headers
#> Warning in untar2(tarfile, files, list, exdir, restore_times): skipping pax
#> global extended headers
#> 
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>       ✔  checking for file 'C:\Users\paanw\AppData\Local\Temp\RtmpW6tOn1\remotesff861ec4d0\paanwaris-bean-56264d1/DESCRIPTION'
#>   ─  preparing 'bean':
#>    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#> ─  building 'bean_0.1.0.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/paanw/AppData/Local/R/cache/R/renv/library/bean-77808901/windows/R-4.4/x86_64-w64-mingw32'
#> (as 'lib' is unspecified)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(bean)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
