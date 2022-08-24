
<!-- README.md is generated from README.Rmd. Please edit that file -->

# declared

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/declared)](https://CRAN.R-project.org/package=declared)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/dusadrian/declared?branch=main&svg=true)](https://ci.appveyor.com/project/dusadrian/declared)
[![Codecov test
coverage](https://codecov.io/gh/dusadrian/declared/branch/main/graph/badge.svg)](https://app.codecov.io/gh/dusadrian/declared?branch=main)
<!-- badges: end -->

The goal of `declared` is to improve the functionality of imported
social science microdata, particularly labelled data. While there are
excellent packages available for these purposes, such as
[haven](https://haven.tidyverse.org/) and
[labelled](http://larmarange.github.io/labelled/), they have some
fundamental design features that run, in some situations, against the
user’s expectations. This has a lot to do with the treatment of declared
missing values, that are instrumental for the social sciences. The aim
of `declared` is to offer an alternative class, `declared()`, whilst
ensuring as much compatibility as possible with these packages popular
packages.

## Installation

You can install the development version of declared like so:

``` r
install.packages("declared", repos = "dusadrian.r-universe.dev")
```

## Example

``` r
library(haven)
x1 <- labelled_spss(
  x = c(1:5, -91),
  labels = c("Missing" = -91),
  na_value = -91
)

print(x1)
#> <labelled_spss<double>[6]>
#> [1]   1   2   3   4   5 -91
#> Missing values: -91
#> 
#> Labels:
#>  value   label
#>    -91 Missing

mean(x1)
#> [1] -12.66667
```

Instead of using the `labelled::labelled()` class or its inherited
version in `haven`, the `declared` package offers a similar class that
behaves more as it is expected–because it interprets certain “missing”
values `NA` codes as existing, declared missing values.

``` r
library(declared)
x2 <- declared(
  x = c(1:5, -91),
  labels = c("Missing" = -91),
  na_value = -91
)

print(x2)
#> <declared<integer>[6]>
#> [1]       1       2       3       4       5 NA(-91)
#> Missing values: -91
#> 
#> Labels:
#>  value   label
#>    -91 Missing

mean(x2)
#> [1] 3
```
