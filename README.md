
<!-- README.md is generated from README.Rmd. Please edit that file -->

# A model-free differences-in-differences framework

<!-- badges: start -->

    #R>  Warning: `git_branch_default()` was deprecated in usethis 2.1.0.
    #R>  ℹ Please use `git_default_branch()` instead.
    #R>  ℹ The deprecated feature was likely used in the badger package.
    #R>    Please report the issue at <https://github.com/GuangchuangYu/badger/issues>.
    #R>  This warning is displayed once every 8 hours.
    #R>  Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    #R>  generated.

[![](http://cranlogs.r-pkg.org/badges/grand-total/npsf?color=blue)](https://cran.r-project.org/package=npsf)
[![](http://cranlogs.r-pkg.org/badges/last-month/npsf?color=yellow)](https://cran.r-project.org/package=npsf)
[![](https://www.r-pkg.org/badges/version/npsf?color=green)](https://cran.r-project.org/package=npsf)
[![](https://img.shields.io/badge/devel%20version-1.0.0-red.svg)](https://github.com/OlegBadunenko/didnp)
[![CRAN
checks](https://badges.cranchecks.info/summary/npsf.svg)](https://cran.r-project.org/web/checks/check_results_npsf.html)
[![](https://img.shields.io/github/last-commit/OlegBadunenko/didnp.svg)](https://github.com/OlegBadunenko/didnp/commits/main)

<!-- badges: end -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

The **didnp** package contains tools for computing average treatment
effect parameters in a Difference-in-Differences setup without
specifying a model.

## The framework

The **didnp** package implements the framework developed in

Daniel J. Henderson and Stefan Sperlich (2023), “A Complete Framework
for Model-Free Difference-in-Differences Estimation”, Foundations and
Trends® in Econometrics: Vol. 12: No. 3, pp 232-323.
<http://dx.doi.org/10.1561/0800000046>

## Installation

The package **didnp** can be install from CRAN by typing:

``` r
install.packages("didnp", dependencies = TRUE)
```

or get the latest version from github by typing:

``` r
if ( !require("devtools") ) install.packages("devtools"); library(devtools)
devtools::install_github("OlegBadunenko/didnp")
```

## Illustration and Uses

This [article](https://olegbadunenko.github.io/didnp/illustration.html) guides through the code and illustrates the functionality of the package using 


> The subset of the data from [IPUMS](https://ipums.org)  available in the package.
