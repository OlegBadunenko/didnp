
<!-- README.md is generated from README.Rmd. Please edit that file -->

# A model-free differences-in-differences framework

<!-- badges: start -->

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
effect parameters in a Difference-in-Differences setup allowing for

- this
- that

The main parameters are **that**. These are

## Getting Started

The **didnp** package implements the framework developed in

- 

## Installation

You can install **didnp** from CRAN with:

``` r
install.packages("didnp")
```

or get the latest version from github with:

``` r
if ( !require("devtools") ) install.packages("devtools"); library(devtools)
devtools::install_github("OlegBadunenko/didnp")
```

## An example

The following is a simplified example …, which comes from
[that](https://elsevier.com/).

A subset of the data is available in the package and can be loaded by

``` r
  library(didnp)
  data(DACAsub, package = "didnp")
```

The dataset contains XXX observations

- **LHS** lhs variable

To estimate verage treatment effects, use the **didnpreg** function

``` r
tym1 <- didnpreg(
  form1,
  data = dat2,
  subset = mysmpl,
  bwmethod = "opt",
  boot.num = 399,
  TTb = TRUE,
  print.level = 2,
  cores = 32)
```

**didnpreg** returns a class **didnp** object. This object contains
estimates of the average treatment effects and their standard errors. To
see these, we can call the **summary** function

``` r
summary(tym1)
```

This provides estimates of

It is often also convenient to plot the group-time average treatment
effects. This can be done using the **didnpplot** command:

``` r
didnpplot(tym1, ylim = c(-.25,.1))
```

The … in the plot are

## Additional Resources

That
