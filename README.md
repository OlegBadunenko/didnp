
<!-- README.md is generated from README.Rmd. Please edit that file -->

# A model-free differences-in-differences framework

[![](http://cranlogs.r-pkg.org/badges/grand-total/npsf?color=blue)](https://cran.r-project.org/package=npsf)
[![](http://cranlogs.r-pkg.org/badges/last-month/npsf?color=blue)](https://cran.r-project.org/package=npsf)
[![](https://www.r-pkg.org/badges/version/npsf?color=blue)](https://cran.r-project.org/package=npsf)
[![](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/OlegBadunenko/didnp)
[![CRAN
checks](https://badges.cranchecks.info/summary/npsf.svg)](https://cran.r-project.org/web/checks/check_results_npsf.html)
[![](https://codecov.io/gh/OlegBadunenko/did/branch/main/graph/badge.svg)](https://codecov.io/gh/OlegBadunenko/did)
[![](https://img.shields.io/github/last-commit/OlegBadunenko/didnp.svg)](https://github.com/OlegBadunenko/didnp/commits/main)

<!-- README.md is generated from README.Rmd. Please edit that file -->

The **didnp** package contains tools for computing average treatment
effect parameters in a Difference-in-Differences setup allowing for

- More than two time periods

The main parameters are **group-time average treatment effects**. These
are

Group-time average treatment effects are

## Getting Started

The **didnp** package implements the framework put forward in

- 

## Installation

You can install **didnp** from CRAN with:

``` r
install.packages("didnp")
```

or get the latest version from github with:

``` r
# install.packages("devtools")
devtools::install_github("OlegBadunenko/didnp")
```

## A short example

The following is a simplified example of the effect of states increasing
their minimum wages on county-level teen employment rates which comes
from [that](https://elsevier.com/).

A subset of the data is available in the package and can be loaded by

``` r
  library(didnp)
  data(qqqqqqqq)
```

The dataset contains 500 observations

- **lemp** This is the log of county-level teen employment. It is the
  outcome variable

- **first.treat**

To estimate group-time average treatment effects, use the **didnpreg**
function

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

The red dots in the plot are

**Event Studies**

Although

## Additional Resources

We have provided
