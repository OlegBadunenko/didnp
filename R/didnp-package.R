# didnp package doc

# https://stackoverflow.com/questions/7198758/roxygen2-how-to-properly-document-s3-methods
# roxygen2::roxygenize()
# https://pkgdown.r-lib.org/
# Run once to configure your package to use pkgdown
# usethis::use_pkgdown()
# pkgdown::build_site()

# didnp package overview
# Models: -
# Data: repeated cross-sectional data & longitudinal data

#' didnp: A package for computing both heterogenous and average treatment effects for the treated in a model-free differences-in-differences framework.
#'
#' The \pkg{didnp} package provides simple ways
#'
#' @section didnpreg: \code{\link{didnpreg}}
#'
#' @name didnp-package
#' @useDynLib didnp
#'
#' @aliases didnp-package
#'
#' @docType package
#'
#' @section Bugreport: Any bug or suggestion can be reported using the
#' \code{didnp} tracker facilities at: \url{https://github.com/}
#'
#' @author
#' Oleg Badunenko \email{oleg.badunenko@@brunel.ac.uk},
#' Daniel J. Henderson \email{djhender@@cba.ua.edu},
#' Stefan Sperlich \email{stefan.sperlich@@unige.ch}
#'
#' @importFrom minqa bobyqa
#' @importFrom Formula Formula
#' @importFrom Formula model.part
#' @importFrom ggplot2 ggplot
#' @import doParallel
#' @import parallel
#' @import foreach
NULL
