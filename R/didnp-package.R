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
#'
#' @aliases didnp-package didnp
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
#' @importFrom progressr progressor with_progress
#' @importFrom parallelly availableCores
#' @importFrom future plan multisession sequential
#' @importFrom dplyr as_tibble
#' @importFrom minqa bobyqa
#' @importFrom np npksum
#' @importFrom Formula Formula
#' @importFrom Formula model.part
#' @import doParallel
#' @import parallel
#' @import foreach
# @importFrom colMaxs colMins colsums
# @importFrom katex math_to_rd
NULL
