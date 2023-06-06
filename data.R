#' @title DACA
#'
#' @description A dataset containing (the log of) teen employment in 500 counties
#'  in the U.S. from 2004 to 2007.  This is a subset of the dataset used in Callaway and
#'  Sant'Anna (2021).  See that paper for additional descriptions.
#'
#' @format A data frame with 330106 rows and 20 columns:
#' \describe{
#'   \item{inschool}{the year of the observation}
#'   \item{hs}{a unique identifier for a particular county}
#'   \item{scol}{the log of 1000s of population for the county}
#'   \item{post}{the log of teen employment in the county}
#'   \item{elig}{the year that the state where the county is located
#'    raised its minimum wage, it is set equal to 0 for counties that have
#'    minimum wages equal to the federal minimum wage over the entire
#'    period.}
#'   \item{fem}{whether or not a particular county is treated in that year}
#'   \item{race}{whether or not a particular county is treated in that year}
#'   \item{var.bpl}{whether or not a particular county is treated in that year}
#'   \item{state}{whether or not a particular county is treated in that year}
#'   \item{year}{whether or not a particular county is treated in that year}
#'   \item{age}{whether or not a particular county is treated in that year}
#'   \item{yrimmig}{whether or not a particular county is treated in that year}
#'   \item{ageimmig}{whether or not a particular county is treated in that year}
#'   \item{a1418}{whether or not a particular county is treated in that year}
#'   \item{a1922}{whether or not a particular county is treated in that year}
#'   \item{a2330}{whether or not a particular county is treated in that year}
#'   \item{htus}{whether or not a particular county is treated in that year}
#'   \item{perwt}{whether or not a particular county is treated in that year}
#'   \item{treatment_period}{whether or not a particular county is treated in that year}
#'   \item{mysmple}{a1922==1 & !is.na(a1922) & htus==1 & !is.na(htus)}
#' }
#' @source Henderson and Sperlich (2023)
"DACAsub1"
