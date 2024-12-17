#' Subset of the dataset aboutthe Deferred Action for Childhood Arrivals (DACA) program
#'
#' A dataset about Deferred Action for Childhood Arrivals (DACA) program containing 330106 observations
#'
#' @format A data frame with 330106 rows and 18 columns:
#' \describe{
#'   \item{inschool}{High school attendance (In School)}
#'   \item{hs}{High school graduation (HS Degree)}
#'   \item{scol}{Some College}
#'   \item{post}{Post treatment period: a dummy variable that equals 1 on or after 2012}
#'   \item{elig}{Eligible for treatment: is equal to one for individuals in the sample who arrived in the US by age 10 and by year 2007, and who are currently not citizens.}
#'   \item{fem}{Female}
#'   \item{race}{Race}
#'   \item{var.bpl}{Birth region (Birthplace)}
#'   \item{state}{State}
#'   \item{year}{Year}
#'   \item{age}{Age}
#'   \item{yrimmig}{Year of immigration}
#'   \item{ageimmig}{Age of immigration}
#'   \item{a1418}{Equals 1 if age between 14 and 18}
#'   \item{a1922}{Equals 1 if age between 19 and 22}
#'   \item{a2330}{Equals 1 if age between 23 and 30}
#'   \item{htus}{High take-up}
#'   \item{perwt}{Person weight}
#' }
#' @source \url{https://doi.org/10.1257/pol.20180352}.
"DACAsub"

#' Two Subset of the dataset about the Deferred Action for Childhood Arrivals (DACA) program
#'
#' Two A dataset about Deferred Action for Childhood Arrivals (DACA) program containing 330106 observations
#'
#' @format A data frame with 17379 rows and 11 columns:
#' \describe{
#'   \item{FT}{2}
#'   \item{unemp}{2}
#'   \item{sex}{Female}
#'   \item{statefip}{State}
#'   \item{school}{Post treatment period: a dummy variable that equals 1 on or after 2012}
#'   \item{age}{Age}
#'   \item{yrimmig}{Year of immigration}
#'   \item{educ}{Years of education}
#'   \item{period}{1-8}
#'   \item{treatment_period}{Birth region (Birthplace)}
#'   \item{eligible}{Eligible for treatment}
#' }
"Unemp"
