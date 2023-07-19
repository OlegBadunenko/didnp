# R functions for the didnp package

# https://stackoverflow.com/questions/7198758/roxygen2-how-to-properly-document-s3-methods
# roxygen2::roxygenize()
# https://pkgdown.r-lib.org/
# Run once to configure your package to use pkgdown
# usethis::use_pkgdown()
# pkgdown::build_site()

# Theme: https://pkgdown.r-lib.org/articles/customise.html
# bootswatch: https://bootswatch.com/


#' Model-free Treatment Effect Estimators
#'
#' The \code{didnpbsctest} command contains tools for computing both heterogenous and average treatment effects for the treated in a model-free differences-in-differences framework.
#'
#' @param formula an object of class formula (or one that can be coerced to that class): a symbolic description of the model. The details of model specification are given under `Details'
#' @param data name of the data frame; must be specified if the 'formula' method is used
#' @param subset NULL, optional subsample of 'data'
#' @param outcome a vector, matrix, or data frame of length \eqn{NT}. The outcome can be a continuous or dummy variable.
#' @param regressors a data frame with \eqn{NT} rows that contains regressors.
#' A data frame class is required to identify the type/class of each regressor.
#' @param id a vector, matrix, or data frame of length \eqn{NT} that identifies the unit of observation.
#' @param time a vector, matrix, or data frame of length \eqn{NT} that specifies in which period \code{id} is observed.
#' @param treatment a vector, matrix, or data frame of length \eqn{NT} with zeros for the control and ones for the treated observations.
#' @param treatment_period a vector, matrix, or data frame of length \eqn{NT} with zeros for the period before treatment and ones for the period of treatment and after.
#' @param weights NULL,
#' @param boot.num 399,
#' @param print.level the level of printing; larger number implies more output is printed. Default is 1. 0 suppresses all printing.
#' @param cores Integer specifies the number of cores to be used for parallel computation.
#' @param seed integer used for the random number generation for the replication purposes. Default is 17345168.
#'
#' @details
#' The formula shell contain multiple parts separated by '|'. An example is
#'
#' form1 <- y ~ x1 + x2 | id | time | treatment | treatment_period | weights
#'
#' weights can be omitted if not available
#'
#' form1 <- y ~ x1 + x2 | id | time | treatment | treatment_period
#'
#'
#' @return \code{didnpbsctest} returns a list containing:
#' \tabular{ll}{
#'    \code{NT}
#'    \tab Total number of observations
#'    \cr \tab \cr
#'    \code{esample}
#'    \tab A vector of TRUE/FALSE values identifying observations used in estimation. Relevant for the 'formula' method but complete cases will also be checked in the matrix method
#'    \cr \tab \cr
#'    \code{sample1}
#'    \tab A vector of TRUE/FALSE values identifying treated observations.
#'    \cr \tab \cr
#'    \code{sample11}
#'    \tab A vector of TRUE/FALSE values identifying treated observations right after the treatement
#'    \cr \tab \cr
#'    \code{sample10}
#'    \tab A vector of TRUE/FALSE values identifying treated observations just before the treatment
#'    \cr \tab \cr
#'    \code{sample01}
#'    \tab A vector of TRUE/FALSE values identifying observations in control group right after the treatement
#'    \cr \tab \cr
#'    \code{sample00}
#'    \tab A vector of TRUE/FALSE values identifying observations in control group just before the treatement
#'    \cr \tab \cr
#'    \code{regressor.type}
#'    \tab A vector of length 3 with number of continuous, unordered categorical, and ordered categorical regressors.
#'    \cr \tab \cr
#'    \code{bwmethod}
#'    \tab bandwidth type
#'    \cr \tab \cr
#'    \code{bw.time}
#'    \tab Time in seconds it took to calculate bandwidths. For bandwidth type "opt" is 0.
#'    \cr \tab \cr
#'    \code{bws}
#'    \tab Data frame with variable names, type of the regressor and bandwidths.
#'    \cr \tab \cr
#'    \code{boot.time}
#'    \tab Time in seconds it took to bootstrap the standard errors.
#'    \cr \tab \cr
#'    \code{boot.num}
#'    \tab Number of bootstrap replications.
#'    \cr \tab \cr
#'    \code{bw11}
#'    \tab Bandwidths calculated for the sample of treated right after the treatment.
#'    \cr \tab \cr
#'    \code{bw10}
#'    \tab Bandwidths calculated for the sample of treated just before the treatment.
#'    \cr \tab \cr
#'    \code{bw01}
#'    \tab Bandwidths calculated for the sample of of observations in control group right after the treatment.
#'    \cr \tab \cr
#'    \code{bw00}
#'    \tab Bandwidths calculated for the sample of observations in control group just before the treatement
#'    \cr \tab \cr
#'    \code{do.TTb}
#'    \tab TRUE/FALSE whether to perform TTb
#'    \cr \tab \cr
#'    \code{TTa.positions.in.TTb}
#'    \tab Positions of TTa observations in TTb. Only if \code{do.TTb}
#'    \cr \tab \cr
#'    \code{TTa}
#'    \tab the DiD estimator of the avarage unconditional TT
#'    \cr \tab \cr
#'    \code{TTa.i}
#'    \tab the DiD estimators of the unconditional TT
#'    \cr \tab \cr
#'    \code{TTb}
#'    \tab the DiD estimator of the avarage unconditional TT
#'    \cr \tab \cr
#'    \code{TTb.i}
#'    \tab the DiD estimators of the unconditional TT
#'    \cr \tab \cr
#'    \code{TTa.sd}
#'    \tab the standard error of the DiD estimator of the avarage unconditional TT
#'    \cr \tab \cr
#'    \code{TTb.sd}
#'    \tab the standard error of the DiD estimator of the avarage unconditional TT
#'    \cr \tab \cr
#'    \code{TTx}
#'    \tab the DiD estimators of the conditional TT (also known as CATET)
#'    \cr \tab \cr
#'    \code{TTa.i.boot}
#'    \tab Matrix of the size \eqn{n_{11} \times boot.num}
#'    \cr \tab \cr
#'    \code{TTb.i.boot}
#'    \tab Matrix of the size \eqn{n_{1} \times boot.num}
#'    \cr
#' }
#'
#'
#' @keywords did np
#'
#' @examples
#' \dontrun{
#'   data(DACAsub, package = "didnp")
#'   # will get a data frame 'DACAsub' with 330106 rows and 18 columns
#'
#'   # get the subsample
#'   DACAsub$mysmpl <- mysmpl <-
#'     DACAsub$a1922==1 & !is.na(DACAsub$a1922) &
#'     DACAsub$htus==1 & !is.na(DACAsub$htus)
#'
#'   # generate 'treatment_period'
#'   DACAsub$treatment_period <- ifelse(DACAsub[,"year"]>2011,1,0)
#'
#'   # define formula with the weight
#'   form1 <- inschool ~ fem + race + var.bpl + state + age + yrimmig +
#'     ageimmig | inschool | year | elig | treatment_period | perwt
#'
#'   # or without the weight
#'   form11 <- inschool ~ fem + race + var.bpl + state + age + yrimmig +
#'     ageimmig | inschool | year | elig | treatment_period
#'
#'   ## Syntax using formula
#'   # suppress output
#'   tym1a <- didnpbsctest(
#'     form1,
#'     data = DACAsub,
#'     subset = mysmpl,
#'     bwmethod = "opt",
#'     boot.num = 399,
#'     TTb = FALSE,
#'     print.level = 0,
#'     cores = 4)
#'
#'   # Print the summary
#'   summary(tym1a)
#'
#'   ## Use CV bandwidths
#'   tym1aCV <- didnpbsctest(
#'     form1,
#'     data = DACAsub,
#'     subset = mysmpl,
#'     bwmethod = "CV",
#'     boot.num = 399,
#'     TTb = FALSE,
#'     print.level = 1,
#'     cores = 4)
#'
#'   # Print the summary
#'   summary(tym1aCV)
#'
#'   ## Calculate also TTb (will take longer)
#'   tym1bCV <- didnpbsctest(
#'     form1,
#'     data = DACAsub,
#'     subset = mysmpl,
#'     bwmethod = "CV",
#'     boot.num = 399,
#'     TTb = TRUE,
#'     print.level = 1,
#'     cores = 4)
#'
#'   # Print the summary
#'   summary(tym1bCV)
#'
#'   ## Syntax using matrices
#'
#'   tym1aM <- didnpbsctest(
#'     outcome = DACAsub[mysmpl,"inschool"],
#'     regressors = DACAsub[mysmpl,c("fem", "race", "var.bpl", "state", "age", "yrimmig", "ageimmig")],
#'     id = DACAsub[mysmpl,"inschool"],
#'     time = DACAsub[mysmpl,"year"],
#'     treatment = DACAsub[mysmpl,"elig"],
#'     treatment_period = ifelse(DACAsub[mysmpl,"year"]>2011,1,0),
#'     weights = DACAsub[mysmpl,"perwt"],
#'     bwmethod = "opt",
#'     boot.num = 399,
#'     TTb = FALSE,
#'     print.level = 1,
#'     cores = 4)
#'
#'   # Print the summary
#'   summary(tym1aM)
#'
#' }
#'
#' @references
#' ... (...). This. \emph{Journal of },
#' \bold{1}(1), 1-1
#' \url{https://doi.org/10.}
#'
#' @author
#' Oleg Badunenko \email{oleg.badunenko@@brunel.ac.uk},
#'
#' Daniel J. Henderson \email{djhender@@cba.ua.edu},
#'
#' Stefan Sperlich \email{stefan.sperlich@@unige.ch}
#'
#'
#'
# https://stackoverflow.com/questions/7198758/roxygen2-how-to-properly-document-s3-methods
#' @rdname didnpbsctest
#' @export
didnpbsctest <- function(...){
  # args = list(...)
  # cat.print(args)
  # cat.print(names(args))
  # cat.print(class(args))
  # lapply(args, cat.print)
  UseMethod("didnpbsctest")
}

#' @rdname didnpbsctest
#' @method didnpbsctest formula
#' @export
didnpbsctest.formula <- function(
    formula,
    data  = stop("argument 'data' is missing"),
    subset,
    boot.num = 399,
    print.level = 1,
    digits = 4,
    cores = 1,
    seed = 17345168,
    ...)
{
  # cat("'formula': this is to get the matrices from formula and feed to 'didnpbsctest.default'\n")

  # cat.print(bwmethod)

  # cat.print(boot.num)

  ## handle TTx ----

  # if ( !(TTx %in% c("TTa", "TTb")) ) stop("'TTx' other than TTa or TTb not implemented yet")

  # if (TTx == "TTa") {
  #   TTb = FALSE
  # } else {
  #   TTb = TRUE
  # }

  form1 <- Formula::Formula(formula)
  # cat.print(form1)

  mf0 <- match.call(expand.dots = FALSE)

  # cat("01\n")

  data.order <- as.numeric(rownames(data)) # seq_len(nrow(data))

  # cat("2\n")

  mf <- mf0
  mf$formula <- form1 #formula( form )
  # cat("05\n")
  m <- match(c("formula", "data", "subset"), names(mf), 0L)
  # cat("06\n")
  # cat.print(m)
  mf <- mf[c(1L, m)]
  # cat("07\n")
  # cat.print(mf)
  mf[[1L]] <- as.name("model.frame")
  # cat("08\n")
  # cat.print(mf)
  # cat.print(needed.frame)
  # mf <- eval(mf, sys.frame(sys.parent(n = needed.frame+2)))
  mf <- eval(mf, parent.frame())
  # cat.print(mf)
  # cat("09\n")
  # cat.print(mf)
  mt <- attr(mf, "terms")
  # x <- as.matrix(model.matrix(mt, mf))
  # cat("10\n")
  # cat.print(mt)
  X <- model.frame(mt, mf)
  # cat("11\n")
  # cat.print(X)
  esample.nu <- as.numeric(rownames(X))
  esample <- data.order %in% esample.nu

  # cat("12\n")

  # cat.print(length(form1))
  # cat.print(length(form1)[2])

  if (length(form1)[2] < 5) stop("specificaions are inappropriate")

  # Y <- model.matrix(form1, data = mf, lhs = 1)
  Y <- Formula::model.part(form1, data = mf, lhs = 1, drop = TRUE)
  nt <- length(Y)
  # cat("13\n")
  # print(length(Y))
  # cat.print(class(Y))
  # cat.print(head(Y))
  # cat.print(Y[1:10])
  Formula::model.part(form1, lhs = 0, rhs = 1, data = mf) -> X
  # cat.print(dim(X))
  # cat.print(head(X))
  # cat.print(class(X))
  model.matrix(form1, lhs = 0, rhs = 2, data = mf)[,-1] -> id
  if (length(id) != nt) stop("specificaion 'id' is inappropriate")
  # cat.print(class(id))
  # cat.print(head(id))
  model.matrix(form1, lhs = 0, rhs = 3, data = mf)[,-1] -> time
  if (length(time) != nt) stop("specificaion 'time' is inappropriate")
  # cat.print(class(time))
  # cat.print(head(time))
  model.matrix(form1, lhs = 0, rhs = 4, data = mf)[,-1] -> treatment
  if (length(treatment) != nt) stop("specificaion 'treatment' is inappropriate")
  # cat.print(class(treatment))
  # cat.print(head(treatment))
  model.matrix(form1, lhs = 0, rhs = 5, data = mf)[,-1] -> treatment_period
  if (length(treatment_period) != nt) stop("specificaion 'treatment_period' is inappropriate")
  # cat.print(class(treatment_period))
  # cat.print(head(treatment_period))
  # see if weights was specified
  if (length(form1)[2] == 5) {
    weights <-  rep(1, nt)
  } else {
    model.matrix(form1, lhs = 0, rhs = 6, data = mf)[,-1] -> weights
    if (length(weights) != nt) stop("specificaion 'weights' is inappropriate")
  }
  # cat.print(class(weights))
  # cat.print(head(weights))

  # cat("14\n")

  # tymch <- eval(parse(text=paste("
  # didnpbsctest( outcome=Y,
  #     regressors=X,
  #     id=id,
  #     time=time,
  #     treatment=treatment,
  #     treatment_period=treatment_period,
  #     weights = weights,
  #     bwmethod = bwmethod,
  #     boot.num = boot.num,
  #     TTaOnly = TTaOnly,
  #     print.level = print.level,
  #     ...)
  #  ")))

  tymch <- didnpbsctest(
    outcome=Y,
    regressors=X,
    id=id,
    time=time,
    treatment=treatment,
    treatment_period=treatment_period,
    weights = weights,
    boot.num = boot.num,
    print.level = print.level,
    digits = digits,
    cores = cores,
    seed = seed,
    ...)

  tymch$esample <- esample
  # cat("15\n")


  class(tymch) <- c("didnpbsctest", "didnp")
  return(tymch)

}

#' @rdname didnpbsctest
#' @method didnpbsctest default
#' @export
didnpbsctest.default <- function(
    outcome,
    regressors,
    id,
    time,
    treatment,
    treatment_period,
    weights = NULL,
    boot.num = 399,
    print.level = 1,
    digits = 4,
    cores = 1,
    seed = 17345168,
    ...)
{
  # cat("'default': this is the workhorse\n")
  # c(class(outcome), class(regressors), class(id), class(time), class(treatment), class(treatment_period))
  ## check specifications ----
  if (missing(outcome)) stop("vector 'outcome' is missing")
  if ( !(class(outcome) %in% c("numeric")) ) stop("wrong class of 'outcome': must be 'numeric'")

  if (missing(regressors)) stop("data.frame 'regressors' is missing")
  if ( !(class(regressors) %in% c("data.frame")) ) stop("wrong class of 'regressors': must be 'data.frame'")

  if (missing(id)) stop("vector 'id' is missing")

  if (missing(time)) stop("vector 'time' is missing")
  if ( !(class(time) %in% c("numeric")) ) stop("wrong class of 'time': must be 'numeric'")

  if (missing(treatment)) stop("vector 'treatment' is missing")
  if ( !(class(treatment) %in% c("numeric")) ) stop("wrong class of 'treatment': must be 'numeric'")
  table(treatment) -> treatment_values
  if (length(treatment_values) != 2 | names(treatment_values)[1] != "0" | names(treatment_values)[2] != "1") stop("vector 'treatment' must have exactly 2 values, 0 and 1")

  if (missing(treatment_period)) stop("vector 'treatment_period' is missing")
  if ( !(class(treatment_period) %in% c("numeric")) ) stop("wrong class of 'treatment_period': must be 'numeric'")
  table(treatment_period) -> treatment_period_values
  if (length(treatment_period_values) != 2 | names(treatment_period_values)[1] != "0" | names(treatment_period_values)[2] != "1") stop("vector 'treatment_period' must have exactly 2 values, 0 and 1")

  # if ( !(bwmethod %in% c("opt", "CV")) ) stop("'bwmethod' can be either 'opt' or 'CV'")

  # ## handle TTx ----
  #
  # if ( !(TTx %in% c("TTa", "TTb")) ) stop("'TTx' other than TTa or TTb not implemented yet")
  #
  # if (TTx == "TTa") {
  #   TTb <- FALSE
  # } else {
  #   TTb <- TRUE
  # }
  # do.TTb <- TTb


  ## check dimensions ----

  nt.o <- length(outcome)
  nt.x <- nrow(regressors)
  nt.id <- length(id)
  nt.time <- length(time)
  nt.tr <- length(treatment)
  nt.tr.p <- length(treatment_period)
  if (is.null(weights)) {
    weights <- rep(1, n.o)
  }
  nt.w <- length(weights)

  if(nt.o != nt.x) stop("vector 'outcome' and data.frame 'regressors' have different number of observations")
  if(nt.o != nt.id) stop("vector 'outcome' and vector 'id' have different number of observations")
  if(nt.o != nt.time) stop("vector 'outcome' and vector 'time' have different number of observations")
  if(nt.o != nt.tr) stop("vector 'outcome' and vector 'treatment' have different number of observations")
  if(nt.o != nt.tr.p) stop("vector 'outcome' and vector 'treatment_period' have different number of observations")
  if(nt.o != nt.w) stop("vector 'outcome' and vector 'weights' have different number of observations")

  ## handle missing values ----

  d0 <-
    data.frame(
      weights,
      treatment_period,
      treatment,
      time,
      id,
      outcome,
      regressors
    )

  data.order <- as.numeric(rownames(d0))

  ## esample ----

  d0 <- na.omit(d0)
  esample.nu <- as.numeric(rownames(d0))
  esample <- data.order %in% esample.nu

  d0[,1] -> w
  d0[,2] -> treatment_period
  d0[,3] -> d # treatment
  d0[,4] -> it.time # time
  d0[,5] -> it.id # id
  d0[,6] -> y # outcome
  d0[ , -c(1:6), drop = FALSE] -> x # regressors
  k.x <- ncol(x)


  ## handle treatment ----

  time.treatment <- min( it.time[treatment_period == 1]  - 1)
  # cat.print(table(it.time))
  # cat.print(time.treatment)
  time.min <- min( it.time )
  time.max <- max( it.time )
  if( time.treatment - time.min < 2 ) warning (
    paste0("Data starts in ",time.min,", while treatment is in ",time.treatment)
  )
  if( time.max - time.treatment < 2 ) warning (
    paste0("Data ends in ",time.max,", while treatment is in ",time.treatment)
  )
  t <- it.time - time.treatment
  # cat.print(table(t))

  ## variable type ----

  q.type <- matrix(0,nrow=3,ncol=1)
  q.typeY <- character(k.x)
  q.typeYnum <- double(k.x)
  q.levels <- rep(1, k.x)

  for (i in 1:k.x){

    if (is.ordered(x[[i]])==TRUE){

      x[,i] <- droplevels(x[,i])

      q.type[3] <- q.type[3] + 1
      q.typeY[i] <- "ordered"
      q.typeYnum[i] <- 3
      q.levels[i] <- length( levels( x[[i]] ) ) - 1

    } else if (is.factor(x[[i]])==TRUE) {

      x[,i] <- droplevels(x[,i])

      q.type[2] <- q.type[2] + 1
      q.typeY[i] <- "factor"
      q.typeYnum[i] <- 2
      q.levels[i] <- length( levels( x[[i]] ) ) - 1

    } else {

      q.type[1] <- q.type[1] + 1
      q.typeY[i] <- "continuous"
      q.typeYnum[i] <- 1

      ## divide each continuous x by its standard deviation
      ## it is only a mean to an end
      ## it will not change the conclusion of the test
      x[[i]] <- as.vector(x[[i]]/sd(x[[i]]))

    }

  } ## i

  ## print info about the data ----

  if (print.level > 0) {
    cat("Number of Observations is ", nt.o, "\n")
  }

  ## print info about regressors ----

  if (print.level > 0) {

    if (q.type[1] > 0) {
      cat("Number of Continuous Regressors is             ", q.type[1], "\n")
    }

    if (q.type[1] > 0 & q.type[1] <= 3){
      cat("There are 3 or fewer continuous regressors\n")
    }
    if (q.type[1] > 3) {
      cat("There are more than 3 continuous regressors\n")
    }


    if (q.type[2] > 0) {
      cat("Number of Unordered Categorical Regressors is  ", q.type[2], "\n")
    }

    if (q.type[3] > 0) {
      cat("Number of Ordered Categorical Regressors is    ", q.type[3], "\n")
    }

    cat("\n")

  }

  ## separating the data by time and treatment status

  # subsamples
  smpl1  <- d == 1
  smpl11 <- d == 1 & t == 1
  smpl10 <- d == 1 & t == 0
  smpl01 <- d == 0 & t == 1
  smpl00 <- d == 0 & t == 0

  smpl1less <- d == 1 & t < 0
  smpl0less <- d == 0 & t < 0

  # pooled for the bootstrap
  smpl1pool <- d == 1 & t <= 0
  smpl0pool <- d == 0 & t <= 0

  # w11 <- w[which(d==1 & t==1)]
  w11 <- w[smpl11]
  # w10 <- w[which(d==1 & t==0)]
  w10 <- w[smpl10]
  w1less <-w[smpl1less]
  # w01 <- w[which(d==0 & t==1)]
  w01 <- w[smpl01]
  # w00 <- w[which(d==0 & t==0)]
  w00 <- w[smpl00]
  w0less <- w[smpl0less]

  # xx <- x
  xx10 <- x[smpl10,]
  xx00 <- x[smpl00,]
  # for (i in 1:k.x) {
  #   cat.print(i)
  #   cat.print(levels(xx00[,i]))
  # }
  xx1less <- x[smpl1less,]
  xx0less <- x[smpl0less,]
  xx1pool <- x[smpl1pool,]
  xx0pool <- x[smpl0pool,]

  xx11 <- x[smpl11,] # for TTa
  xx01 <- x[smpl01,]

  xx1 <- x[smpl1,]

  n11 <- sum(smpl11)
  n10 <- sum(smpl10)
  n01 <- sum(smpl01)
  n00 <- sum(smpl00)

  n1less <- sum(smpl1less)
  n0less <- sum(smpl0less)
  n1pool <- sum(smpl1pool)
  n0pool <- sum(smpl0pool)

  t1pool <- t[smpl1pool]
  t0pool <- t[smpl0pool]

  n1 <- nrow(xx1)

  y10 <- y[smpl10]
  y1less <- y[smpl1less]
  y00 <- y[smpl00]
  y0less <- y[smpl0less]

  y1pool <- y[smpl1pool]
  y0pool <- y[smpl0pool]

  ## with sample weights
  wy10 <- w10*y10
  wy1less <- w1less*y1less
  wy00 <- w00*y00
  wy0less <- w0less*y0less

  ## pooled for the bootstrap
  w1pool <- w[smpl1pool]
  w0pool <- w[smpl0pool]

  wy1pool <- y1pool*w1pool
  wy0pool <- y0pool*w0pool

#   if (TTb) {
#     # prepare to retrieve TTa from TTb
#     # init
#     tym1 <- as.numeric(rep(NA,nt.o))
#     # fill only subsample
#     tym1[smpl1] <- 1:n1
#     tym1[smpl11] -> TTa.positions.in.TTb
#   }


  ## plug-in bandwidths
  ## Silverman (1986) for cont (make it a function of q 1.06, 1.00, ....)
  ## Chu, Henderson and Parmeter (2015) for discrete
  ## bandwidths will be scaled by sample size for the other status (d=0, t=0)
  if (print.level > 0){
    cat("Bandwidths are chosen via the plug-in method\n")
  }

  rot.bw10 <- rot.bw00 <- rot.bw1less <- rot.bw0less <- rot.bw1pool <- rot.bw0pool <- rot.bw10 <-
    matrix(0, nrow = k.x, ncol = 1)

  for (i in 1:k.x){

    if ( q.typeYnum[i] == 3){ #ordered

      ## First equation on page 8 in CHP (2015) - calculating relative frequencies for plug-in bandwidth
      rf <- table(xx10[,i])/length(xx10[,i])
      rot.bw10[i] <- (1/(1 + (n10*sum((1-rf)^2/(sum(rf*(1-rf)))))))

      ## switch for the sample size
      rot.bw00[i] <- (1/(1 + (n00*sum((1-rf)^2/(sum(rf*(1-rf)))))))
      rot.bw1less[i] <- (1/(1 + (n1less*sum((1-rf)^2/(sum(rf*(1-rf)))))))
      rot.bw0less[i] <- (1/(1 + (n0less*sum((1-rf)^2/(sum(rf*(1-rf)))))))

      rot.bw1pool[i] <- (1/(1 + (n1pool*sum((1-rf)^2/(sum(rf*(1-rf)))))))
      rot.bw0pool[i] <- (1/(1 + (n0pool*sum((1-rf)^2/(sum(rf*(1-rf)))))))

    } else if (q.typeYnum[i] == 2) { # factor

      ## First equation on page 8 in CHP (2015) - calculating relative frequencies for plug-in bandwidth
      rf <- table(xx10[,i])/length(xx10[,i])
      rot.bw10[i] <- (1/(1 + (n10*sum((1-rf)^2/(sum(rf*(1-rf)))))))

      ## switch for the sample size
      rot.bw00[i] <- (1/(1 + (n00*sum((1-rf)^2/(sum(rf*(1-rf)))))))
      rot.bw1less[i] <- (1/(1 + (n1less*sum((1-rf)^2/(sum(rf*(1-rf)))))))
      rot.bw0less[i] <- (1/(1 + (n0less*sum((1-rf)^2/(sum(rf*(1-rf)))))))

      rot.bw1pool[i] <- (1/(1 + (n1pool*sum((1-rf)^2/(sum(rf*(1-rf)))))))
      rot.bw0pool[i] <- (1/(1 + (n0pool*sum((1-rf)^2/(sum(rf*(1-rf)))))))

    } else { # continuous

      ## note no sd(x) because we scaled them already
      rot.bw10[i] <- 1.06*n10^(-1/(4+q.type[1]))

      ## switch for the sample size
      rot.bw00[i] <- 1.06*n00^(-1/(4+q.type[1]))
      rot.bw1less[i] <- 1.06*n1less^(-1/(4+q.type[1]))
      rot.bw0less[i] <- 1.06*n0less^(-1/(4+q.type[1]))

      rot.bw1pool[i] <- 1.06*n1pool^(-1/(4+q.type[1]))
      rot.bw0pool[i] <- 1.06*n0pool^(-1/(4+q.type[1]))

    }

  } ## i

  # ## print bws ----
  # my.bw <- data.frame(Regressor = colnames(x), Type = q.typeY, Bandwidth = bw11)
  # if (print.level > 0) {
  #   cat("\n")
  #   print(my.bw)
  # }

   ## Calculating BSC statistic ----

  time.05 <- proc.time()

  if (print.level > 0) {
    cat(paste0("\nCalculating BSC \n"))
  }

  ## produce equation 8 from the analog write-up
  # print("num10")
  # num10_ <- np::npksum(txdat=xx10,tydat=wy10,exdat=xx10,bws=rot.bw10)$ksum
  num10 <- .npksumYXnew(
    Nthreds = cores,
    ydat=wy10, xdat=as.matrix(xx10), xeval=as.matrix(xx10), bw=rot.bw10,
    xtype=q.typeYnum, nlevels=q.levels, n=n10, neval=n10, q=k.x
  )
  # cat.print( data.frame(num10_, num10, num10_ - num10)[1:10,] )


  # print("num1less")
  # num1less_ <- np::npksum(txdat=xx1less,tydat=wy1less,exdat=xx10,bws=rot.bw1less)$ksum
  num1less <- .npksumYXnew(
    Nthreds = cores,
    ydat=wy1less, xdat=as.matrix(xx1less), xeval=as.matrix(xx10), bw=rot.bw1less,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx1less), neval=n10, q=k.x
  )
  # cat.print( data.frame(num1less_, num1less, num1less_ - num1less)[1:10,] )

  # print("num00")
  # num00_ <- np::npksum(txdat=xx00,tydat=wy00,exdat=xx10,bws=rot.bw00)$ksum
  num00 <- .npksumYXnew(
    Nthreds = cores,
    ydat=wy00, xdat=as.matrix(xx00), xeval=as.matrix(xx10), bw=rot.bw00,
    xtype=q.typeYnum, nlevels=q.levels, n=n00, neval=n10, q=k.x
  )
  # cat.print( data.frame(num00_, num00, num00_ - num00)[1:10,] )

  # print("num0less")
  # num0less_ <- np::npksum(txdat=xx0less,tydat=wy0less,exdat=xx10,bws=rot.bw0less)$ksum
  num0less <- .npksumYXnew(
    Nthreds = cores,
    ydat=wy0less, xdat=as.matrix(xx0less), xeval=as.matrix(xx10), bw=rot.bw0less,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx0less), neval=n10, q=k.x
  )
  # cat.print( data.frame(num0less_, num0less, num0less_ - num0less)[1:10,] )


  # print("dem10")
  # dem10_ <- np::npksum(txdat=xx10,tydat=w10,exdat=xx10,bws=rot.bw10)$ksum
  dem10 <- .npksumYXnew(
    Nthreds = cores,
    ydat=w10, xdat=as.matrix(xx10), xeval=as.matrix(xx10), bw=rot.bw10,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx10), neval=n10, q=k.x
  )
  # cat.print( data.frame(dem10_, dem10, dem10_ - dem10)[1:10,] )


  # print("dem1less")
  # dem1less_ <- np::npksum(txdat=xx1less,tydat=w1less,exdat=xx10,bws=rot.bw1less)$ksum
  dem1less <- .npksumYXnew(
    Nthreds = cores,
    ydat=w1less, xdat=as.matrix(xx1less), xeval=as.matrix(xx10), bw=rot.bw1less,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx1less), neval=n10, q=k.x
  )
  # cat.print( data.frame(dem1less_, dem1less, dem1less_ - dem1less)[1:10,] )


  # print("dem00")
  # dem00_ <- np::npksum(txdat=xx00,tydat=w00,exdat=xx10,bws=rot.bw00)$ksum
  dem00 <- .npksumYXnew(
    Nthreds = cores,
    ydat=w00, xdat=as.matrix(xx00), xeval=as.matrix(xx10), bw=rot.bw00,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx00), neval=n10, q=k.x
  )
  # cat.print( data.frame(dem00_, dem00, dem00_ - dem00)[1:10,] )


  # print("dem0less")
  # dem0less_ <- np::npksum(txdat=xx0less,tydat=w0less,exdat=xx10,bws=rot.bw0less)$ksum
  dem0less <- .npksumYXnew(
    Nthreds = cores,
    ydat=w0less, xdat=as.matrix(xx0less), xeval=as.matrix(xx10), bw=rot.bw0less,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx0less), neval=n10, q=k.x
  )
  # cat.print( data.frame(dem0less_, dem0less, dem0less_ - dem0less)[1:10,] )


  bsc.stat <- (1/n10)*sum(((num10/dem10) - (num1less/dem1less) - (num00/dem00) + (num0less/dem0less))^2)

  if (print.level > 0) {
    cat(paste0("BSC = ",formatC(bsc.stat, digits = 10),"\n"))
  }

  time.06 <- proc.time()
  BSC.time.sec <- round( (time.06-time.05)[3], 0)
  names(BSC.time.sec) <- "sec"

  if (print.level > 0){
    .timing(BSC.time.sec, "Calculating BSC completed in ")
    # cat("___________________________________________________\n")
  }

  # return(1)

  ## Bootstraping BSC ----


  ## begin the bootstrap for BSC
  if (print.level > 0) {
    cat(paste0("\nBootstrapping the statistic (",boot.num," replications)\n"))
  }
  set.seed(seed = seed)#, kind = "L'Ecuyer-CMRG")
  seeds <- sample.int(n = .Machine$integer.max, size = boot.num)

  ### residuals -----
  ## obtain residuals for the alternative model
  ## estimate the functions at the actual data points for each group, time period
  if (print.level > 0) {
    cat(paste0("Calculating residuals for the alternative model\n"))
  }
  time.05 <- proc.time()

  t.rot.bw10 <- rot.bw10
  t.rot.bw1less <- rot.bw1less
  t.rot.bw00 <- rot.bw00
  t.rot.bw0less <- rot.bw0less

  # nnum10_ <- npksum(txdat=xx10,tydat=wy10,exdat=xx10,bws=t.rot.bw10)$ksum
  nnum10 <- .npksumYX(
    Nthreds = cores,
    ydat=wy10, xdat=as.matrix(xx10), bw=t.rot.bw10,
    xtype=q.typeYnum, nlevels=q.levels, n=n10, q=k.x
  )
  # cat.print( data.frame(nnum10_, nnum10, nnum10_ - nnum10)[1:10,] )

  # nnum1less_ <- npksum(txdat=xx1less,tydat=wy1less,exdat=xx1less,bws=t.rot.bw1less)$ksum
  nnum1less <- .npksumYX(
    Nthreds = cores,
    ydat=wy1less, xdat=as.matrix(xx1less), bw=t.rot.bw1less,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx1less), q=k.x
  )
  # cat.print( data.frame(nnum1less_, nnum1less, nnum1less_ - nnum1less)[1:10,] )

  # nnum00_ <- npksum(txdat=xx00,tydat=wy00,exdat=xx00,bws=t.rot.bw00)$ksum
  nnum00 <- .npksumYX(
    Nthreds = cores,
    ydat=wy00, xdat=as.matrix(xx00), bw=t.rot.bw00,
    xtype=q.typeYnum, nlevels=q.levels, n=n00, q=k.x
  )
  # cat.print( data.frame(nnum00_, nnum00, nnum00_ - nnum00)[1:10,] )

  # nnum0less_ <- npksum(txdat=xx0less,tydat=wy0less,exdat=xx0less,bws=t.rot.bw0less)$ksum
  nnum0less <- .npksumYX(
    Nthreds = cores,
    ydat=wy0less, xdat=as.matrix(xx0less), bw=t.rot.bw0less,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx0less), q=k.x
  )
  # cat.print( data.frame(nnum0less_, nnum0less, nnum0less_ - nnum0less)[1:10,] )


  # ddem10_ <- npksum(txdat=xx10,tydat=w10,exdat=xx10,bws=t.rot.bw10)$ksum
  ddem10 <- .npksumYX(
    Nthreds = cores,
    ydat=w10, xdat=as.matrix(xx10), bw=t.rot.bw10,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx10), q=k.x
  )
  # cat.print( data.frame(ddem10_, ddem10, ddem10_ - ddem10)[1:10,] )


  # ddem1less_ <- npksum(txdat=xx1less,tydat=w1less,exdat=xx1less,bws=t.rot.bw1less)$ksum
  ddem1less <- .npksumYX(
    Nthreds = cores,
    ydat=w1less, xdat=as.matrix(xx1less), bw=t.rot.bw1less,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx1less), q=k.x
  )
  # cat.print( data.frame(ddem1less_, ddem1less, ddem1less_ - ddem1less)[1:10,] )

  # ddem00_ <- npksum(txdat=xx00,tydat=w00,exdat=xx00,bws=t.rot.bw00)$ksum
  ddem00 <- .npksumYX(
    Nthreds = cores,
    ydat=w00, xdat=as.matrix(xx00), bw=t.rot.bw00,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx00), q=k.x
  )
  # cat.print( data.frame(ddem00_, ddem00, ddem00_ - ddem00)[1:10,] )


  # ddem0less_ <- npksum(txdat=xx0less,tydat=w0less,exdat=xx0less,bws=t.rot.bw0less)$ksum
  ddem0less <- .npksumYX(
    Nthreds = cores,
    ydat=w0less, xdat=as.matrix(xx0less), bw=t.rot.bw0less,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx0less), q=k.x
  )
  # cat.print( data.frame(ddem0less_, ddem0less, ddem0less_ - ddem0less)[1:10,] )

  time.06 <- proc.time()
  rez.time.sec <- round( (time.06-time.05)[3], 0)
  names(rez.time.sec) <- "sec"

  if (print.level > 1){
    .timing(rez.time.sec, "Calculating residuals for the alternative model completed in ")
    # cat("___________________________________________________\n")
  }

  m10 <- nnum10/ddem10
  m1less <- nnum1less/ddem1less
  m00 <- nnum00/ddem00
  m0less <- nnum0less/ddem0less

  resid.10 <- y10 - m10
  resid.1less <- y1less - m1less
  resid.00 <- y00 - m00
  resid.0less <- y0less - m0less

  resid.10 <- resid.10 - mean(resid.10)
  resid.1less <- resid.1less - mean(resid.1less)
  resid.00 <- resid.00 - mean(resid.00)
  resid.0less <- resid.0less - mean(resid.0less)

  ### fitted values ----
  ## created fitted values under the null hypothesis
  ## pool the 0 and past time periods together (separately for both treated and control groups)
  if (print.level > 0) {
    cat(paste0("Calculating fitted values under the null hypothesis\n"))
  }
  time.05 <- proc.time()

  # nnum1pool_ <- npksum(txdat=xx1pool,tydat=wy1pool,exdat=xx1pool,bws=rot.bw1pool)$ksum
  nnum1pool <- .npksumYX(
    Nthreds = cores,
    ydat=wy1pool, xdat=as.matrix(xx1pool), bw=rot.bw1pool,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx1pool), q=k.x
  )
  # cat.print( data.frame(nnum1pool_, nnum1pool, nnum1pool_ - nnum1pool)[1:10,] )

  # nnum0pool_ <- npksum(txdat=xx0pool,tydat=wy0pool,exdat=xx0pool,bws=rot.bw0pool)$ksum
  nnum0pool <- .npksumYX(
    Nthreds = cores,
    ydat=wy0pool, xdat=as.matrix(xx0pool), bw=rot.bw0pool,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx0pool), q=k.x
  )
  # cat.print( data.frame(nnum0pool_, nnum0pool, nnum0pool_ - nnum0pool)[1:10,] )

  # ddem1pool_ <- npksum(txdat=xx1pool,tydat=w1pool,exdat=xx1pool,bws=rot.bw1pool)$ksum
  ddem1pool <- .npksumYX(
    Nthreds = cores,
    ydat=w1pool, xdat=as.matrix(xx1pool), bw=rot.bw1pool,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx1pool), q=k.x
  )
  # cat.print( data.frame(ddem1pool_, ddem1pool, ddem1pool_ - ddem1pool)[1:10,] )

  # ddem0pool_ <- npksum(txdat=xx0pool,tydat=w0pool,exdat=xx0pool,bws=rot.bw0pool)$ksum
  ddem0pool <- .npksumYX(
    Nthreds = cores,
    ydat=w0pool, xdat=as.matrix(xx0pool), bw=rot.bw0pool,
    xtype=q.typeYnum, nlevels=q.levels, n=nrow(xx0pool), q=k.x
  )
  # cat.print( data.frame(ddem0pool_, ddem0pool, ddem0pool_ - ddem0pool_)[1:10,] )
  # cat.print(summary(ddem0pool))

  m1pool <- nnum1pool/ddem1pool
  m0pool <- nnum0pool/ddem0pool

  m1pool0 <- m1pool[which(t1pool==0)]
  m1poolless <- m1pool[which(t1pool<0)]

  m0pool0 <- m0pool[which(t0pool==0)]
  m0poolless <- m0pool[which(t0pool<0)]

  # cat.print(table(t0pool))
  # cat.print(table(t1pool))

  # if (print.level > 1) {
  #   cat(paste0("Calculating fitted values under the null hypothesis completed\n"))
  # }
  time.06 <- proc.time()
  fit.time.sec <- round( (time.06-time.05)[3], 0)
  names(fit.time.sec) <- "sec"

  if (print.level > 1){
    .timing(fit.time.sec, "Calculating fitted values under the null hypothesis completed in ")
    # cat("___________________________________________________\n")
  }

  time.05 <- proc.time()

  # cat.print(length(m1pool0))
  # cat.print(length(resid.10))
  # cat.print(n10)
  #
  # cat.print(length(m1poolless))
  # cat.print(length(resid.1less))
  # cat.print(n1less)
  #
  # cat.print(length(m0pool0))
  # cat.print(length(resid.00))
  # cat.print(n00)
  #
  # cat.print(length(m0poolless))
  # cat.print(length(resid.0less))
  # cat.print(n0less)

  # # do sequentially
  # atet.boot <- rep(1, boot.num)
  # atet.boot.hetero <- matrix(0, nrow = n1, ncol = boot.num)
  #
  # # if (print.level > 0)
  # # {
  # #     # cat("\n")
  # #     pb <- utils::txtProgressBar(min = 0, max = boot.num, style = 3)
  # # }
  #
  #
  # for (j in 1:boot.num){
  #
  #     ## bootstrap if the y variable is binary
  #     if(is.binary(y)==TRUE){
  #
  #         y11.new <- ifelse(tt11>runif(n11, min = 0, max = 1),1,0)
  #         y10.new <- ifelse(tt10>runif(n10, min = 0, max = 1),1,0)
  #         y01.new <- ifelse(tt01>runif(n01, min = 0, max = 1),1,0)
  #         y00.new <- ifelse(tt00>runif(n00, min = 0, max = 1),1,0)
  #
  #     } else {
  #
  #         y11.new <- tt11 + resid.11*rnorm(n11,0,1)
  #         y10.new <- tt10 + resid.10*rnorm(n10,0,1)
  #         y01.new <- tt01 + resid.01*rnorm(n01,0,1)
  #         y00.new <- tt00 + resid.00*rnorm(n00,0,1)
  #
  #     }
  #
  #     wy11.new <- as.vector(w11*y11.new)
  #     wy10.new <- as.vector(w10*y10.new)
  #     wy01.new <- as.vector(w01*y01.new)
  #     wy00.new <- as.vector(w00*y00.new)
  #
  #     num11.new <- np::npksum(txdat=xx11,tydat=wy11.new,exdat=xx1,bws=bw11)$ksum
  #     num10.new <- np::npksum(txdat=xx10,tydat=wy10.new,exdat=xx1,bws=bw10)$ksum
  #     num01.new <- np::npksum(txdat=xx01,tydat=wy01.new,exdat=xx1,bws=bw01)$ksum
  #     num00.new <- np::npksum(txdat=xx00,tydat=wy00.new,exdat=xx1,bws=bw00)$ksum
  #
  #     atet.boot[j] <- mean(num11.new/dem11 - num10.new/dem10 - num01.new/dem01 + num00.new/dem00)
  #     atet.boot.hetero[,j] <- as.matrix(num11.new/dem11 - num10.new/dem10 - num01.new/dem01 + num00.new/dem00)
  #
  #     if (print.level > 0 & j == 1)
  #     {
  #       time.06 <- proc.time()
  #       boot.time.sec <- boot.num*(time.06-time.05)[3]
  #       .timing(boot.time.sec, "Bootstrapping will take approximately: ")
  #       # cat("\n")
  #       pb <- utils::txtProgressBar(min = 1, max = boot.num, style = 3)
  #     }
  #
  #     if (print.level > 0 & j > 1) utils::setTxtProgressBar(pb, j)
  #
  # } ## j
  #
  # # do sequentially completed

  # do parallel

  # doParallel::registerDoParallel(cores = parallel::detectCores()/4)
  doParallel::registerDoParallel(cores = cores)

  # if (print.level > 0 & cores == 1)
  # {
  #   pb <- utils::txtProgressBar(min = 0, max = boot.num, style = 3)
  # }

  mcoptions <- list(set.seed = TRUE)

  if (print.level > 0) {
    cat(paste0("\nThe main Loop of the Bootstrapping started\n"))
  }

  cores1 <- 1

  bsc.boot <-
    foreach::foreach(j = 1:boot.num, .options.multicore = mcoptions, .combine = "c", .verbose = FALSE) %dopar% {

      set.seed(seeds[j])

      y10.new <- m1pool0 + resid.10*rnorm(n10,0,1)
      y1less.new <- m1poolless + resid.1less*rnorm(n1less,0,1)
      y00.new <- m0pool0 + resid.00*rnorm(n00,0,1)
      y0less.new <- m0poolless + resid.0less*rnorm(n0less,0,1)

      wy10.new <- as.vector(w10*y10.new)
      wy1less.new <- as.vector(w1less*y1less.new)
      wy00.new <- as.vector(w00*y00.new)
      wy0less.new <- as.vector(w0less*y0less.new)

      ## produce equation 8 from the analog write-up
      # num10.new_ <- npksum(txdat=xx10,tydat=wy10.new,exdat=xx10,bws=rot.bw10)$ksum
      num10.new <- .npksumYX(
        Nthreds = cores1,
        ydat=wy10.new, xdat=as.matrix(xx10), bw=rot.bw10,
        xtype=q.typeYnum, nlevels=q.levels, n=n10, q=k.x
      )
      # cat.print( data.frame(num10.new_, num10.new, num10.new_ - num10.new)[1:10,] )

      # num1less.new_ <- npksum(txdat=xx1less,tydat=wy1less.new,exdat=xx10,bws=rot.bw1less)$ksum
      num1less.new <- .npksumYXnew(
        Nthreds = cores1,
        ydat=wy1less.new, xdat=as.matrix(xx1less), xeval=as.matrix(xx10), bw=rot.bw1less,
        xtype=q.typeYnum, nlevels=q.levels, n=n1less, neval=n10, q=k.x
      )
      # cat.print( data.frame(num1less.new_, num1less.new, num1less.new_ - num1less.new)[1:10,] )


      # num00.new_ <- npksum(txdat=xx00,tydat=wy00.new,exdat=xx10,bws=rot.bw00)$ksum
      num00.new <- .npksumYXnew(
        Nthreds = cores1,
        ydat=wy00.new, xdat=as.matrix(xx00), xeval=as.matrix(xx10), bw=rot.bw00,
        xtype=q.typeYnum, nlevels=q.levels, n=n00, neval=n10, q=k.x
      )
      # cat.print( data.frame(num00.new_, num00.new, num00.new_ - num00.new)[1:10,] )

      # num0less.new_ <- npksum(txdat=xx0less,tydat=wy0less.new,exdat=xx10,bws=rot.bw0less)$ksum
      num0less.new <- .npksumYXnew(
        Nthreds = cores1,
        ydat=wy0less.new, xdat=as.matrix(xx0less), xeval=as.matrix(xx10), bw=rot.bw0less,
        xtype=q.typeYnum, nlevels=q.levels, n=n0less, neval=n10, q=k.x
      )
      # cat.print( data.frame(num0less.new_, num0less.new, num0less.new_ - num0less.new)[1:10,] )


      if (print.level > 0 & cores == 1)
      {
        pb <- utils::txtProgressBar(min = 0, max = boot.num, style = 3)
      }
      if (print.level > 0 & cores == 1 & j == 1)
      {
        time.06 <- proc.time()
        boot.time.sec <- round( boot.num*(time.06-time.05)[3], 0)
        .timing(boot.time.sec, "\nBootstrapping will take approximately: ")
        cat("\n")
        pb <- utils::txtProgressBar(min = 1, max = boot.num, style = 3)
      }

      if (print.level > 0 & cores == 1 & j > 1) utils::setTxtProgressBar(pb, j)

      (1/n10)*sum(((num10.new/dem10) - (num1less.new/dem1less) - (num00.new/dem00) + (num0less.new/dem0less))^2)

    }

  # do parallel completed
  if (print.level > 0 & cores == 1) cat("\n")

  time.06 <- proc.time()
  boot.time.sec <- round( (time.06-time.05)[3], 0)
  names(boot.time.sec) <- "sec"

  if (print.level >= 1){
    .timing(boot.time.sec, "Bootstrapping the statistic completed in ")
    # cat("___________________________________________________\n")
  }

  bsc.sd <- sd(bsc.boot)

  ## Calculate p-value
  rank.stat <- rank(c(bsc.stat,bsc.boot))
  p.value <- 1-(rank.stat[1]/(boot.num+1))

  if (print.level > 0) {
    cat("\nBSC stat =",formatC(bsc.stat, digits = digits),"")
    cat("\nBSC sd   =",formatC(bsc.sd, digits = digits),"")
    cat("\n p-value =",formatC(p.value, digits = digits),"\n")
  }


  # if (print.level > 0) cat("\n")

  # cat("11\n")

  # sd.atet <- sd(atet.boot)



  # cat("12\n")

  ## Return ----

  tymch <- list(
    BSC = bsc.stat,
    BSC.sd = bsc.sd,
    BSC.P = p.value,
    BSC.boot = bsc.boot,
    boot.num = boot.num#,
    # plot(density(bsc.boot,bw="sj")),abline(v=bsc.stat)
    )

  # cat("13\n")

  class(tymch) <- c("didnpbsctest", "didnp")
  return(tymch)

}
