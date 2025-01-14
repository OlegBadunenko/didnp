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
#' The \code{didnpreg} command contains tools for computing both heterogenous and average treatment effects for the treated in a model-free differences-in-differences framework.
#'
#' @param formula an object of class formula (or one that can be coerced to that class): a symbolic description of the model. The details of model specification are given under `Details'
#' @param data name of the data frame; must be specified if the 'formula' method is used
#' @param subset NULL, optional subsample of 'data'
#' @param outcome a vector, matrix, or data frame of length \eqn{NT}. The outcome can be a continuous or dummy variable.
#' @param regressors a data frame with \eqn{NT} rows that contains regressors.
#' A data frame class is required to identify the type/class of each regressor.
#' @param time a vector, matrix, or data frame of length \eqn{NT} that specifies in which period \code{id} is observed.
#' @param treated a vector, matrix, or data frame of length \eqn{NT} with zeros for the control and ones for the treated observations.
#' @param treatment_period a vector, matrix, or data frame of length \eqn{NT} with zeros for the period before treatment and ones for the period of treatment and after.
#' @param weights NULL,
#' @param bws a bandwidth specification. A vector of bandwidths of length corresponding to the number of regressors.
#' @param bwmethod bandwidth type. 2 options can be specified. "opt" is the default option, the  plug-in is rule of thumb for continuous and basic for categorical. "CV" will calculate cross-validated bandwidths.
#' @param boot.num an single value specifying the number of bootstrap replications.  Default is 399.
#' @param TTx Conditional Treatment Effect on the Treated. Default is FALSE.
#' @param TTb Unconditional Treatment Effect on the Treated. TTb estimates by averaging over *all* treated. TTa estimates by averaging over treated one time period after the treatment. Depending on the sample, calcularing TTb may take some time. Default is FALSE.
#' @param print.level the level of printing; larger number implies more output is printed. Default is 1. 0 suppresses all printing.
#' @param cores Integer specifies the number of cores to be used for parallel computation.
#' @param seed integer used for the random number generation for the replication purposes. Default is 17345168.
#'
#' @details
#' The formula shell contain multiple parts separated by '|'. An example is
#'
#' form1 <- y ~ x1 + x2 | time | treated | treatment_period | weights
#'
#' weights can be omitted if not available
#'
#' form1 <- y ~ x1 + x2 | time | treated | treatment_period
#'
#'
#' @return \code{didnpreg} returns a list containing:
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
#'    \tab A vector of TRUE/FALSE values identifying treated observations right after the treatment
#'    \cr \tab \cr
#'    \code{sample10}
#'    \tab A vector of TRUE/FALSE values identifying treated observations just before the treatment
#'    \cr \tab \cr
#'    \code{sample01}
#'    \tab A vector of TRUE/FALSE values identifying observations in control group right after the treatment
#'    \cr \tab \cr
#'    \code{sample00}
#'    \tab A vector of TRUE/FALSE values identifying observations in control group just before the treatment
#'    \cr \tab \cr
#'    \code{n11}
#'    \tab A number of treated observations right after the treatment
#'    \cr \tab \cr
#'    \code{n10}
#'    \tab A number of treated observations just before the treatment
#'    \cr \tab \cr
#'    \code{n01}
#'    \tab A number of observations in control group right after the treatment
#'    \cr \tab \cr
#'    \code{n00}
#'    \tab A number of observations in control group just before the treatment
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
#'    \tab Bandwidths calculated for the sample of observations in control group just before the treatment
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
#'    \code{TTa.se}
#'    \tab the standard error of the DiD estimator of the avarage unconditional TT
#'    \cr \tab \cr
#'    \code{TTb.se}
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
#'   tym1a <- didnpreg(
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
#'   tym1aCV <- didnpreg(
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
#'   tym1bCV <- didnpreg(
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
#'   tym1aM <- didnpreg(
#'     outcome = DACAsub[mysmpl,"inschool"],
#'     regressors = DACAsub[mysmpl,c("fem", "race", "var.bpl", "state", "age", "yrimmig", "ageimmig")],
#'     id = DACAsub[mysmpl,"inschool"],
#'     time = DACAsub[mysmpl,"year"],
#'     treated = DACAsub[mysmpl,"elig"],
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
#' Daniel J. Henderson and Stefan Sperlich (2023). A Complete Framework for Model-Free Difference-in-Differences Estimation. \emph{Foundations and Trends in Econometrics},
#' \bold{12}(3), 232-323
#' \url{http://dx.doi.org/10.1561/0800000046.}
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
#' @rdname didnpreg
#' @export
didnpreg <- function(...){
  # args = list(...)
  # cat.print(args)
  # cat.print(names(args))
  # cat.print(class(args))
  # lapply(args, cat.print)
  UseMethod("didnpreg")
}

#' @rdname didnpreg
#' @method didnpreg formula
#' @export
didnpreg.formula <- function(
    formula,
    data  = stop("argument 'data' is missing"),
    subset,
    bws = NULL,
    bwmethod = "opt", # plug-in is rule of thumb for continuous and basic for categorical, can be cross-validation
    boot.num = 399,
    TTx = "TTa",
    level = 95,
    print.level = 1,
    digits = 4,
    cores = 1,
    seed = 17345168,
    ...)
{
  # cat("'formula': this is to get the matrices from formula and feed to 'didnpreg.default'\n")

  # cat.print(bwmethod)

  # cat.print(boot.num)

  ## handle TTx ----

  if ( !(TTx %in% c("TTa", "TTb")) ) stop("'TTx' other than TTa or TTb not implemented yet")

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

  # cat.print(table(esample))
  # cat("12\n")

  # cat.print(form1)
  # cat.print(length(form1))
  # cat.print(length(form1)[2])

  if (length(form1)[2] < 4) stop("specificaions are inappropriate")

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
  # model.matrix(form1, lhs = 0, rhs = 2, data = mf)[,-1] -> id
  # if (length(id) != nt) stop("specificaion 'id' is inappropriate")
  # cat.print(class(id))
  # cat.print(head(id))
  model.matrix(form1, lhs = 0, rhs = 2, data = mf)[,-1] -> time
  if (length(time) != nt) stop("specificaion 'time' is inappropriate")
  # cat.print(class(time))
  # cat.print(head(time))
  # cat.print(table(time))
  model.matrix(form1, lhs = 0, rhs = 3, data = mf)[,-1] -> treated
  if (length(treated) != nt) stop("specificaion 'treated' is inappropriate")
  # cat.print(class(treated))
  # cat.print(head(treated))
  # cat.print(table(treated))
  # cat.print(table(time,treated))
  model.matrix(form1, lhs = 0, rhs = 4, data = mf)[,-1] -> treatment_period
  if (length(treatment_period) != nt) stop("specificaion 'treatment_period' is inappropriate")
  # cat.print(class(treatment_period))
  # cat.print(head(treatment_period))
  # cat.print(table(treatment_period))
  # see if weights was specified
  if (length(form1)[2] == 4) {
    weights <-  rep(1, nt)
  } else {
    model.matrix(form1, lhs = 0, rhs = 5, data = mf)[,-1] -> weights
    if (length(weights) != nt) stop("specificaion 'weights' is inappropriate")
  }
  # cat.print(class(weights))
  # cat.print(head(weights))

  # cat("14\n")

  # tymch <- eval(parse(text=paste("
  # didnpreg( outcome=Y,
  #     regressors=X,
  #     id=id,
  #     time=time,
  #     treated=treated,
  #     treatment_period=treatment_period,
  #     weights = weights,
  #     bwmethod = bwmethod,
  #     boot.num = boot.num,
  #     TTaOnly = TTaOnly,
  #     print.level = print.level,
  #     ...)
  #  ")))

  tymch <- didnpreg(
    outcome=Y,
    regressors=X,
    time=time,
    treated=treated,
    treatment_period=treatment_period,
    weights = weights,
    bws=bws,
    bwmethod = bwmethod,
    boot.num = boot.num,
    TTx = TTx,
    level = level,
    print.level = print.level,
    digits = digits,
    cores = cores,
    seed = seed,
    ...)

  # https://www.programmingr.com/fast-r-append-list/
  tymch[["esample"]] <- esample
  # tymch <- c(tymch, esample = list(esample))   # Add element to list
  # cat("15\n")
  # cat.print(table(esample))
  # cat.print(table(tymch[["esample"]]))
  # cat.print(table(tymch$esample))

  class(tymch) <- c("didnpreg", "didnp")
  return(tymch)

}

#' @rdname didnpreg
#' @method didnpreg default
#' @export
didnpreg.default <- function(
    outcome,
    regressors,
    time,
    treated,
    treatment_period,
    weights = NULL,
    bws = NULL,
    bwmethod = "opt", # plug-in is rule of thumb for continuous and basic for categorical, can be cross-validation
    boot.num = 399,
    TTx = "TTa",
    level = 95,
    print.level = 1,
    digits = 4,
    cores = 1,
    seed = 17345168,
    ...)
{
  # cat("'default': this is the workhorse\n")
  # c(class(outcome), class(regressors), class(id), class(time), class(treated), class(treatment_period))
  ## check specifications ----
  if (missing(outcome)) stop("vector 'outcome' is missing")
  if ( !(class(outcome) %in% c("numeric")) ) stop("wrong class of 'outcome': must be 'numeric'")

  if (missing(regressors)) stop("data.frame 'regressors' is missing")
  if ( !(class(regressors) %in% c("data.frame")) ) stop("wrong class of 'regressors': must be 'data.frame'")

  if (missing(time)) stop("vector 'time' is missing")
  if ( !(class(time) %in% c("numeric")) ) stop("wrong class of 'time': must be 'numeric'")

  if (missing(treated)) stop("vector 'treated' is missing")
  if ( !(class(treated) %in% c("numeric")) ) stop("wrong class of 'treated': must be 'numeric'")
  table(treated) -> treatment_values
  if (length(treatment_values) != 2 | names(treatment_values)[1] != "0" | names(treatment_values)[2] != "1") stop("vector 'treated' must have exactly 2 values, 0 and 1")

  if (missing(treatment_period)) stop("vector 'treatment_period' is missing")
  if ( !(class(treatment_period) %in% c("numeric")) ) stop("wrong class of 'treatment_period': must be 'numeric'")
  table(treatment_period) -> treatment_period_values
  # cat.print(treatment_period_values)
  if (length(treatment_period_values) != 2 | names(treatment_period_values)[1] != "0" | names(treatment_period_values)[2] != "1") stop("vector 'treatment_period' must have exactly 2 values, 0 and 1")

  if ( !(bwmethod %in% c("opt", "CV")) ) stop("'bwmethod' can be either 'opt' or 'CV'")

  ## handle TTx ----

  if ( !(TTx %in% c("TTa", "TTb")) ) stop("'TTx' other than TTa or TTb not implemented yet")

  if (TTx == "TTa") {
    TTb <- FALSE
  } else {
    TTb <- TRUE
  }
  do.TTb <- TTb

  if (level < 10 | level > 99.99) {
    stop("'level' must be between 10 and 99.99 inclusive")
  }
  level <- floor(level)


  ## check dimensions ----

  nt.o <- length(outcome)
  nt.x <- nrow(regressors)
  # nt.id <- length(id)
  nt.time <- length(time)
  nt.tr <- length(treated)
  nt.tr.p <- length(treatment_period)
  if (is.null(weights)) {
    weights <- rep(1, n.o)
  }
  nt.w <- length(weights)

  if(nt.o != nt.x) stop("vector 'outcome' and data.frame 'regressors' have different number of observations")
  # if(nt.o != nt.id) stop("vector 'outcome' and vector 'id' have different number of observations")
  if(nt.o != nt.time) stop("vector 'outcome' and vector 'time' have different number of observations")
  if(nt.o != nt.tr) stop("vector 'outcome' and vector 'treated' have different number of observations")
  if(nt.o != nt.tr.p) stop("vector 'outcome' and vector 'treatment_period' have different number of observations")
  if(nt.o != nt.w) stop("vector 'outcome' and vector 'weights' have different number of observations")

  ## handle missing values ----

  d0 <-
    data.frame(
      weights,
      treatment_period,
      treated,
      time,
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
  d0[,3] -> d # treated
  d0[,4] -> it.time # time
  # d0[,5] -> it.id # id
  d0[,5] -> y # outcome
  d0[ , -c(1:5), drop = FALSE] -> x # regressors
  k.x <- ncol(x)

  ## handle treatment period ----

  time.treatment <- min( it.time[treatment_period == 1]  - 1)
  # cat.print(table(it.time))
  # cat.print(time.treatment)
  time.min <- min( it.time )
  time.max <- max( it.time )
  if( time.treatment - time.min < 2 ) warning (
    paste0("Data starts in ",time.min,", while the treatment is in ",time.treatment)
  )
  if( time.max - time.treatment < 2 ) warning (
    paste0("Data ends in ",time.max,", while the treatment is in ",time.treatment)
  )
  # t <- it.time - time.treatment
  # t <- treatment_period
  # cat.print(table(t))
  # t <- it.time
  # cat.print(table(t))
  t <- it.time - time.treatment
  # cat.print(table(t))

  # return(1)

  time.treatment <- min( it.time[treatment_period == 1] ) - 1
  # cat.print(time.treatment)
  ta <- it.time - time.treatment
  # cat.print(table(ta))

  ## variable type ----

  q.type <- matrix( 0, nrow = 3, ncol = 1 ) # count type of variables
  q.typeY <- character(k.x) # character names
  q.typeYnum <- double(k.x) # type of each variable
  q.levels <- rep(1, k.x) # number of levels of each variable; 1 for continuous

  for (i in 1:k.x){

    if (is.ordered(x[,i]) == TRUE){

      x[,i] <- droplevels(x[,i])

      rf <- table(droplevels(x[,i]))
      if( length(rf) == 1) {
        stop("Variable ", colnames(x)[i], " has no variation\n")
      }

      q.type[3] <- q.type[3] + 1
      q.typeY[i] <- "ordered"
      q.typeYnum[i] <- 3
      q.levels[i] <- length( levels( x[,i] ) ) - 1

    } else if (is.factor(x[,i]) == TRUE) {

      x[,i] <- droplevels(x[,i])

      rf <- table(droplevels(x[,i]))
      if( length(rf) == 1) {
        stop("Variable ", colnames(x)[i], " has no variation\n")
      }

      q.type[2] <- q.type[2] + 1
      q.typeY[i] <- "factor"
      q.typeYnum[i] <- 2
      q.levels[i] <- length( levels( x[,i] ) ) - 1

    } else {

      if( sd(x[,i]) == 0) {
        stop("Variable ", colnames(x)[i], " has no variation\n")
      }

      q.type[1] <- q.type[1] + 1
      q.typeY[i] <- "continuous"
      q.typeYnum[i] <- 1

      ## divide each continuous x by its standard deviation
      ## it is only a mean to an end
      ## it will not change the conclusion of the test
      x[,i] <- as.vector(x[,i]/sd(x[,i]))

    }

  } ## i

  ## print info about the data ----

  if (print.level > 0) {
    cat("Number of observations =", nt.o, "\n")
  }

  ## separating the data by time and treatment status

  ## subsamples ------

  if(do.TTb){

    smpl1  <- d == 1
    smpl11 <- d == 1 & t >= 1
    smpl10 <- d == 1 & t <= 0
    smpl01 <- d == 0 & t >= 1
    smpl00 <- d == 0 & t <= 0

  } else {

    if (print.level > 0) {
      cat("Number of observations in the year of the treatment and one year after the treatment =", sum(ta == 1 | ta == 0), "\n")
    }

    smpl1  <- d == 1
    smpl11 <- d == 1 & ta >= 1
    smpl10 <- d == 1 & ta <= 0
    smpl01 <- d == 0 & ta >= 1
    smpl00 <- d == 0 & ta <= 0

  }

  # cat.print(table(d))
  # cat.print(table(t))
  # cat.print(table(smpl00))
  # cat.print(table(smpl11))

  # w11 <- w[which(d==1 & t==1)]
  w11 <- w[smpl11]
  # w10 <- w[which(d==1 & t==0)]
  w10 <- w[smpl10]
  # w1less <- w[which(d==1 & t<0)]
  # w01 <- w[which(d==0 & t==1)]
  w01 <- w[smpl01]
  # w00 <- w[which(d==0 & t==0)]
  w00 <- w[smpl00]
  # w0less <- w[which(d==0 & t<0)]

  # xx <- x
  xx10 <- x[smpl10,]
  xx00 <- x[smpl00,]
  # for (i in 1:k.x) {
  #   cat.print(i)
  #   cat.print(levels(xx00[,i]))
  # }
  # xx1less <- x[which(d==1 & t<0),]
  # xx0less <- x[which(d==0 & t<0),]

  xx11 <- x[smpl11,] # for TTa
  xx01 <- x[smpl01,]

  xx1 <- x[smpl1,]

  y11 <- y[smpl11]
  y10 <- y[smpl10]
  y01 <- y[smpl01]
  y00 <- y[smpl00]

  wy11 <- y11*w11
  wy01 <- y01*w01
  wy10 <- y10*w10
  wy00 <- y00*w00

  n11 <- length(y11)
  n10 <- length(y10)
  n01 <- length(y01)
  n00 <- length(y00)

  if (print.level > 0) {
    cat("\n")
    cat("Number of observations in treated group after  the treatment (N_ 1, 1) =" ,n11, "\n")
    cat("Number of observations in treated group before the treatment (N_ 1, 0) =" ,n10, "\n")
    cat("Number of observations in control group after  the treatment (N_ 0, 1) =" ,n01, "\n")
    cat("Number of observations in control group before the treatment (N_ 0, 0) =" ,n00, "\n")
  }

  # sanity checks ----

  if(!is.finite(n11) || n11 == 0){
    stop("No observations in treated group right after the treatment")
  }
  if(!is.finite(n10) || n10 == 0){
    stop("No observations in treated group just before the treatment")
  }

  if(!is.finite(n01) || n01 == 0){
    stop("No observations in control group right after the treatment")
  }
  if(!is.finite(n00) || n00 == 0){
    stop("No observations in control group just before the treatment")
  }


  n1 <- nrow(xx1)

  ## print info about regressors ----

  if (print.level > 0) {

    cat("\n")

    # if (q.type[1] > 0) {
    cat("Number of Continuous Regressors            =" ,q.type[1], "\n")
    # }

    if (print.level > 2) {
      if (q.type[1] >= 0 & q.type[1] <= 3){
        cat("There are 3 or fewer continuous regressors\n")
      }
      if (q.type[1] > 3) {
        cat("There are more than 3 continuous regressors\n")
      }
    }

    # if (q.type[2] > 0) {
    cat("Number of Unordered Categorical Regressors =" ,q.type[2], "\n")
    # }

    # if (q.type[3] > 0) {
    cat("Number of Ordered Categorical Regressors   =" ,q.type[3], "\n")
    # }

    cat("\n")

  }

  if (TTb) {
    # prepare to retrieve TTa from TTb
    # init
    tym1 <- as.numeric( rep(NA, nt.o) )
    # fill only subsample
    tym1[smpl1] <- 1:n1
    tym1[smpl11] -> TTa.positions.in.TTb
  }



  ## bandwidths ----

  ### CV  ----

  if(is.null(bws)) {
    if (bwmethod == "CV") {
      if (print.level > 0) {
        cat(paste0("Calculating cross-validated bandwidths\n"))

        #### print info about bw ----

        if (q.type[1] > 0) {
          cat("Kernel Type for Continuous Regressors is               Gaussian\n")
        }
        if (q.type[2] > 0) {
          cat("Kernel Type for Unordered Categorical Regressors is    Aitchison and Aitken\n")
        }
        if (q.type[3] > 0) {
          cat("Kernel Type for Ordered Categorical is                 Li and Racine\n")
        }
      }

      ## plug-in bandwidths (used for starting values in LSCV)
      ## Silverman (1986) for cont (make it a function of q 1.06, 1.00, ....)
      ## Chu, Henderson and Parmeter (2015) for discrete
      ## bandwidths will be scaled by sample size for the other status (d=0, t=0)
      rot.bw00 <- matrix(0,nrow=k.x,ncol=1)

      # cat.print(dim(xx00))
      # cat.print(table(d,t))

      for (i in 1:k.x) {

        # cat.print(i)

        if (is.ordered(xx00[,i])==TRUE){

          ## First equation on page 8 in CHP (2015) - calculating relative frequencies for plug-in bandwidth
          tym <- table(droplevels(xx00[,i]))
          rf <- tym[tym>0]/length(xx00[,i])
          # rf <- table(xx00[,i])/length(xx00[,i])
          rot.bw00[i] <- (1/(1 + (n00*sum((1-rf)^2/(sum(rf*(1-rf)))))))
          # cat.print(rf)
          # cat.print(rot.bw00[i])

        } else if (is.factor(xx00[,i])==TRUE) {

          ## First equation on page 8 in CHP (2015) - calculating relative frequencies for plug-in bandwidth
          tym <- table(droplevels(xx00[,i]))
          rf <- tym[tym>0]/length(xx00[,i])
          # rf <- table(xx00[,i])/length(xx00[,i])
          rot.bw00[i] <- (1/(1 + (n00*sum((1-rf)^2/(sum(rf*(1-rf)))))))
          # cat.print(rf)
          # cat.print(rot.bw00[i])

        } else {

          ## note no sd(x) because we scaled them already
          rot.bw00[i] <- 1.06*n00^(-1/(4+q.type[1])) # adjust 1.06 to values of the Gaussian row on Page 70 Table 3.3
          # cat.print(rot.bw00[i])

        }

      }

      ## setting upper and lower bounds for bandwidths
      ## need them to be 5 for the continuous variables and 1 for the discrete variables
      lower <- rep(0,ncol(x))
      upper <- rep(1,ncol(x))
      for (ii in 1:k.x) {
        upper[ii] <- ifelse(is.factor(x[,ii]),1,5)
      }

      bw.start <- rot.bw00

      # cat.print(rot.bw00)

      # do it only on the treated in the treatment period: "11"

      time.05 <- proc.time()

      # bw.optim <- minqa::bobyqa(bw.start,lcls.lscv,lower,upper,y=y11,x=xx11,w=w11)

      # tym11 <- lcls.lscv0(h=bw.start,y=y11,x=xx11,w=w11)
      # tym12 <- lcls.lscv(h=bw.start,cores=cores,
      #                    y=y11, wy=wy11, w=w11, x=as.matrix(xx11),
      #                    xtype=q.typeYnum, nlevels=q.levels, n=n11, k=k.x)
      # cat.print(c(tym11,tym12))

      # return(1)

      bw.optim <- minqa::bobyqa(
        bw.start, lcls.lscv, lower, upper,
        cores = cores, y = y11, wy = wy11, w = w11, x = as.matrix(xx11),
        xtype = q.typeYnum, nlevels = q.levels, n = n11, k = k.x)


      # cat.print(bw.start)
      # cat.print(bw.optim)

      time.06 <- proc.time()
      CV.time.sec <- round( (time.06-time.05)[3], 0)
      names(CV.time.sec) <- "sec"

      if (print.level > 0){
        .timing(CV.time.sec, "Calculating cross-validated bandwidths completed in ")
        # cat("___________________________________________________\n")
      }

      # bw.optim$par
      # bw.optim$fval
      # bw.optim$feval
      # bw.optim$ierr

      bw11 <- bw.optim$par
      # if (print.level > 1) {
      #   cat(paste0("Calculating cross-validated bandwidths completed\n"))
      # }

      ## take these cross-validated bandwidths and calculate the scale factors, then get the remaining bandwidths
      sf <- rep(1,k.x)
      bw10 <- sf
      bw01 <- sf
      bw00 <- sf

      for (ii in 1:k.x){
        sf[ii] <-
          ifelse(
            is.factor(x[,ii]),
            bw11[ii]*(n11^(2/(4+q.type[1]))),
            bw11[ii]*(n11^(1/(4+q.type[1])))
          )
        bw10[ii] <-
          ifelse(
            is.factor(x[,ii]),
            sf[ii]*(n10^(-2/(4+q.type[1]))),
            sf[ii]*(n10^(-1/(4+q.type[1])))
          )
        bw01[ii] <-
          ifelse(
            is.factor(x[,ii]),
            sf[ii]*(n01^(-2/(4+q.type[1]))),
            sf[ii]*(n01^(-1/(4+q.type[1])))
          )
        bw00[ii] <-
          ifelse(
            is.factor(x[, ii]),
            sf[ii] * (n00 ^ (-2 / ( 4 + q.type[1] ))),
            sf[ii] * (n00 ^ (-1 / ( 4 + q.type[1] )))
          )
      }
    } else {
      ### plug-in ----
      if (print.level > 0){
        cat("Bandwidths are chosen via the plug-in method\n")
      }
      # bw11 <- rot.bw00

      ## plug-in bandwidths
      ## Silverman (1986) for cont (make it a function of q 1.06, 1.00, ....)
      ## Chu, Henderson and Parmeter (2015) for discrete
      rot.bw00 <- rot.bw01 <- rot.bw10 <- rot.bw11 <-  matrix(0,nrow=k.x,ncol=1)

      for (i in 1:k.x){

        # cat.print(i)

        # if (is.ordered(xx00[,i])==TRUE){
        if ( q.typeYnum[i] == 3 ){

          ## First equation on page 8 in CHP (2015) - calculating relative frequencies for plug-in bandwidth
          # cat.print(levels(xx00[,i]))
          rf <- table(droplevels(xx00[,i]))/length(xx00[,i])
          # cat.print(rf)
          rot.bw00[i] <- (1/(1 + (n00*sum((1-rf)^2/(sum(rf*(1-rf)))))))
          # cat.print(rot.bw00[i])
          rf <- table(droplevels(xx01[,i]))/length(xx01[,i])
          rot.bw01[i] <- (1/(1 + (n01*sum((1-rf)^2/(sum(rf*(1-rf)))))))
          rf <- table(droplevels(xx10[,i]))/length(xx10[,i])
          rot.bw10[i] <- (1/(1 + (n10*sum((1-rf)^2/(sum(rf*(1-rf)))))))
          rf <- table(droplevels(xx11[,i]))/length(xx11[,i])
          rot.bw11[i] <- (1/(1 + (n11*sum((1-rf)^2/(sum(rf*(1-rf)))))))

          # } else if (is.factor(xx00[,i])==TRUE) {
        } else if ( q.typeYnum[i] == 2 ) {

          ## First equation on page 8 in CHP (2015) - calculating relative frequencies for plug-in bandwidth
          rf <- table(droplevels(xx00[,i]))/length(xx00[,i])
          # cat.print(rf)
          if( length(rf) == 1) {
            stop("Variable ", colnames(x)[i], " has no variation\n")
          }
          rot.bw00[i] <- (1/(1 + (n00*sum((1-rf)^2/(sum(rf*(1-rf)))))))
          # cat.print(rot.bw00[i])
          rf <- table(droplevels(xx01[,i]))/length(xx01[,i])
          rot.bw01[i] <- (1/(1 + (n01*sum((1-rf)^2/(sum(rf*(1-rf)))))))
          rf <- table(droplevels(xx10[,i]))/length(xx10[,i])
          rot.bw10[i] <- (1/(1 + (n10*sum((1-rf)^2/(sum(rf*(1-rf)))))))
          rf <- table(droplevels(xx11[,i]))/length(xx11[,i])
          rot.bw11[i] <- (1/(1 + (n11*sum((1-rf)^2/(sum(rf*(1-rf)))))))

        } else {

          if( sd(xx00[,i]) == 0) {
            cat("Variable ", colnames(x)[i], " has no variation\n")
          }

          ## note: no sd(x) because we scaled them already
          rot.bw00[i] <- 1.06*n00^(-1/(4+q.type[1]))
          # cat.print(rot.bw00[i] )
          rot.bw01[i] <- 1.06*n01^(-1/(4+q.type[1]))
          rot.bw10[i] <- 1.06*n10^(-1/(4+q.type[1]))
          rot.bw11[i] <- 1.06*n11^(-1/(4+q.type[1]))

        }

      } ## i

      # print(rot.bw11)

      ## ROT bandwidths instead of LSCV
      bw11 <- rot.bw11
      bw10 <- rot.bw10
      bw01 <- rot.bw01
      bw00 <- rot.bw00

    }
  } else {
    if (print.level > 0) {
      cat("\n The bandwidths are provided.\n")
      cat("\n Make sure the bandwidths are chosen using the 'didnpreg' command.\n")
    }

    if(length(bws) != k.x){
      stop("The length of 'bws' is not equal to the number of regressors")
    }

    bw11 <- as.vector(as.matrix(bws))

    ## take these bandwidths and calculate the scale factors, then get the remaining bandwidths
    sf <- rep(1,k.x)
    bw10 <- sf
    bw01 <- sf
    bw00 <- sf

    for (ii in 1:k.x){
      sf[ii] <-
        ifelse(
          is.factor(x[,ii]),
          bw11[ii]*(n11^(2/(4+q.type[1]))),
          bw11[ii]*(n11^(1/(4+q.type[1])))
        )
      bw10[ii] <-
        ifelse(
          is.factor(x[,ii]),
          sf[ii]*(n10^(-2/(4+q.type[1]))),
          sf[ii]*(n10^(-1/(4+q.type[1])))
        )
      bw01[ii] <-
        ifelse(
          is.factor(x[,ii]),
          sf[ii]*(n01^(-2/(4+q.type[1]))),
          sf[ii]*(n01^(-1/(4+q.type[1])))
        )
      bw00[ii] <-
        ifelse(
          is.factor(x[, ii]),
          sf[ii] * (n00 ^ (-2 / ( 4 + q.type[1] ))),
          sf[ii] * (n00 ^ (-1 / ( 4 + q.type[1] )))
        )
    }
  }

  # return(1)

  ## print bws ----

  my.bw <- data.frame(Regressor = colnames(x), Type = q.typeY, Bandwidth = bw11)
  if (print.level > 0) {
    cat("\n")
    print(my.bw)
  }


  ## Calculating ATET ----

  time.05 <- proc.time()

  if (print.level > 0) {
    if (do.TTb) {
      # my.atet <- paste0("TTa and TTb (may take some time)")
      my.atet <- paste0("TTb")
    } else {
      my.atet <- paste0("TTa")
    }
    cat(paste0("\nCalculating ATET: ",my.atet,"\n"))
  }

  # cat.print(dim(xx11))
  # cat.print(dim(xx10))
  # cat.print(dim(xx01))
  # cat.print(dim(xx00))

  # cat.print(
  #   data.frame(rbind(
  #     as.vector(bw11),
  #     bw10,
  #     bw01,
  #     bw00
  #   ))
  # )

  # cat.print(bw11)
  # cat.print(dim(xx11))
  # cat.print(bw10)
  # cat.print(dim(xx10))
  # cat.print(bw01)
  # cat.print(dim(xx01))
  # cat.print(bw00)
  # cat.print(dim(xx00))

  if (do.TTb) {
    # num11 <- np::npksum(txdat=xx11,tydat=wy11,exdat=xx1,bws=bw11)$ksum
    # num10 <- np::npksum(txdat=xx10,tydat=wy10,exdat=xx1,bws=bw10)$ksum
    # num01 <- np::npksum(txdat=xx01,tydat=wy01,exdat=xx1,bws=bw01)$ksum
    # num00 <- np::npksum(txdat=xx00,tydat=wy00,exdat=xx1,bws=bw00)$ksum
    #
    # dem11 <- np::npksum(txdat=xx11,tydat=w11,exdat=xx1,bws=bw11)$ksum
    # dem10 <- np::npksum(txdat=xx10,tydat=w10,exdat=xx1,bws=bw10)$ksum
    # dem01 <- np::npksum(txdat=xx01,tydat=w01,exdat=xx1,bws=bw01)$ksum
    # dem00 <- np::npksum(txdat=xx00,tydat=w00,exdat=xx1,bws=bw00)$ksum
    num11 <- .npksumYXnew(
      Nthreds = cores,
      ydat=wy11, xdat=as.matrix(xx11), xeval=as.matrix(xx1), bw=bw11,
      xtype=q.typeYnum, nlevels=q.levels, n=n11, neval=n1, q=k.x
    )
    # cat.print(num11[1:20])
    # cat.print(q.typeYnum)
    # cat.print(q.levels)
    # cat.print(num11_[1:20])
    # cat.print( data.frame(num11, num11_, num11-num11_)[1:20,] )


    num10 <- .npksumYXnew(
      Nthreds = cores,
      ydat=wy10, xdat=as.matrix(xx10), xeval=as.matrix(xx1), bw=bw10,
      xtype=q.typeYnum, nlevels=q.levels, n=n10, neval=n1, q=k.x
    )
    # cat.print( data.frame(num10, num10_,num10- num10_)[1:20,] )

    num01 <- .npksumYXnew(
      Nthreds = cores,
      ydat=wy01, xdat=as.matrix(xx01), xeval=as.matrix(xx1), bw=bw01,
      xtype=q.typeYnum, nlevels=q.levels, n=n01, neval=n1, q=k.x
    )
    # cat.print( data.frame(num01, num01_,num01- num01_)[1:20,] )

    num00 <- .npksumYXnew(
      Nthreds = cores,
      ydat=wy00, xdat=as.matrix(xx00), xeval=as.matrix(xx1), bw=bw00,
      xtype=q.typeYnum, nlevels=q.levels, n=n00, neval=n1, q=k.x
    )
    # cat.print( data.frame(num00, num00_,num00- num00_)[1:20,] )



    dem11 <- .npksumYXnew(
      Nthreds = cores,
      ydat=w11, xdat=as.matrix(xx11), xeval=as.matrix(xx1), bw=bw11,
      xtype=q.typeYnum, nlevels=q.levels, n=n11, neval=n1, q=k.x
    )
    # cat.print( data.frame(dem11, dem11_,dem11- dem11_)[1:20,] )

    dem10 <- .npksumYXnew(
      Nthreds = cores,
      ydat=w10, xdat=as.matrix(xx10), xeval=as.matrix(xx1), bw=bw10,
      xtype=q.typeYnum, nlevels=q.levels, n=n10, neval=n1, q=k.x
    )
    # cat.print( data.frame(dem10, dem10_,dem10- dem10_)[1:20,] )

    dem01 <- .npksumYXnew(
      Nthreds = cores,
      ydat=w01, xdat=as.matrix(xx01), xeval=as.matrix(xx1), bw=bw01,
      xtype=q.typeYnum, nlevels=q.levels, n=n01, neval=n1, q=k.x
    )
    # cat.print( data.frame(dem01, dem01_,dem01- dem01_)[1:20,] )

    dem00 <- .npksumYXnew(
      Nthreds = cores,
      ydat=w00, xdat=as.matrix(xx00), xeval=as.matrix(xx1), bw=bw00,
      xtype=q.typeYnum, nlevels=q.levels, n=n00, neval=n1, q=k.x
    )
    # cat.print( data.frame(dem00, dem00_,dem00- dem00_)[1:20,] )
  } else {
    # num11_ <- np::npksum(txdat=xx11,tydat=wy11,exdat=xx11,bws=bw11)$ksum
    # num10_ <- np::npksum(txdat=xx10,tydat=wy10,exdat=xx11,bws=bw10)$ksum
    # num01_ <- np::npksum(txdat=xx01,tydat=wy01,exdat=xx11,bws=bw01)$ksum
    # num00_ <- np::npksum(txdat=xx00,tydat=wy00,exdat=xx11,bws=bw00)$ksum
    # dem11_ <- np::npksum(txdat=xx11,tydat=w11,exdat=xx11,bws=bw11)$ksum
    # dem10_ <- np::npksum(txdat=xx10,tydat=w10,exdat=xx11,bws=bw10)$ksum
    # dem01_ <- np::npksum(txdat=xx01,tydat=w01,exdat=xx11,bws=bw01)$ksum
    # dem00_ <- np::npksum(txdat=xx00,tydat=w00,exdat=xx11,bws=bw00)$ksum
    # cat.print(cores)

    num11 <- .npksumYXnew(
      Nthreds = cores,
      ydat=wy11, xdat=as.matrix(xx11), xeval=as.matrix(xx11), bw=bw11,
      xtype=q.typeYnum, nlevels=q.levels, n=n11, neval=n11, q=k.x
    )
    # cat.print(num11[1:20])
    # cat.print(q.typeYnum)
    # cat.print(q.levels)
    # cat.print(num11_[1:20])
    # cat.print( data.frame(num11, num11_, num11-num11_)[1:20,] )


    num10 <- .npksumYXnew(
      Nthreds = cores,
      ydat=wy10, xdat=as.matrix(xx10), xeval=as.matrix(xx11), bw=bw10,
      xtype=q.typeYnum, nlevels=q.levels, n=n10, neval=n11, q=k.x
    )
    # cat.print( data.frame(num10, num10_,num10- num10_)[1:20,] )

    num01 <- .npksumYXnew(
      Nthreds = cores,
      ydat=wy01, xdat=as.matrix(xx01), xeval=as.matrix(xx11), bw=bw01,
      xtype=q.typeYnum, nlevels=q.levels, n=n01, neval=n11, q=k.x
    )
    # cat.print( data.frame(num01, num01_,num01- num01_)[1:20,] )

    num00 <- .npksumYXnew(
      Nthreds = cores,
      ydat=wy00, xdat=as.matrix(xx00), xeval=as.matrix(xx11), bw=bw00,
      xtype=q.typeYnum, nlevels=q.levels, n=n00, neval=n11, q=k.x
    )
    # cat.print( data.frame(num00, num00_,num00- num00_)[1:20,] )



    dem11 <- .npksumYXnew(
      Nthreds = cores,
      ydat=w11, xdat=as.matrix(xx11), xeval=as.matrix(xx11), bw=bw11,
      xtype=q.typeYnum, nlevels=q.levels, n=n11, neval=n11, q=k.x
    )
    # cat.print( data.frame(dem11, dem11_,dem11- dem11_)[1:20,] )

    dem10 <- .npksumYXnew(
      Nthreds = cores,
      ydat=w10, xdat=as.matrix(xx10), xeval=as.matrix(xx11), bw=bw10,
      xtype=q.typeYnum, nlevels=q.levels, n=n10, neval=n11, q=k.x
    )
    # cat.print( data.frame(dem10, dem10_,dem10- dem10_)[1:20,] )

    dem01 <- .npksumYXnew(
      Nthreds = cores,
      ydat=w01, xdat=as.matrix(xx01), xeval=as.matrix(xx11), bw=bw01,
      xtype=q.typeYnum, nlevels=q.levels, n=n01, neval=n11, q=k.x
    )
    # cat.print( data.frame(dem01, dem01_,dem01- dem01_)[1:20,] )

    dem00 <- .npksumYXnew(
      Nthreds = cores,
      ydat=w00, xdat=as.matrix(xx00), xeval=as.matrix(xx11), bw=bw00,
      xtype=q.typeYnum, nlevels=q.levels, n=n00, neval=n11, q=k.x
    )
    # cat.print( data.frame(dem00, dem00_,dem00- dem00_)[1:20,] )
  }

  atet <- mean(num11/dem11 - num10/dem10 - num01/dem01 + num00/dem00)
  # cat("101\n")
  # cat.print(mean(num11/dem11 - num10/dem10 - num01/dem01 + num00/dem00))
  # cat.print(mean(num11_/dem11_ - num10_/dem10_ - num01_/dem01_ + num00_/dem00_))
  # atet.hetero <- as.matrix(num11/dem11 - num10/dem10 - num01/dem01 + num00/dem00)

  if (do.TTb) {
    TTb.i <- as.vector(num11/dem11 - num10/dem10 - num01/dem01 + num00/dem00)
    # cat.print(length(TTb.i))
    TTa.i <- TTb.i[TTa.positions.in.TTb]
    # cat.print(length(TTa.i))
    TTa <- mean(TTa.i)
    TTb <- mean(TTb.i)

    # cat.print(mean(TTb.i))
    # cat.print(mean(TTb.i, na.rm = TRUE))
    # cat.print(TTa)
    # cat.print(length(TTa.i))
    # cat.print(TTb)
    # cat.print(length(TTb.i))
    # if (print.level > 0 ) {
    #   cat(paste0("TTa = ",formatC(TTa, digits = 4),", N(TTa) = ",n11,"\n"))
    # }
    if (print.level > 0) {
      cat(paste0("TTb = ",formatC(TTb, digits = digits),", N (TTb; all treated) = ",n1,"\n"))
    }
  } else {
    TTa.i <- as.vector(num11/dem11 - num10/dem10 - num01/dem01 + num00/dem00)
    TTa <- mean(TTa.i)
    # cat.print(mean(TTa.i))
    # cat.print(mean(TTa.i, na.rm = TRUE))

    if (print.level > 0) {
      cat(paste0("TTa = ",formatC(TTa, digits = digits),", N (TTa; treated in the first period or N_ 1, 1) = ",n11,"\n"))
    }

    # cat.print(TTa)
    # cat.print(length(TTa.i))
  }

  # if (print.level > 1) {
  #   cat(paste0("Calculating ATET completed\n"))
  # }

  time.06 <- proc.time()
  ATET.time.sec <- round( (time.06-time.05)[3], 0)
  names(ATET.time.sec) <- "sec"

  if (print.level > 0){
    .timing(ATET.time.sec, "Calculating ATET completed in ")
    # cat("___________________________________________________\n")
  }

  # return(1)

  # tym <- cbind(
  #   num11,dem11 , num10,dem10 , num01,dem01 , num00,dem00
  # )
  # cat.print(tym[1:20,])
  #
  # cat.print(mean(num11/dem11 - num10/dem10 - num01/dem01 + num00/dem00))
  # cat.print(mean(num11/dem11 - num10/dem10 - num01/dem01 + num00/dem00, na.rm = TRUE))
  #
  # cat.print(head(xx11))
  # cat.print(head(wy11))
  # cat.print(head(xx1))
  # cat.print(bw11)

  ## Bootstraping ATET ----


  ## begin the bootstrap for atet
  if (print.level > 0) {
    cat(paste0("\nBootstrapping standard errors (",boot.num," replications)\n"))
  }
  set.seed(seed = seed)#, kind = "L'Ecuyer-CMRG")
  seeds <- sample.int(n = .Machine$integer.max, size = boot.num)

  ## need to evaluate the data at the observations to get the residuals
  # if (print.level > 1) {
  #   cat(paste0("Calculating residuals\n"))
  # }


  # nnum11 <- np::npksum(txdat=xx11,tydat=wy11,exdat=xx11,bws=bw11)$ksum
  # nnum10 <- np::npksum(txdat=xx10,tydat=wy10,exdat=xx10,bws=bw10)$ksum
  # nnum01 <- np::npksum(txdat=xx01,tydat=wy01,exdat=xx01,bws=bw01)$ksum
  # nnum00 <- np::npksum(txdat=xx00,tydat=wy00,exdat=xx00,bws=bw00)$ksum


  nnum11 <- .npksumYX(
    Nthreds = cores,
    ydat=wy11, xdat=as.matrix(xx11), bw=bw11,
    xtype=q.typeYnum, nlevels=q.levels, n=n11, q=k.x
  )
  nnum10 <- .npksumYX(
    Nthreds = cores,
    ydat=wy10, xdat=as.matrix(xx10), bw=bw10,
    xtype=q.typeYnum, nlevels=q.levels, n=n10, q=k.x
  )
  nnum01 <- .npksumYX(
    Nthreds = cores,
    ydat=wy01, xdat=as.matrix(xx01), bw=bw01,
    xtype=q.typeYnum, nlevels=q.levels, n=n01, q=k.x
  )
  nnum00 <- .npksumYX(
    Nthreds = cores,
    ydat=wy00, xdat=as.matrix(xx00), bw=bw00,
    xtype=q.typeYnum, nlevels=q.levels, n=n00, q=k.x
  )

  # ddem11 <- np::npksum(txdat=xx11,tydat=w11,exdat=xx11,bws=bw11)$ksum
  # ddem10 <- np::npksum(txdat=xx10,tydat=w10,exdat=xx10,bws=bw10)$ksum
  # ddem01 <- np::npksum(txdat=xx01,tydat=w01,exdat=xx01,bws=bw01)$ksum
  # ddem00 <- np::npksum(txdat=xx00,tydat=w00,exdat=xx00,bws=bw00)$ksum


  ddem11 <- .npksumYX(
    Nthreds = cores,
    ydat=w11, xdat=as.matrix(xx11), bw=bw11,
    xtype=q.typeYnum, nlevels=q.levels, n=n11, q=k.x
  )
  ddem10 <- .npksumYX(
    Nthreds = cores,
    ydat=w10, xdat=as.matrix(xx10), bw=bw10,
    xtype=q.typeYnum, nlevels=q.levels, n=n10, q=k.x
  )
  ddem01 <- .npksumYX(
    Nthreds = cores,
    ydat=w01, xdat=as.matrix(xx01), bw=bw01,
    xtype=q.typeYnum, nlevels=q.levels, n=n01, q=k.x
  )
  ddem00 <- .npksumYX(
    Nthreds = cores,
    ydat=w00, xdat=as.matrix(xx00), bw=bw00,
    xtype=q.typeYnum, nlevels=q.levels, n=n00, q=k.x
  )

  # cat.print( data.frame(nnum11, nnum11_, nnum11 - nnum11_)[1:20,] )
  # cat.print( data.frame(nnum10, nnum10_, nnum10 - nnum10_)[1:20,] )
  # cat.print( data.frame(nnum01, nnum01_, nnum01 - nnum01_)[1:20,] )
  # cat.print( data.frame(nnum00, nnum00_, nnum00 - nnum00_)[1:20,] )
  #
  # cat.print( data.frame(ddem11, ddem11_, ddem11 - ddem11_)[1:20,] )
  # cat.print( data.frame(ddem10, ddem10_, ddem10 - ddem10_)[1:20,] )
  # cat.print( data.frame(ddem01, ddem01_, ddem01 - ddem01_)[1:20,] )
  # cat.print( data.frame(ddem00, ddem00_, ddem00 - ddem00_)[1:20,] )

  # return(2)

  tt11 <- nnum11/ddem11
  tt10 <- nnum10/ddem10
  tt01 <- nnum01/ddem01
  tt00 <- nnum00/ddem00

  resid.11 <- y11 - tt11
  resid.10 <- y10 - tt10
  resid.01 <- y01 - tt01
  resid.00 <- y00 - tt00

  if (print.level > 1) {
    cat(paste0("Calculating residuals completed\n"))
  }

  time.05 <- proc.time()

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

  cores1 = 1
  # doParallel::registerDoParallel(cores = cores1)

  # if (print.level > 0 & cores1 == 1)
  # {
  #   pb <- utils::txtProgressBar(min = 0, max = boot.num, style = 3)
  # }

  # mcoptions <- list(set.seed = TRUE)

  atet.boot.hetero <- matrix(NA, nrow = length(dem11), ncol = boot.num)

  for(j in 1:boot.num){

    # atet.boot.hetero <-
    #   foreach::foreach(j = 1:boot.num, .options.multicore = mcoptions, .combine = "cbind", .verbose = FALSE) %dopar% {

    set.seed(seeds[j])

    ## bootstrap if the y variable is binary
    if (is.binary(y) == TRUE) {

      y11.new <- ifelse(tt11 > runif(n11, min = 0, max = 1),1,0)
      y10.new <- ifelse(tt10 > runif(n10, min = 0, max = 1),1,0)
      y01.new <- ifelse(tt01 > runif(n01, min = 0, max = 1),1,0)
      y00.new <- ifelse(tt00 > runif(n00, min = 0, max = 1),1,0)

    } else {

      y11.new <- tt11 + resid.11*rnorm(n11,0,1)
      y10.new <- tt10 + resid.10*rnorm(n10,0,1)
      y01.new <- tt01 + resid.01*rnorm(n01,0,1)
      y00.new <- tt00 + resid.00*rnorm(n00,0,1)

    }

    wy11.new <- as.vector(w11*y11.new)
    wy10.new <- as.vector(w10*y10.new)
    wy01.new <- as.vector(w01*y01.new)
    wy00.new <- as.vector(w00*y00.new)

    if (do.TTb) {
      # num11.new <- np::npksum(txdat=xx11,tydat=wy11.new,exdat=xx1,bws=bw11)$ksum
      # num10.new <- np::npksum(txdat=xx10,tydat=wy10.new,exdat=xx1,bws=bw10)$ksum
      # num01.new <- np::npksum(txdat=xx01,tydat=wy01.new,exdat=xx1,bws=bw01)$ksum
      # num00.new <- np::npksum(txdat=xx00,tydat=wy00.new,exdat=xx1,bws=bw00)$ksum

      num11.new <- .npksumYXnew(
        Nthreds = cores,
        ydat=wy11.new, xdat=as.matrix(xx11), xeval=as.matrix(xx1), bw=bw11,
        xtype=q.typeYnum, nlevels=q.levels, n=n11, neval=n1, q=k.x
      )
      num10.new <- .npksumYXnew(
        Nthreds = cores,
        ydat=wy10.new, xdat=as.matrix(xx10), xeval=as.matrix(xx1), bw=bw10,
        xtype=q.typeYnum, nlevels=q.levels, n=n10, neval=n1, q=k.x
      )
      num01.new <- .npksumYXnew(
        Nthreds = cores,
        ydat=wy01.new, xdat=as.matrix(xx01), xeval=as.matrix(xx1), bw=bw01,
        xtype=q.typeYnum, nlevels=q.levels, n=n01, neval=n1, q=k.x
      )
      num00.new <- .npksumYXnew(
        Nthreds = cores,
        ydat=wy00.new, xdat=as.matrix(xx00), xeval=as.matrix(xx1), bw=bw00,
        xtype=q.typeYnum, nlevels=q.levels, n=n00, neval=n1, q=k.x
      )

    } else {
      # num11.new <- np::npksum(txdat=xx11,tydat=wy11.new,exdat=xx11,bws=bw11)$ksum
      # num10.new <- np::npksum(txdat=xx10,tydat=wy10.new,exdat=xx11,bws=bw10)$ksum
      # num01.new <- np::npksum(txdat=xx01,tydat=wy01.new,exdat=xx11,bws=bw01)$ksum
      # num00.new <- np::npksum(txdat=xx00,tydat=wy00.new,exdat=xx11,bws=bw00)$ksum

      num11.new <- .npksumYXnew(
        Nthreds = cores,
        ydat=wy11.new, xdat=as.matrix(xx11), xeval=as.matrix(xx11), bw=bw11,
        xtype=q.typeYnum, nlevels=q.levels, n=n11, neval=n11, q=k.x
      )
      num10.new <- .npksumYXnew(
        Nthreds = cores,
        ydat=wy10.new, xdat=as.matrix(xx10), xeval=as.matrix(xx11), bw=bw10,
        xtype=q.typeYnum, nlevels=q.levels, n=n10, neval=n11, q=k.x
      )
      num01.new <- .npksumYXnew(
        Nthreds = cores,
        ydat=wy01.new, xdat=as.matrix(xx01), xeval=as.matrix(xx11), bw=bw01,
        xtype=q.typeYnum, nlevels=q.levels, n=n01, neval=n11, q=k.x
      )
      num00.new <- .npksumYXnew(
        Nthreds = cores,
        ydat=wy00.new, xdat=as.matrix(xx00), xeval=as.matrix(xx11), bw=bw00,
        xtype=q.typeYnum, nlevels=q.levels, n=n00, neval=n11, q=k.x
      )

    }

    # if (print.level > 0 & cores == 1)
    # {
    #   pb <- utils::txtProgressBar(min = 0, max = boot.num, style = 3)
    # }
    if (print.level > 0 & cores1 == 1 & j == 1)
    {
      time.06 <- proc.time()
      boot.time.sec <- round( boot.num*(time.06-time.05)[3], 0)
      .timing(boot.time.sec, "\nBootstrapping will take approximately: ")
      cat("\n")
      pb <- utils::txtProgressBar(min = 1, max = boot.num, style = 3)
    }

    if (print.level > 0 & cores1 == 1 & j > 1) utils::setTxtProgressBar(pb, j)

    # it does not matter if 'TTa' or 'TTb', since denominators are
    # not bootstrapped and are already calculated
    # num11.new/dem11 - num10.new/dem10 - num01.new/dem01 + num00.new/dem00
    atet.boot.hetero[,j] <- num11.new/dem11 - num10.new/dem10 - num01.new/dem01 + num00.new/dem00
  }

  # do parallel completed
  if (print.level > 0 & cores1 == 1) cat("\n")

  time.06 <- proc.time()
  boot.time.sec <- round( (time.06-time.05)[3], 0)
  names(boot.time.sec) <- "sec"

  if (print.level >= 1){
    .timing(boot.time.sec, "Bootstrapping standard errors completed in ")
    # cat("___________________________________________________\n")
  }

  atet.boot <- colMeans(atet.boot.hetero)

  if (do.TTb) {
    TTa.se <- sd(atet.boot[TTa.positions.in.TTb])
    TTb.se <- sd(atet.boot)
    # cat.print(TTa.se)
    # cat.print(TTb.se)
    if (print.level > 0) {
      # cat("\nTTa sd =",formatC(TTa.se, digits = 4),"\n")
      cat("\n")
      # cat(paste0("TTb = ",formatC(TTb, digits = digits),"\n"))
      cat("TTb bootstrapped standard error =",formatC(TTb.se, digits = digits),"\n")
      qu1 <- quantile(atet.boot, probs = c((1-level/100)/2))
      qu2 <- quantile(atet.boot, probs = c((1+level/100)/2))
      atets.boot <- cbind(round(TTb, digits = digits),
                          round(TTb.se, digits = digits),
                          round(qu1, digits = digits),
                          round(qu2, digits = digits))
      colnames(atets.boot) <- c("Coef.", "SE ", "CIl", "CIu")

      qu1 <-ifelse(qu1 > 999,
                   formatC(qu1, digits=1, format="e"),
                   formatC(qu1, digits=digits, format="f"))
      qu2 <-ifelse(qu2 > 999,
                   formatC(qu2, digits=1, format="e"),
                   formatC(qu2, digits=digits, format="f"))
      # cat("TTb bootstrapped ",level,"% confidence interval: [",qu1,", ",qu2,"]\n", sep = "")
    }
    atets <- cbind(round(TTb, digits = digits),
                   round(TTb.se, digits = digits),
                   round(TTb/TTb.se,digits = 2),
                   round(pnorm(abs(TTb/TTb.se), lower.tail = FALSE)*2, digits = digits),
                   round(TTb-qnorm((1+level/100)/2)*TTb.se, digits = digits),
                   round(TTb+qnorm((1+level/100)/2)*TTb.se, digits = digits))
    rownames(atets) <- rownames(atets.boot) <- "ATET (TTb)"
    colnames(atets) <- c("Coef.", "SE ", "z ",  "P>|z|", "CIl", "CIu")
    max.name.length <- max(nchar(row.names(atets)))
    mycutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1)
    mysymbols = c("***", "**", "*", ".", " ")
    na.print = "NA"
  } else {
    TTa.se <- sd(atet.boot)
    # cat.print(TTa.se)
    if (print.level > 0) {
      cat("\n")
      # cat(paste0("TTa =\n ",formatC(TTa, digits = digits),"\n"))
      cat("TTa bootstrapped standard error = ",formatC(TTa.se, digits = digits),"\n")
      qu1 <- quantile(atet.boot, probs = c((1-level/100)/2))
      qu2 <- quantile(atet.boot, probs = c((1+level/100)/2))
      atets.boot <- cbind(round(TTa, digits = digits),
                          round(TTa.se, digits = digits),
                          round(qu1, digits = digits),
                          round(qu2, digits = digits))
      colnames(atets.boot) <- c("Coef.", "SE ", "CIl", "CIu")

      qu1 <-ifelse(qu1 > 999,
                   formatC(qu1, digits=1, format="e"),
                   formatC(qu1, digits=digits, format="f"))
      qu2 <-ifelse(qu2 > 999,
                   formatC(qu2, digits=1, format="e"),
                   formatC(qu2, digits=digits, format="f"))
      # cat("\nTTa bootstrapped ",level,"% confidence interval:\n [",qu1,", ",qu2,"]\n", sep = "")
    }
    atets <- cbind(round(TTa, digits = digits),
                   round(TTa.se, digits = digits),
                   round(TTa/TTa.se,digits = 2),
                   round(pnorm(abs(TTa/TTa.se), lower.tail = FALSE)*2, digits = digits),
                   round(TTa-qnorm((1+level/100)/2)*TTa.se, digits = digits),
                   round(TTa+qnorm((1+level/100)/2)*TTa.se, digits = digits))
    rownames(atets) <- rownames(atets.boot) <- "ATET (TTa)"
    colnames(atets) <- c("Coef.", "SE ", "z ",  "P>|z|", "CIl", "CIu")
  }

  if (print.level > 0) {
    cat("\nBootstrapped confidence interval:\n\n")

    Cf <- cbind(
      ifelse(atets.boot[,1, drop = FALSE]> 999,
             formatC(atets.boot[,1, drop = FALSE], digits=1, format="e",width=10),
             formatC(atets.boot[,1, drop = FALSE], digits=digits, format="f", width=10)),
      ifelse(atets.boot[,2, drop = FALSE]>999,
             formatC(atets.boot[,2, drop = FALSE], digits=1, format="e", width=10),
             formatC(atets.boot[,2, drop = FALSE], digits=digits, format="f", width=10)),
      ifelse(atets.boot[,3, drop = FALSE]> 999,
             formatC(atets.boot[,3, drop = FALSE], digits=1, format="e", width=12),
             formatC(atets.boot[,3, drop = FALSE], digits=digits, format="f", width=12, flag = "-")),
      ifelse(atets.boot[,4, drop = FALSE]> 999,
             formatC(atets.boot[,4, drop = FALSE], digits=1, format="e",width=12),
             formatC(atets.boot[,4, drop = FALSE], digits=digits, format="f", width=12))
    )

    max.name.length <- max(nchar(row.names(atets)))
    mycutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1)
    mysymbols = c("***", "**", "*", ".", " ")
    na.print = "NA"

    row.names(Cf) <- formatC(row.names(Cf), width = max(nchar(row.names(Cf))), flag = "-")
    cat("",rep(" ", max.name.length+6),"Coef.        SE  [",level,"% confidence interval]", sep = "")

    # cat.print(Cf)
    dimnames(Cf)[[2]] <- rep("", dim(Cf)[[2]])
    print.default(Cf[1:1,,drop=FALSE], quote = FALSE, right = TRUE, na.print = na.print)
  }

    if (print.level > 0) {
    if (do.TTb) {
      cat("\n\np-value and confidence interval assuming ATET (TTb) is normally distributed:\n\n")
    } else {
      cat("\n\np-value and confidence interval assuming ATET (TTa) is normally distributed:\n\n")
    }


    Cf <- cbind(
      ifelse(atets[,1, drop = FALSE]> 999,
             formatC(atets[,1, drop = FALSE], digits=1, format="e",width=10),
             formatC(atets[,1, drop = FALSE], digits=digits, format="f", width=10)),
      ifelse(atets[,2, drop = FALSE]>999,
             formatC(atets[,2, drop = FALSE], digits=1, format="e", width=10),
             formatC(atets[,2, drop = FALSE], digits=digits, format="f", width=10)),
      ifelse(atets[,3, drop = FALSE]>999,
             formatC(atets[,3, drop = FALSE], digits=1, format="e", width=7),
             formatC(atets[,3, drop = FALSE], digits=2, format="f", width=7)),
      ifelse(atets[,4, drop = FALSE]>999,
             formatC(atets[,4, drop = FALSE], digits=1, format="e", width=10),
             formatC(atets[,4, drop = FALSE], digits=digits, format="f", width=10)),
      # formatC(mysymbols[findInterval(x = atets[,4], vec = mycutpoints)], flag = "-"),
      ifelse(atets[,5, drop = FALSE]> 999,
             formatC(atets[,5, drop = FALSE], digits=1, format="e", width=12),
             formatC(atets[,5, drop = FALSE], digits=digits, format="f", width=12, flag = "-")),
      ifelse(atets[,6, drop = FALSE]> 999,
             formatC(atets[,6, drop = FALSE], digits=1, format="e",width=12),
             formatC(atets[,6, drop = FALSE], digits=digits, format="f", width=12))
    )

    max.name.length <- max(nchar(row.names(atets)))
    mycutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1)
    mysymbols = c("***", "**", "*", ".", " ")
    na.print = "NA"

    row.names(Cf) <- formatC(row.names(Cf), width = max(nchar(row.names(Cf))), flag = "-")
    cat("",rep(" ", max.name.length+6),"Coef.        SE       z      P>|z|  [",level,"% confidence interval]", sep = "")


    # cat.print(Cf)
    dimnames(Cf)[[2]] <- rep("", dim(Cf)[[2]])
    print.default(Cf[1:1,,drop=FALSE], quote = FALSE, right = TRUE, na.print = na.print)
  }


  # if (print.level > 0) cat("\n")

  # cat("11\n")

  # sd.atet <- sd(atet.boot)

  # cat("12\n")

  ## Return ----

  ## let's return the objects npdid,npdid.se,rot.bw,lscv.bw

  if (do.TTb) {
    tymch <- list(
      NT = nt.o,
      esample = esample,
      sample1 = smpl1,
      sample11 = smpl11,
      sample10 = smpl10,
      sample01 = smpl01,
      sample00 = smpl00,
      n11 = n11,
      n10 = n10,
      n01 = n01,
      n00 = n00,
      regressor.type = q.type,
      bwmethod = bwmethod,
      bw.time = ifelse(bwmethod == "CV", CV.time.sec, 0),
      bws = my.bw,
      boot.time = boot.time.sec,
      boot.num = boot.num,
      bw11 = as.vector(bw11),
      bw10 = bw10,
      bw01 = bw01,
      bw00 = bw00,
      TTx = TTx,
      TTa.positions.in.TTb = TTa.positions.in.TTb,
      TTa.i = TTa.i,
      TTa = TTa,
      TTb.i = TTb.i,
      TTb = TTb,
      TTa.se = TTa.se,
      TTb.se = TTb.se,
      TTa.i.boot = atet.boot.hetero[TTa.positions.in.TTb,],
      TTb.i.boot = atet.boot.hetero
    )
  } else {
    tymch <- list(
      NT = nt.o,
      esample = esample,
      sample1 = smpl1,
      sample11 = smpl11,
      sample10 = smpl10,
      sample01 = smpl01,
      sample00 = smpl00,
      n11 = n11,
      n10 = n10,
      n01 = n01,
      n00 = n00,
      regressor.type = q.type,
      bwmethod = bwmethod,
      bw.time = ifelse(bwmethod == "CV", CV.time.sec, 0),
      bws = my.bw,
      boot.time = boot.time.sec,
      boot.num = boot.num,
      bw11 = as.vector(bw11),
      bw10 = bw10,
      bw01 = bw01,
      bw00 = bw00,
      TTx = TTx,
      TTa.i = TTa.i,
      TTa = TTa,
      TTa.se = TTa.se,
      TTa.i.boot = atet.boot.hetero
    )
  }

  # cat("13\n")

  class(tymch) <- c("didnpreg", "didnp")
  return(tymch)

}
