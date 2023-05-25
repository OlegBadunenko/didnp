# R functions for the didnp package

# https://stackoverflow.com/questions/7198758/roxygen2-how-to-properly-document-s3-methods
# roxygen2::roxygenize()
# https://pkgdown.r-lib.org/
# Run once to configure your package to use pkgdown
# usethis::use_pkgdown()
# usethis::use_pkgdown_github_pages()
# pkgdown::build_site()


#' Treatment Effect Estimators
#'
#' The \code{didnpreg} command contains tools for computing both heterogenous and average treatment effects for the treated in a model-free differences-in-differences framework.
#'
#' @param formula an object of class formula (or one that can be coerced to that class): a symbolic description of the model. The details of model specification are given under `Details'
#' @param data, # name of the data frame; must be specified if the 'formula' method is used
#' @param subset NULL, optional subsample of 'data'
#' @param outcome a vector, matrix, or data frame of length \eqn{NT}. The outcome can be continuous or dummy.
#' @param regressors a data frame with \eqn{NT} rows that contains regressors.
#' A data frame is required to identify the type/class of each regressor.
#' @param id a vector, matrix, or data frame of length \eqn{NT} that identifies the unit of observation.
#' @param time a vector, matrix, or data frame of length \eqn{NT} that specifies in which period \code{id} is observed.
#' @param treatment a vector, matrix, or data frame of length \eqn{NT} with zeros for the control and ones for the treated observations.
#' @param treatment_period a vector, matrix, or data frame of length \eqn{NT} with zeros for the period before treatment and ones for the period of treatment and after.
#' @param weights NULL,
#' @param boot.num 399,
#' @param bwmethod "opt", # plug-in is rule of thumb for continuous and basic for categorical, can be cross-validation
#' @param TTx FALSE
#' @param TTb FALSE
#' @param print.level the level of printing; larger number implies more output is printed. Default is 1. 0 suppresses all printing.
#' @param cores Integer specifies the number of cores to be used for parallel computation.
#'
#'
#' @details
#' All estimations are based. y ~ x1 + x2 | id | time | treatment | treatment_period | weights
#'
#'
#'
#' @return \code{didnpreg} returns a list containing:
#' \tabular{ll}{
#'    \code{esample}
#'    \tab A vector of TRUE/FALSE values identifying observations used in estimation. Relevant for the 'formula' method but complete cases will also be checked in the matrix method
#'    \cr \tab \cr
#'    \code{esample_treated}
#'    \tab A vector of TRUE/FALSE values identifying observations used in estimation and is treated (intersection of \code{esample} and \code{treatment})
#'    \cr \tab \cr
#'    \code{esample_TTa}
#'    \tab A vector of TRUE/FALSE values identifying observations used in estimation of TTa)
#'    \cr \tab \cr
#'    \code{esample_TTb}
#'    \tab A vector of TRUE/FALSE values identifying observations used in estimation of TTb. Returned only if \code{TTb = TRUE})
#'    \cr \tab \cr
#'    \code{N}
#'    \tab number of units of obs, defined by the \code{id}
#'    \cr \tab \cr
#'    \code{N_dt}
#'    \tab the number of observations used as training data for each conditional expectation
#'    \cr \tab \cr
#'    \code{T_i}
#'    \tab number of observations in each \code{id}
#'    \cr \tab \cr
#'    \code{bws}
#'    \tab bandwidth(s) for the data
#'    \cr \tab \cr
#'    \code{TTx}
#'    \tab the DiD estimators of the conditional TT (also known as CATET) (equation 2.12)
#'    \cr \tab \cr
#'    \code{TTa}
#'    \tab
#'    \cr \tab \cr
#'    \code{TTb}
#'    \tab NULL is option TTaOnly is used
#'    \cr
#' }
#'
#'
#' @keywords did np
#'
#' @examples
#' \dontrun{
#'   didnpreg(jhklangkjhakgkjh)
#' }
#'
#' @references
#' ... (...). This. \emph{Journal of },
#' \bold{1}(1), 1-1
#' \url{https://doi.org/10.}
#'
#' @author
#' Oleg Badunenko \email{oleg.badunenko@@brunel.ac.uk},
#' Daniel J. Henderson \email{djhender@@cba.ua.edu},
#' Stefan Sperlich \email{stefan.sperlich@@unige.ch}
#'
#'
#'
# didnpreg <- function(
    #     formula, # {y ~ x1 + x2 | id | time | treatment | treatment_period | weights}; y can be either cont or binary
#     outcome = NULL,
#     regressors = NULL,
#     id = NULL, #ID
#     time = NULL,
#     treatment = NULL, #0 - control, 1 treatment (like the Eligible variable)
#     treatment_period = NULL, # 0 - pre-treatment period, 1 - treatment period (like the Post variable)
#     weights = NULL,
#     data,
#     subset = NULL) {
#     1
# }
#
# didnpreg <- function(...){
#     args = list(...)
#     cat.print(args)
#     cat.print(names(args))
#     if (is(args[[1]],"formula")){
#         cat.print("a")
#         UseMethod("didnpreg",args[[1]])
#     }
#     else{
#         cat.print("b")
#         UseMethod("didnpreg",args$formula)
#     }
# }
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
    bwmethod = "opt", # plug-in is rule of thumb for continuous and basic for categorical, can be cross-validation
    boot.num = 399,
    TTx = FALSE,
    TTb = TRUE,
    print.level = 1,
    cores = 4,
    seed = 17345168,
    ...)
{
  cat("'formula': this is to get the matrices from formula and feed to 'didnpreg.default'\n")

  # cat.print(bwmethod)

  # cat.print(boot.num)

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
  cat.print(dim(X))
  cat.print(head(X))
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
  # didnpreg( outcome=Y,
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

  tymch <- didnpreg(
    outcome=Y,
    regressors=X,
    id=id,
    time=time,
    treatment=treatment,
    treatment_period=treatment_period,
    weights = weights,
    bwmethod = bwmethod,
    boot.num = boot.num,
    TTx = TTx,
    TTb = TTb,
    print.level = print.level,
    cores = cores,
    seed = seed,
    ...)

  # cat("15\n")


  class(tymch) <- "didnp"
  return(tymch)

}

#' @rdname didnpreg
#' @method didnpreg default
#' @export
didnpreg.default <- function(
    outcome,
    regressors,
    id,
    time,
    treatment,
    treatment_period,
    weights = NULL,
    bwmethod = "opt", # plug-in is rule of thumb for continuous and basic for categorical, can be cross-validation
    boot.num = 399,
    TTx = FALSE,
    TTb = TRUE,
    print.level = 1,
    cores = 4,
    seed = 17345168,
    ...)
{
  cat("'default': this is the workhorse\n")
  # c(class(outcome), class(regressors), class(id), class(time), class(treatment), class(treatment_period))
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

  if ( !(bwmethod %in% c("opt", "CV")) ) stop("'bwmethod' can be either 'opt' or 'CV'")


  # check dimensions

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

  # handle missing values

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


  # handle treatment

  time.treatment <- min( it.time[treatment_period == 1] )
  time.min <- min( it.time )
  time.max <- max( it.time )
  if( time.treatment - time.min < 2 ) warning (
    paste0("Data starts in ",time.min,", while treatment is in ",time.treatment)
  )
  if( time.max - time.treatment < 2 ) warning (
    paste0("Data ends in ",time.max,", while treatment is in ",time.treatment)
  )
  t <- it.time - time.treatment



  q.type <- matrix(0,nrow=3,ncol=1)
  q.typeY <- character(k.x)

  for (i in 1:k.x){

    if (is.ordered(x[[i]])==TRUE){

      q.type[3] <- q.type[3] + 1
      q.typeY[i] <- "ordered"

    } else if (is.factor(x[[i]])==TRUE) {

      q.type[2] <- q.type[2] + 1
      q.typeY[i] <- "factor"

    } else {

      q.type[1] <- q.type[1] + 1
      q.typeY[i] <- "continuous"

      ## divide each continuous x by its standard deviation
      ## it is only a mean to an end
      ## it will not change the conclusion of the test
      x[[i]] <- as.vector(x[[i]]/sd(x[[i]]))

    }

  } ## i

  cat.print(q.typeY)

  if (q.type[1]<= 3){
    cat("there are three or less cont regressors\n")
  }
  if (q.type[1]>3){cat("there are more than three cont regressors\n")}

  ## separating the data by time and treatment status

  # subsamples
  smpl1  <- d == 1
  smpl11 <- d == 1 & t == 1
  smpl10 <- d == 1 & t == 0
  smpl01 <- d == 0 & t == 1
  smpl00 <- d == 0 & t == 0

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
  # xx1less <- x[which(d==1 & t<0),]
  # xx0less <- x[which(d==0 & t<0),]

  xx11 <- x[smpl11,] # for TTa
  xx01 <- x[smpl01,]

  xx1 <- x[smpl1,]

  y11 <- y[smpl11]
  y10 <- y[smpl10]
  y01 <- y[smpl01]
  y00 <- y[smpl00]

  n11 <- length(y11)
  n10 <- length(y10)
  n01 <- length(y01)
  n00 <- length(y00)

  n1 <- nrow(xx1)

  if (TTb) {
    # prepare to retrieve TTa from TTb
    # init
    tym1 <- as.numeric(rep(NA,nt.o))
    # fill only subsample
    tym1[smpl1] <- 1:n1
    tym1[smpl11] -> TTa.positions.in.TTb
  }

  wy11 <- y11*w11
  wy01 <- y01*w01
  wy10 <- y10*w10
  wy00 <- y00*w00

  ## plug-in bandwidths (used for starting values in LSCV)
  ## Silverman (1986) for cont (make it a function of q 1.06, 1.00, ....)
  ## Chu, Henderson and Parmeter (2015) for discrete
  ## bandwidths will be scaled by sample size for the other status (d=0, t=0)
  rot.bw00 <- matrix(0,nrow=k.x,ncol=1)

  for (i in 1:k.x) {

    if (is.ordered(xx00[,i])==TRUE){

      ## First equation on page 8 in CHP (2015) - calculating relative frequencies for plug-in bandwidth
      rf <- table(xx00[,i])/length(xx00[,i])
      rot.bw00[i] <- (1/(1 + (n00*sum((1-rf)^2/(sum(rf*(1-rf)))))))

    } else if (is.factor(xx00[,i])==TRUE) {

      ## First equation on page 8 in CHP (2015) - calculating relative frequencies for plug-in bandwidth
      rf <- table(xx00[,i])/length(xx00[,i])
      rot.bw00[i] <- (1/(1 + (n00*sum((1-rf)^2/(sum(rf*(1-rf)))))))

    } else {

      ## note no sd(x) because we scaled them already
      rot.bw00[i] <- 1.06*n00^(-1/(4+q.type[1])) # adjust 1.06 to values of the Gaussian row on Page 70 Table 3.3

    }

  } ## i


  if (bwmethod == "CV") {
    if (print.level > 0) {
      cat(paste0("Calculating cross-validated bandwidths\n"))
    }
    ## setting upper and lower bounds for bandwidths
    ## need them to be 5 for the continuous variables and 1 for the discrete variables
    lower <- rep(0,ncol(x))
    upper <- rep(1,ncol(x))
    for (ii in 1:k.x) {
      upper[ii] <- ifelse(is.factor(x[,ii]),1,5)
    }

    bw.start <- rot.bw00

    # do it only on the treated in the treatment period: "11"
    bw.optim <- minqa::bobyqa(bw.start,lcls.lscv,lower,upper,y=y11,x=xx11,w=w11)
    bw.optim$par
    bw.optim$fval
    bw.optim$feval
    bw.optim$ierr

    bw11 <- bw.optim$par
    if (print.level > 1) {
      cat(paste0("Calculating cross-validated bandwidths completed\n"))
    }
  } else {
    bw11 <- rot.bw00
  }

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

  if (print.level > 0) {
    cat(paste0("Calculating ATET\n"))
  }

  if (TTb) {
    num11 <- np::npksum(txdat=xx11,tydat=wy11,exdat=xx1,bws=bw11)$ksum
    num10 <- np::npksum(txdat=xx10,tydat=wy10,exdat=xx1,bws=bw10)$ksum
    num01 <- np::npksum(txdat=xx01,tydat=wy01,exdat=xx1,bws=bw01)$ksum
    num00 <- np::npksum(txdat=xx00,tydat=wy00,exdat=xx1,bws=bw00)$ksum

    dem11 <- np::npksum(txdat=xx11,tydat=w11,exdat=xx1,bws=bw11)$ksum
    dem10 <- np::npksum(txdat=xx10,tydat=w10,exdat=xx1,bws=bw10)$ksum
    dem01 <- np::npksum(txdat=xx01,tydat=w01,exdat=xx1,bws=bw01)$ksum
    dem00 <- np::npksum(txdat=xx00,tydat=w00,exdat=xx1,bws=bw00)$ksum
  } else {
    num11 <- np::npksum(txdat=xx11,tydat=wy11,exdat=xx11,bws=bw11)$ksum
    num10 <- np::npksum(txdat=xx10,tydat=wy10,exdat=xx11,bws=bw10)$ksum
    num01 <- np::npksum(txdat=xx01,tydat=wy01,exdat=xx11,bws=bw01)$ksum
    num00 <- np::npksum(txdat=xx00,tydat=wy00,exdat=xx11,bws=bw00)$ksum

    dem11 <- np::npksum(txdat=xx11,tydat=w11,exdat=xx11,bws=bw11)$ksum
    dem10 <- np::npksum(txdat=xx10,tydat=w10,exdat=xx11,bws=bw10)$ksum
    dem01 <- np::npksum(txdat=xx01,tydat=w01,exdat=xx11,bws=bw01)$ksum
    dem00 <- np::npksum(txdat=xx00,tydat=w00,exdat=xx11,bws=bw00)$ksum
  }

  # atet <- mean(num11/dem11 - num10/dem10 - num01/dem01 + num00/dem00)
  # cat("101\n")
  # atet.hetero <- as.matrix(num11/dem11 - num10/dem10 - num01/dem01 + num00/dem00)

  if (TTb) {
    TTb.i <- as.vector(num11/dem11 - num10/dem10 - num01/dem01 + num00/dem00)
    TTa.i <- TTb.i[TTa.positions.in.TTb]
    TTa <- mean(TTa.i)
    TTb <- mean(TTb.i)
    cat.print(TTa)
    cat.print(length(TTa.i))
    cat.print(TTb)
    cat.print(length(TTb.i))
  } else {
    TTa.i <- as.vector(num11/dem11 - num10/dem10 - num01/dem01 + num00/dem00)
    TTa <- mean(TTa.i)
    cat.print(TTa)
    cat.print(length(TTa.i))
  }

  if (print.level > 1) {
    cat(paste0("Calculating ATET completed\n"))
  }

  ## begin the bootstrap for atet
  if (print.level > 0) {
    cat(paste0("Bootstrapping standard errors (",boot.num," replications)\n"))
  }
  set.seed(seed = seed)#, kind = "L'Ecuyer-CMRG")
  seeds <- sample.int(n = .Machine$integer.max, size = boot.num)


  ## need to evaluate the data at the observations to get the residuals
  if (print.level > 0) {
    cat(paste0("Calculating residuals\n"))
  }
  nnum11 <- np::npksum(txdat=xx11,tydat=wy11,exdat=xx11,bws=bw11)$ksum
  nnum10 <- np::npksum(txdat=xx10,tydat=wy10,exdat=xx10,bws=bw10)$ksum
  nnum01 <- np::npksum(txdat=xx01,tydat=wy01,exdat=xx01,bws=bw01)$ksum
  nnum00 <- np::npksum(txdat=xx00,tydat=wy00,exdat=xx00,bws=bw00)$ksum

  ddem11 <- np::npksum(txdat=xx11,tydat=w11,exdat=xx11,bws=bw11)$ksum
  ddem10 <- np::npksum(txdat=xx10,tydat=w10,exdat=xx10,bws=bw10)$ksum
  ddem01 <- np::npksum(txdat=xx01,tydat=w01,exdat=xx01,bws=bw01)$ksum
  ddem00 <- np::npksum(txdat=xx00,tydat=w00,exdat=xx00,bws=bw00)$ksum

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
  doParallel::registerDoParallel(cores = cores)

  if (print.level > 0 & cores == 1)
  {
    pb <- utils::txtProgressBar(min = 0, max = boot.num, style = 3)
  }

  mcoptions <- list(set.seed = TRUE)

  atet.boot.hetero <-
    foreach::foreach(j = 1:boot.num, .options.multicore = mcoptions, .combine = "cbind", .verbose = FALSE) %dopar% {

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

      if (TTb) {
        num11.new <- np::npksum(txdat=xx11,tydat=wy11.new,exdat=xx1,bws=bw11)$ksum
        num10.new <- np::npksum(txdat=xx10,tydat=wy10.new,exdat=xx1,bws=bw10)$ksum
        num01.new <- np::npksum(txdat=xx01,tydat=wy01.new,exdat=xx1,bws=bw01)$ksum
        num00.new <- np::npksum(txdat=xx00,tydat=wy00.new,exdat=xx1,bws=bw00)$ksum
      } else {
        num11.new <- np::npksum(txdat=xx11,tydat=wy11.new,exdat=xx11,bws=bw11)$ksum
        num10.new <- np::npksum(txdat=xx10,tydat=wy10.new,exdat=xx11,bws=bw10)$ksum
        num01.new <- np::npksum(txdat=xx01,tydat=wy01.new,exdat=xx11,bws=bw01)$ksum
        num00.new <- np::npksum(txdat=xx00,tydat=wy00.new,exdat=xx11,bws=bw00)$ksum
      }


      if (print.level > 0 & j == 1)
      {
        time.06 <- proc.time()
        boot.time.sec <- boot.num*(time.06-time.05)[3]
        .timing(boot.time.sec, "\nBootstrapping will take approximately: ")
        # cat("\n")
        # pb <- utils::txtProgressBar(min = 1, max = boot.num, style = 3)
      }

      if (print.level > 0 & j > 1) utils::setTxtProgressBar(pb, j)

      # it does not matter if 'TTa' or 'TTb', since denominators are
      # not bootstrapped and are already calculated
      num11.new/dem11 - num10.new/dem10 - num01.new/dem01 + num00.new/dem00
    }

  # do parallel completed
  if (print.level > 0) cat("\n")

  atet.boot <- rowMeans(atet.boot.hetero)

  if (TTb) {
    TTa.sd <- sd(atet.boot[TTa.positions.in.TTb])
    TTb.sd <- sd(atet.boot)
    cat.print(TTa.sd)
    cat.print(TTb.sd)
  } else {
    TTa.sd <- sd(atet.boot)
    cat.print(TTa.sd)
  }

  time.06 <- proc.time()
  boot.time.sec <- (time.06-time.05)[3]
  names(boot.time.sec) <- "sec"

  if (print.level >= 2){
    .timing(boot.time.sec, "Bootstrapping standard errors completed in\n")
    # cat("___________________________________________________\n")
  }

  # if (print.level > 0) cat("\n")

  # cat("11\n")

  sd.atet <- sd(atet.boot)

  # cat("12\n")

  ## let's return the objects npdid,npdid.se,rot.bw,lscv.bw

  if (TTb) {
    tymch <- list(
      bw11 = as.vector(bw11),
      bw10 = bw10,
      bw01 = bw01,
      bw00 = bw00,
      TTa.i = TTa.i,
      TTa = TTa,
      TTb.i = TTb.i,
      TTb = TTb,
      TTa.sd = TTa.sd,
      TTb.sd = TTb.sd,
      TTa.i.boot = atet.boot.hetero[TTa.positions.in.TTb,],
      TTb.i.boot = atet.boot.hetero
    )
  } else {
    tymch <- list(
      bw11 = as.vector(bw11),
      bw10 = bw10,
      bw01 = bw01,
      bw00 = bw00,
      TTa.i = TTa.i,
      TTa = TTa,
      TTa.sd = TTa.sd,
      TTa.i.boot = atet.boot.hetero
    )
  }

  # cat("13\n")

  class(tymch) <- "didnp"
  return(tymch)

}
