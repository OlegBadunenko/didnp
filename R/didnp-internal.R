lcls.lscv0 <- function(h,y,x,w){

  h <- as.vector(h)
  y <- as.vector(y)
  w <- as.vector(w)

  n <- length(y)

  # ones11 <- as.vector(rep(1,n))

  wy <- w*y

  if (min(h) <= 0) {

    return(.Machine$double.xmax)

  } else {
    # section 3.1.3 Sampling Weights
    n.mhat <- np::npksum(txdat=x,tydat=wy,leave.one.out=TRUE,bandwidth.divide=TRUE,bws=h)$ksum
    d.mhat <- np::npksum(txdat=x,tydat=w, leave.one.out=TRUE,bandwidth.divide=TRUE,bws=h)$ksum

    mhat <- n.mhat/d.mhat

    # cat.print(summary(n.mhat))
    # cat.print(summary(d.mhat))
    # cat.print(summary(mhat))

    return(sum((y - mhat)^2)/n)

  }

}

lcls.lscv <- function(h, cores, y, wy, w, x, xtype, nlevels, n, k){

  # h <- as.vector(h)
  # y <- as.vector(y)
  # w <- as.vector(w)
  #
  # n <- length(y)

  # ones11 <- as.vector(rep(1,n))

  # wy <- w*y

  if (min(h) <= 0) {

    return(.Machine$double.xmax)

  } else {
    # section 3.1.3 Sampling Weights
    # n.mhat <- np::npksum(txdat=x,tydat=wy,leave.one.out=TRUE,bandwidth.divide=TRUE,bws=h)$ksum
    # d.mhat <- np::npksum(txdat=x,tydat=w, leave.one.out=TRUE,bandwidth.divide=TRUE,bws=h)$ksum

    n.mhat <- .npksumYXloo(
      Nthreds=cores,
      ydat=wy, x=x, bw=h,
      xtype=xtype, nlevels=nlevels, n=n,q=k
    )
    d.mhat <- .npksumYXloo(
      Nthreds=cores,
      ydat=w, x=x, bw=h,
      xtype=xtype, nlevels=nlevels, n=n,q=k
    )

    # cat.print(summary(n.mhat))
    # cat.print(summary(d.mhat))

    mhat <- n.mhat/d.mhat

    # cat.print(summary(n.mhat))
    # cat.print(summary(d.mhat))
    # cat.print(summary(mhat))

    return(sum((y - mhat)^2)/n)

  }

}

is.binary <- function(v) {
  x <- unique(v)
  length(x) - sum(is.na(x)) == 2L && all(x[1:2] == 0:1)
}

.timing <- function(x, wording = "Time elapsed is")
{
  # print(x)
  if(x < 60){
    # cat("\n")
    my.sec <- ifelse(x %% 10 == 1 | x == 1 & x != 11, "second", "seconds")
    cat("",wording,"",x," ",my.sec,"\n", sep = "")
    # cat("\n")
  } else {
    if(x >= 60 & x < 60*60){
      minutes <- floor(x/60)
      my.min <- ifelse(minutes %% 10 == 1 | minutes == 1, "minute", "minutes")
      seconds <- round(x - minutes * 60,1)
      my.sec <- ifelse(seconds %% 10 == 1 | seconds == 1, "second", "seconds")
      # cat("\n")
      cat("",wording,"",minutes," ",my.min," and ",seconds," ",my.sec,"\n", sep = "")
      # cat("\n")
    } else {
      if(x >= 60*60 & x < 60*60*24){
        hours   <- floor(x / 60 / 60)
        my.hour <- ifelse(hours %% 10 == 1 | hours == 1, "hour", "hours")
        minutes <- round( (x - hours * 60 *60) / 60, 1)
        my.min <- ifelse(minutes %% 10 == 1 | minutes == 1, "minute", "minutes")
        seconds <- floor(x - hours * 60 *60 - minutes * 60)
        # cat("\n")
        cat("",wording,"",hours," ",my.hour," and ",minutes," ",my.min," \n", sep = "")
        # cat("\n")
      } else {
        if(x >= 60*60*24){
          days    <- floor(x / 60 / 60 / 24)
          my.days <- ifelse(days %% 10 == 1 | days == 1, "day", "days")
          hours   <- round( (x - days * 60 * 60 * 24) / 60 /60 ,1)
          my.hour <- ifelse(hours %% 10 == 1 | hours == 1, "hour", "hours")
          minutes <- floor( (x - days * 60 * 60 * 24 - hours * 60 *60) / 60)
          seconds <- floor(x - days * 60 * 60 * 24 - hours * 60 *60 - minutes * 60)
          # cat("\n")
          cat("",wording,"",days," ",my.days," and ",hours," ",my.hour,"\n", sep = "")
          # cat("\n")
        }
      }
    }
  }
}

cat.print <- function(x, name = NULL){
  if (is.null(name) | !is.character(name)) {
    cat(" ",deparse(substitute(x)),":\n", sep = "")
  } else {
    cat(" ",name,":\n", sep = "")
  }
  print(x)
}

.npksumYXnew <-
  function(
    Nthreds = 1,
    ydat, xdat, xeval, bw,
    xtype, nlevels, n, neval, q
  )
  {
    # cat.print(xtype)
    tymch0 <- .C(
      "npksumYXnew",
      Nthreds = as.integer(Nthreds),
      ydat = as.double(ydat),
      xdat = as.double(xdat),
      xeval = as.double(xeval),
      bw = as.double(bw),
      xtype = as.integer(xtype),
      nlevels = as.double(nlevels),
      n = as.integer(n),
      neval = as.integer(neval),
      q = as.integer(q),
      ksum = double(neval)
    )
    # cat.print(tymch0[1:10])
    # cat.print(length(tym))
    # cat.print( names(tymch0) )
    # cat.print( tymch0 $ksum[1:100] )
    return(tymch0 $ksum)
  }

.npksumYX <-
  function(
    Nthreds = 1,
    ydat, xdat, bw,
    xtype, nlevels, n, q
  )
  {
    tymch0 <- .C(
      "npksumYX",
      Nthreds = as.integer(Nthreds),
      ydat = as.double(ydat),
      xdat = as.double(xdat),
      bw = as.double(bw),
      xtype = as.integer(xtype),
      nlevels = as.double(nlevels),
      n = as.integer(n),
      q = as.integer(q),
      ksum = double(n)
    )
    return(tymch0 $ksum)
  }


.npksumYXloo <-
  function(
    Nthreds = 1,
    ydat, xdat, bw,
    xtype, nlevels, n, q
  )
  {
    tymch0 <- .C(
      "npksumYXloo",
      Nthreds = as.integer(Nthreds),
      ydat = as.double(ydat),
      xdat = as.double(xdat),
      bw = as.double(bw),
      xtype = as.integer(xtype),
      nlevels = as.double(nlevels),
      n = as.integer(n),
      q = as.integer(q),
      ksum = double(n)
    )
    return(tymch0 $ksum)
  }


.npksumXnew <-
  function(
    Nthreds = 1,
    xdat, xeval, bw,
    xtype, nlevels, n, neval, q
  )
  {
    tymch0 <- .C(
      "npksumXnew",
      Nthreds = as.integer(Nthreds),
      xdat = as.double(xdat),
      xeval = as.double(xeval),
      bw = as.double(bw),
      xtype = as.integer(xtype),
      nlevels = as.double(nlevels),
      n = as.integer(n),
      neval = as.integer(neval),
      q = as.integer(q),
      ksum = double(neval)
    )
    return(tymch0 $ksum)
  }


.npksumX <-
  function(
    Nthreds = 1,
    xdat, bw,
    xtype, nlevels, n, q  )
  {
    tymch0 <- .C(
      "npksumX",
      Nthreds = as.integer(Nthreds),
      xdat = as.double(xdat),
      bw = as.double(bw),
      xtype = as.integer(xtype),
      nlevels = as.double(nlevels),
      n = as.integer(n),
      q = as.integer(q),
      ksum = double(n)
    )
    return(tymch0 $ksum)
  }






