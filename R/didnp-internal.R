lcls.lscv <- function(h,y,x,w){

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
    d.mhat <- np::npksum(txdat=x,tydat=w,leave.one.out=TRUE,bandwidth.divide=TRUE,bws=h)$ksum

    mhat <- n.mhat/d.mhat

    return(sum((y - mhat)^2)/n)

  }

}

is.binary <- function(v) {
  x <- unique(v)
  length(x) - sum(is.na(x)) == 2L && all(x[1:2] == 0:1)
}

.timing <- function(x, wording = "Time elapsed is")
{
  if(x < 60){
    # cat("\n")
    my.sec <- ifelse(x %% 10 == 1 | x == 1, "second", "seconds")
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
