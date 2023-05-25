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
    cat("",wording,"",x," seconds\n", sep = "")
    # cat("\n")
  } else {
    if(x >= 60 & x < 60*60){
      minutes <- floor(x/60)
      seconds <- round(x - minutes * 60,1)
      # cat("\n")
      cat("",wording,"",minutes," minute(s) and ",seconds," second(s)\n", sep = "")
      # cat("\n")
    } else {
      if(x >= 60*60 & x < 60*60*24){
        hours   <- floor(x / 60 / 60)
        minutes <- round( (x - hours * 60 *60) / 60, 1)
        seconds <- floor(x - hours * 60 *60 - minutes * 60)
        # cat("\n")
        cat("",wording,"",hours," hour(s) and ",minutes," minute(s) \n", sep = "")
        # cat("\n")
      } else {
        if(x >= 60*60*24){
          days    <- floor(x / 60 / 60 / 24)
          hours   <- round( (x - days * 60 * 60 * 24) / 60 /60 ,1)
          minutes <- floor( (x - days * 60 * 60 * 24 - hours * 60 *60) / 60)
          seconds <- floor(x - days * 60 * 60 * 24 - hours * 60 *60 - minutes * 60)
          # cat("\n")
          cat("",wording,"",days," day(s) and ",hours," hour(s)\n", sep = "")
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
