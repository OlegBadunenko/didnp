#' @export
print.summary.didnp <- function( x, digits = 4, print.level = 1, level = 95, ... ) {
   # digits
   if( is.null( digits ) ) {
      digits <- 4
   }
   # print level
   if( is.null( print.level ) ) {
      print.level <- 2
   }

   if (print.level > 0) {
      cat("Number of Observations =", x$NT, "\n")
   }

   if (print.level > 0 & x$TTx == "TTa") {
      cat("Number of observations in the year of the treatment and one year after the treatment =", x$n11 + x$n10 + x$n01 + x$n00, "\n")
   }

   if (print.level > 0) {
      cat("\n")
      cat("Number of observations in treated group right after the treatment (N_11) =" ,x$n11, "\n")
      cat("Number of observations in treated group just before the treatment (N_10) =" ,x$n10, "\n")
      cat("Number of observations in control group right after the treatment (N_01) =" ,x$n01, "\n")
      cat("Number of observations in control group just before the treatment (N_00) =" ,x$n00, "\n")
   }

   ## print info about regressors ----

   if (print.level > 0) {

      q.type <- x$regressor.type

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

   if (print.level > 0) {
      if (x$bwmethod == "CV") {

         cat(paste0("Calculating cross-validated bandwidths\n"))

         ## print info about bw ----

         if (q.type[1] > 0) {
            cat("Kernel Type for Continuous Regressors is               Gaussian\n")
         }
         if (q.type[2] > 0) {
            cat("Kernel Type for Unordered Categorical Regressors is    Aitchison and Aitken\n")
         }
         if (q.type[3] > 0) {
            cat("Kernel Type for Ordered Categorical is                 Li and Racine\n")
         }
         .timing(x$bw.time, "Calculating cross-validated bandwidths completed in ")
      } else {
         cat("Bandwidths are chosen via the plug-in method\n")
      }
   }

   ## Print bws ----

   if (print.level > 0) {
      cat("\n")
      print(x$bws)
   }

   if (print.level > 1){
      .timing(x$boot.time, paste0("\nBootstrapping standard errors (",x$boot.num," replications) completed in "))
      # cat("___________________________________________________\n")
   }

   if (x$TTx == "TTa") {
      TTb <- FALSE
   } else {
      TTb <- TRUE
   }
   do.TTb <- TTb

   if (do.TTb) {
      atet.boot <- colMeans(x$TTb.i.boot)
   } else {
      atet.boot <- colMeans(x$TTa.i.boot)
   }

   if (print.level > 0) {
      cat(paste0("\nUnconditional Treatment Effect on the Treated (ATET):\n\n"))
   }

   if (do.TTb) {
      TTa.positions.in.TTb <- x$TTa.positions.in.TTb
      TTa.se <- sd(atet.boot[TTa.positions.in.TTb])
      TTb.se <- sd(atet.boot)
      TTb <- x$TTb
      TTa <- x$TTa

      # cat.print(TTa.se)
      # cat.print(TTb.se)
      if (print.level > 0) {
         # cat("\nTTa sd =",formatC(TTa.se, digits = 4),"\n")
         # cat("TTb se =",formatC(TTb.se, digits = digits),"\n")

         cat(paste0("TTb    = ",formatC(TTb, digits = digits),"\n"))
         cat(paste0("TTb sd = ",formatC(TTb.se, digits = digits),"\n"))
         cat(paste0("N(TTb) = ",sum(x$sample11),"\n"))


         qu1 <- quantile(atet.boot, probs = c((1-level/100)/2))
         qu1 <-ifelse(qu1 > 999,
                      formatC(qu1, digits=1, format="e"),
                      formatC(qu1, digits=digits, format="f"))
         qu2 <- quantile(atet.boot, probs = c((1+level/100)/2))
         qu2 <-ifelse(qu2 > 999,
                      formatC(qu2, digits=1, format="e"),
                      formatC(qu2, digits=digits, format="f"))
         cat("\nBootstrapped ",level,"% confidence interval: [",qu1,", ",qu2,"]\n", sep = "")
      }
      atets <- cbind(round(TTb, digits = digits),
                     round(TTb.se, digits = digits),
                     round(TTb/TTb.se,digits = 2),
                     round(pnorm(abs(TTb/TTb.se), lower.tail = FALSE)*2, digits = digits),
                     round(TTb-qnorm((1+level/100)/2)*TTb.se, digits = digits),
                     round(TTb+qnorm((1+level/100)/2)*TTb.se, digits = digits))
      rownames(atets) <- "ATET"
      colnames(atets) <- c("Coef.", "SE ", "z ",  "P>|z|", "CIl", "CIu")
      max.name.length <- max(nchar(row.names(atets)))
      mycutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1)
      mysymbols = c("***", "**", "*", ".", " ")
      na.print = "NA"
   } else {
      TTa.se <- x$TTa.se
      TTa <- x$TTa
      if (print.level > 0) {
         cat(paste0("TTa    = ",formatC(TTa, digits = digits),"\n"))
         cat(paste0("TTa sd = ",formatC(TTa.se, digits = digits),"\n"))
         cat(paste0("N(TTa) = ",sum(x$sample11),"\n"))

         # cat("\nTTa se =",formatC(TTa.se, digits = digits),"\n")

         qu1 <- quantile(atet.boot, probs = c((1-level/100)/2))
         qu1 <-ifelse(qu1 > 999,
                      formatC(qu1, digits=1, format="e"),
                      formatC(qu1, digits=digits, format="f"))
         qu2 <- quantile(atet.boot, probs = c((1+level/100)/2))
         qu2 <-ifelse(qu2 > 999,
                      formatC(qu2, digits=1, format="e"),
                      formatC(qu2, digits=digits, format="f"))
         cat("Bootstrapped ",level,"% confidence interval: [",qu1,", ",qu2,"]\n", sep = "")
      }
      atets <- cbind(round(TTa, digits = digits),
                     round(TTa.se, digits = digits),
                     round(TTa/TTa.se,digits = 2),
                     round(pnorm(abs(TTa/TTa.se), lower.tail = FALSE)*2, digits = digits),
                     round(TTa-qnorm((1+level/100)/2)*x$TTa.se, digits = digits),
                     round(TTa+qnorm((1+level/100)/2)*x$TTa.se, digits = digits))
      rownames(atets) <- "ATET"
      colnames(atets) <- c("Coef.", "SE ", "z ",  "P>|z|", "CIl", "CIu")
      max.name.length <- max(nchar(row.names(atets)))
      mycutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1)
      mysymbols = c("***", "**", "*", ".", " ")
      na.print = "NA"
   }

   if (print.level > 0) {
      cat("\np-value and confidence interval assuming ATET is normally distributed:\n\n")

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

      row.names(Cf) <- formatC(row.names(Cf), width = max(nchar(row.names(Cf))), flag = "-")
      cat("",rep(" ", max.name.length+6),"Coef.        SE       z      P>|z|  [",level,"% confidence interval]", sep = "")


      # cat.print(Cf)
      dimnames(Cf)[[2]] <- rep("", dim(Cf)[[2]])
      print.default(Cf[1:1,,drop=FALSE], quote = FALSE, right = TRUE, na.print = na.print)
   }



   invisible( x )
}
