#' @rdname summary.didnp
#' @export
print.summary.didnp <- function( x, digits = NULL, print.level = NULL, ... ) {
   # digits
   if( is.null( digits ) ) {
      digits <- 4
   }
   # print level
   if( is.null( print.level ) ) {
      print.level <- 2
   }

   if (print.level > 0) {
      cat("Number of Observations is ", x$NT, "\n")
   }

   ## print info about regressors ----

   if (print.level > 0) {

      q.type <- x$regressor.type

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

   if (print.level > 0) {
      cat(paste0("\nUnconditional Treatment Effect on the Treated (ATET):\n\n"))
      if (x$TTb) {
         cat(paste0("TTa    = ",formatC(x$TTa, digits = digits),"\n"))
         cat(paste0("TTa sd = ",formatC(x$TTa.sd, digits = digits),"\n"))
         cat(paste0("N(TTa) = ",sum(x$sample11),"\n"))
         cat(paste0("\nTTb    = ",formatC(x$TTb, digits = digits),"\n"))
         cat(paste0("TTb sd = ",formatC(x$TTb.sd, digits = digits),"\n"))
         cat(paste0("N(TTb) = ",sum(x$sample1),"\n"))
      } else {
         cat(paste0("TTa    = ",formatC(x$TTa, digits = digits),"\n"))
         cat(paste0("TTa sd = ",formatC(x$TTa.sd, digits = digits),"\n"))
         cat(paste0("N(TTa) = ",sum(x$sample11),"\n"))
      }
   }

   invisible( x )
}
