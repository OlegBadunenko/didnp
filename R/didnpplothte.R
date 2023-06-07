#' Plotting Heterogenous Treatment Effects
#'
#' The \code{didnpplothte} take output created by \link{didnpreg} and produces plots of Heterogeneous Treatment Effects with their corresponding confidence bounds. Average effects per group are produced for discrete covariates and a continuum of effects are produced for continuous covariates.
#'
#' @param obj an object of class "didnp".
#' @param by categorical or continuous.
#' @param n.intervals number of intervals for the numeric "by" to be split into. Default is 10.
#' @param over necessarily categorical.
#' @param xlab Label for horizontal axis. Default is "".
#' @param ylab Label for vertical axis. Default is "ATET".
#' @param point_size of ATET. Default is 3.
#' @param line_width of ATET for the numeric "by". Default is 2.
#' @param labels.values for the categorical "by", the dataframe with two columns. The first column contains unique values of the "by", while the second column contains corresponding values that would appear on a graph. Default is NULL, implying that the unique values of the "by" will be used.
#' @param text_size for ggplot object. Default is 17.
#'
#' @details
#' Vector "by" (and "over") must be of length of TTa.i if \code{TTb = FALSE} was used in running \link{didnpreg} and of length of TTb.i if \code{TTb = TRUE} was used in running \link{didnpreg}
#'
#'
#' @return \code{didnpplot} returns a dataframe containing containing the following columns:
#' \tabular{ll}{
#'    \code{plot.a}
#'    \tab ggplot object
#'    \cr \tab \cr
#'    \code{data.a}
#'    \tab data used to produce \code{plot.a}
#'    \cr \tab \cr
#'    \code{plot.b}
#'    \tab ggplot object for TTb if \code{TTb = TRUE} was used in running \link{didnpreg}
#'    \cr \tab \cr
#'    \code{data.a}
#'    \tab data used to produce \code{plot.b}
#'    \cr
#' }
#'
#'
#' @keywords did np plot
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
#'   tym1a <- didnpreg(
#'     form1,
#'     data = DACAsub,
#'     subset = mysmpl,
#'     bwmethod = "opt",
#'     boot.num = 399,
#'     TTb = FALSE,
#'     print.level = 1,
#'     cores = 4)
#'
#'   # Print the summary
#'   summary(tym1a)
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
#' @rdname didnpplothte
#' @export
didnpplothte <- function(
    obj,
    level = 95,
    by,
    n.intervals = 10,
    over = NULL,
    xlab = "",
    ylab = "ATET",
    point_size = 3,
    line_width = 2,
    labels.values = NULL,
    text_size = 17){

    if(sum(class(obj) == "didnpreg") != 1) stop ("Run 'didnpreg' first")

    # correspondence.val <- data.frame(
    #     old = c(1,2,3,4,5),
    #     new = c("Hispanic", "White","Black","Asian","Other")
    # )

    if (level < 0 | level > 99.99) {
        stop("'level' must be between 0 and 99.99 inclusive", call. = FALSE)
    }
    crit.value <- qnorm( 1 - (1-level/100)/2 )

    # check the length ----

    if (is.null(over)) {

        # single "by"

        if (obj$do.TTb) {
            # do two plots: for TTa and TTb
            if (length(by) != length(obj$TTb.i)) stop ("The length of 'by' must be equal to the length of 'TTb.i'")

            # ateti.b <- obj$TTb.i.boot
            # ateti   <- obj$TTb.i
            # ateti.b <- obj$TTa.i.boot
            # ateti   <- obj$TTa.i

        } else {
            # do one plot
            if (length(by) != length(obj$TTa.i)) stop ("The length of 'by' must be equal to the length of 'TTa.i'")

        }

    } else {

        # double "by"

        if (obj$do.TTb) {
            # do two plots: for TTa and TTb
            if (length(by) != length(obj$TTb.i)) stop ("The length of 'by' must be equal to the length of 'TTb.i'")
            if (length(over) != length(obj$TTb.i)) stop ("The length of 'over' must be equal to the length of 'TTb.i'")

        } else {
            # do one plot
            if (length(by) != length(obj$TTa.i)) stop ("The length of 'by' must be equal to the length of 'TTa.i'")
            if (length(over) != length(obj$TTa.i)) stop ("The length of 'over' must be equal to the length of 'TTa.i'")

        }
    }

    # graph ----

    if (is.null(over)) {

        # single "by" ----

        if (class(by) == "factor"){

            ## 'by' is a factor ----

            by.levels <- sort( unique(by) )
            n.levels <-  length(by.levels)

            if (is.null(labels.values)) {

                by2.levels <- by.levels

            } else {

                if (nrow(labels.values) != n.levels) stop("Inappropriate number of rows in 'labels.values'")
                if (ncol(labels.values) != 2) stop("Inappropriate number of cols in 'labels.values'")
                if (!all(by.levels == labels.values[,1]) ) stop("Column 1 of 'labels.values' contains some inappropriate values")
                colnames(labels.values) <- c("old", "new")
                by2.levels <- merge.data.frame(
                    data.frame(old = by.levels),
                    labels.values)[,2]
            }

            if (obj$do.TTb) {

                ## TTa and TTb ----

                ## TTb ----

                atet <- atet.sd <- numeric(n.levels)

                for(i in 1:n.levels){
                    atet[i] <- mean(obj$TTb.i[by == by.levels[i]])
                    atet.sd[i] <- sd(colMeans(obj$TTb.i.boot[by == by.levels[i],]))
                }

                d1b <-
                    data.frame(
                        atet = atet,
                        atet.sd = atet.sd,
                        by = by.levels,
                        by2 = by2.levels
                    )

                plot.b <- ggplot(d1b, aes(x = by2, y = atet, fill = by2)) +
                    geom_bar(position = position_dodge(), stat = "identity") +
                    geom_errorbar(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), color = "black", width = .1) +
                    labs(x = xlab, y = ylab) +
                    geom_point(size = point_size) +
                    scale_fill_discrete(
                        name = xlab,#
                        # labels = c("Traditional", "Comprehensive", "Technology")
                    ) +
                    theme_bw() +
                    theme(legend.position = "none", text = element_text(size = text_size))

                ## TTa ----

                by.a <- by[obj$TTa.positions.in.TTb]

                atet <- atet.sd <- numeric(n.levels)

                for(i in 1:n.levels){
                    atet[i] <- mean(obj$TTa.i[by.a == by.levels[i]])
                    atet.sd[i] <- sd(colMeans(obj$TTa.i.boot[by.a == by.levels[i],]))
                }

                d1a <-
                    data.frame(
                        atet = atet,
                        atet.sd = atet.sd,
                        by = by.levels,
                        by2 = by2.levels
                    )

                plot.a <- ggplot(d1a, aes(x = by2, y = atet, fill = by2)) +
                    geom_bar(position = position_dodge(), stat = "identity") +
                    geom_errorbar(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), color = "black", width = .1) +
                    labs(x = xlab, y = ylab) +
                    geom_point(size = point_size) +
                    scale_fill_discrete(
                        name = xlab,#
                        # labels = c("Traditional", "Comprehensive", "Technology")
                    ) +
                    theme_bw() +
                    theme(legend.position = "none", text = element_text(size = text_size))

                tymch <- list(plot.a = plot.a, plot.b = plot.b, data.a = d1a, data.b = d1b)
                class(tymch) <- c("didnpplot", "didnp")
                return(tymch)

            } else {

                # TTa ----

                atet <- atet.sd <- numeric(n.levels)

                for(i in 1:n.levels){
                    atet[i] <- mean(obj$TTa.i[by == by.levels[i]])
                    atet.sd[i] <- sd(colMeans(obj$TTa.i.boot[by == by.levels[i],]))
                }

                d1 <-
                    data.frame(
                        atet = atet,
                        atet.sd = atet.sd,
                        by = by.levels,
                        by2 = by2.levels
                    )

                plot.a <- ggplot(d1, aes(x = by2, y = atet, fill = by2)) +
                    geom_bar(position = position_dodge(), stat = "identity") +
                    geom_errorbar(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), color = "black", width = .1) +
                    labs(x = xlab, y = ylab) +
                    geom_point(size = point_size) +
                    scale_fill_discrete(
                        name = xlab,#
                        # labels = c("Traditional", "Comprehensive", "Technology")
                    ) +
                    theme_bw() +
                    theme(legend.position = "none", text = element_text(size = text_size))

                tymch <- list(plot.a = plot.a, data.a = d1)
                class(tymch) <- c("didnpplot", "didnp")
                return(tymch)

            }


        } else if (class(by) == "numeric"){

            ## 'by' is a continuous ----

            if (obj$do.TTb) {

                ## TTa and TTb ----

                ## TTb ----

                cat("TTb begin")

                my.by <- by

                by <- base::cut(my.by, n.intervals)

                by.levels <- sort( unique(by) )

                n.levels <-  length(by.levels)

                atet <- atet.sd <-  numeric(n.levels)

                for(i in 1:n.levels){
                    atet[i] <- mean(obj$TTb.i[by == by.levels[i]])
                    atet.sd[i] <- sd(colMeans(obj$TTb.i.boot[by == by.levels[i],]))
                }

                d1b <-
                    data.frame(
                        atet = atet,
                        atet.sd = atet.sd,
                        by = by.levels
                    )

                plot.b <- ggplot(d1b, aes(x = by, y = atet, group = 1)) +
                    geom_ribbon(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), alpha = 0.3) +
                    geom_line(linewidth = line_width) +
                    geom_point(size = point_size, shape = 16, color = "black") +
                    labs(x = xlab, y = ylab) +
                    theme_bw() +
                    theme(legend.position = "none", text = element_text(size = text_size))

                cat("TTb end")


                ## TTa ----

                cat("TTa begin")


                by <- my.by[obj$TTa.positions.in.TTb]

                my.by <- by

                by <- base::cut(my.by, n.intervals)

                by.levels <- sort( unique(by) )

                n.levels <-  length(by.levels)

                atet <- atet.sd <-  numeric(n.levels)

                for(i in 1:n.levels){
                    atet[i] <- mean(obj$TTa.i[by == by.levels[i]])
                    atet.sd[i] <- sd(colMeans(obj$TTa.i.boot[by == by.levels[i],]))
                }

                d1a <-
                    data.frame(
                        atet = atet,
                        atet.sd = atet.sd,
                        by = by.levels
                    )

                plot.a <- ggplot(d1a, aes(x = by, y = atet, group = 1)) +
                    geom_ribbon(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), alpha = 0.3) +
                    geom_line(linewidth = line_width) +
                    geom_point(size = point_size, shape = 16, color = "black") +
                    labs(x = xlab, y = ylab) +
                    theme_bw() +
                    theme(legend.position = "none", text = element_text(size = text_size))

                cat("TTb end")

                tymch <- list(plot.a = plot.a, plot.b = plot.b, data.a = d1a, data.b = d1b)
                class(tymch) <- c("didnpplot", "didnp")
                return(tymch)

            } else {

                # TTa ----

                my.by <- by

                by <- base::cut(my.by, n.intervals)

                by.levels <- sort( unique(by) )

                n.levels <-  length(by.levels)

                atet <- atet.sd <-  numeric(n.levels)

                for(i in 1:n.levels){
                    atet[i] <- mean(obj$TTa.i[by == by.levels[i]])
                    atet.sd[i] <- sd(colMeans(obj$TTa.i.boot[by == by.levels[i],]))
                }

                d1 <-
                    data.frame(
                        atet = atet,
                        atet.sd = atet.sd,
                        by = by.levels
                    )

                plot.a <- ggplot(d1, aes(x = by, y = atet, group = 1)) +
                    geom_ribbon(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), alpha = 0.3) +
                    geom_line(linewidth = line_width) +
                    geom_point(size = point_size, shape = 16, color = "black") +
                    labs(x = xlab, y = ylab) +
                    theme_bw() +
                    theme(legend.position = "none", text = element_text(size = text_size))

                tymch <- list(plot.a = plot.a, data.a = d1)
                class(tymch) <- c("didnpplot", "didnp")
                return(tymch)


            }

        } else {
            stop("Incorrect class of 'by'")
        }
    } else {

        # double "by"

    }






}
