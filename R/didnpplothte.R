#' Plotting Heterogenous Treatment Effects
#'
#' The \code{didnpplothte} take output created by \link{didnpreg} and produces plots of Heterogeneous Treatment Effects with their corresponding confidence bounds. Average effects per group are produced for discrete covariates and a continuum of effects are produced for continuous covariates.
#'
# see examples here
# https://friendly.github.io/nestedLogit/articles/plotting-ggplot.html
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
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
    over.lab = "ATET over",
    over.ci.lab = "Conf.Int.",
    point_size = 3,
    line_width = 2,
    by.labels.values = NULL,
    over.labels.values = NULL,
    text_size = 17){

    if(sum(class(obj) == "didnpreg") != 1) stop ("Run 'didnpreg' first", call. = FALSE)

    # correspondence.val <- data.frame(
    #     old = c(1,2,3,4,5),
    #     new = c("Hispanic", "White","Black","Asian","Other")
    # )

    if (level < 0 | level > 99.99) {
        stop("'level' must be between 0 and 99.99 inclusive", call. = FALSE)
    }
    crit.value <- qnorm( 1 - (1-level/100)/2 )

    # check the lengths ----

    if (is.null(over)) {

        # single "by"

        if (obj$do.TTb) {
            # do two plots: for TTa and TTb
            if (length(by) != length(obj$TTb.i)) stop ("The length of 'by' must be equal to the length of 'TTb.i'", call. = FALSE)

            # ateti.b <- obj$TTb.i.boot
            # ateti   <- obj$TTb.i
            # ateti.b <- obj$TTa.i.boot
            # ateti   <- obj$TTa.i

        } else {
            # do one plot
            if (length(by) != length(obj$TTa.i)) stop ("The length of 'by' must be equal to the length of 'TTa.i'", call. = FALSE)

        }

    } else {

        # double "by"

        if (obj$do.TTb) {
            # do two plots: for TTa and TTb
            if (length(by) != length(obj$TTb.i)) stop ("The length of 'by' must be equal to the length of 'TTb.i'", call. = FALSE)
            if (length(over) != length(obj$TTb.i)) stop ("The length of 'over' must be equal to the length of 'TTb.i'", call. = FALSE)

        } else {
            # do one plot
            if (length(by) != length(obj$TTa.i)) stop ("The length of 'by' must be equal to the length of 'TTa.i'", call. = FALSE)
            if (length(over) != length(obj$TTa.i)) stop ("The length of 'over' must be equal to the length of 'TTa.i'", call. = FALSE)

        }
    }

    # check missing values

    if ( sum(is.na(by)) ) stop ("'by' contains missing values", call. = FALSE)

    # graph ----

    ## handle "over" ----

    if (!is.null(over)) {

        if (class(over) != "factor") stop("Inappropriate class of 'over'", call. = FALSE)
        if ( sum(is.na(over)) ) stop ("'over' contains missing values", call. = FALSE)

        over.levels <- sort( levels(over) )

        n.over.levels <- length(over.levels)

        if (is.null(over.labels.values)) {

            over2 <- over
            over2.levels <- over.levels

        } else {
            # cat.print(nrow(over.labels.values))
            # cat.print(n.over.levels)
            # cat.print(over)

            if (nrow(over.labels.values) != n.over.levels) stop("Inappropriate number of rows in 'over.labels.values'", call. = FALSE)
            if (ncol(over.labels.values) != 2) stop("Inappropriate number of cols in 'over.labels.values'", call. = FALSE)
            if (!all(sort(over.levels) == sort(over.labels.values[,1]) )) stop("Column 1 of 'over.labels.values' contains some inappropriate values", call. = FALSE)
            colnames(over.labels.values) <- c("old", "new")
            over2 <- merge.data.frame(
                data.frame(order = 1:length(over), old = over),
                over.labels.values)
            over2 <- factor( over2[order(over2$order), 3] )
            over2.levels <- sort( levels(over2) )

        }

        n.over.levels <- length(over2.levels)
    }

    # 'by' is a factor ----

    if (class(by) == "factor"){

        by.levels <- sort( unique(by) )
        n.levels <-  length(by.levels)

        if (is.null(by.labels.values)) {

            by2 <- by
            by2.levels <- by.levels

        } else {

            if (nrow(by.labels.values) != n.levels) stop("Inappropriate number of rows in 'by.labels.values'", call. = FALSE)
            if (ncol(by.labels.values) != 2) stop("Inappropriate number of cols in 'by.labels.values'", call. = FALSE)
            if (!all(sort(by.levels) == sort(by.labels.values[,1])) ) stop("Column 1 of 'by.labels.values' contains some inappropriate values", call. = FALSE)
            colnames(by.labels.values) <- c("old", "new")
            by2.levels <- merge.data.frame(
                data.frame(old = by.levels),
                by.labels.values)[,2]
            by2 <- merge.data.frame(
                data.frame(order = 1:length(by), old = by),
                by.labels.values)
            by2 <- factor( by2[order(by2$order), 3] )
            by2.levels <- sort( levels(by2) )

        }

        # cat.print(table(by2))
        # cat.print(by2.levels)

        if (obj$do.TTb) {

            ## TTa and TTb ----

            if (is.null(over)) {

                # single "by" ----

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

            } else {

                # double "by" ----

                ## TTb ----

                # cat(" factor double 'by'\n")

                n.by.levels <-  n.levels
                atet <- atet.sd <- bY <- oY <- numeric(n.by.levels*n.over.levels)
                by10 <- over10 <-  character(n.by.levels*n.over.levels)

                for (b in 1:n.by.levels) {
                    for (o in 1:n.over.levels) {
                        sele <- by2 == by2.levels[b] & over2 == over2.levels[o]
                        index <- (b-1)*(n.over.levels) + o
                        if ( sum(sele) == 0 ) {
                            atet[index] <- NA
                            atet.sd[index] <- NA
                        } else if (sum(sele) == 1) {
                            atet[index] <- obj$TTb.i[sele]
                            atet.sd[index] <- sd(obj$TTb.i.boot[sele,])
                        } else {
                            atet[index] <- mean(obj$TTb.i[sele])
                            atet.sd[index] <- sd(colMeans(obj$TTb.i.boot[sele,]))
                        }
                        by10[index] <- as.character(by2.levels[b])
                        over10[index] <- over2.levels[o]
                        bY[index] <- b
                        oY[index] <- o
                    }
                }

                d1b <-
                    data.frame(
                        atet = atet,
                        atet.sd = atet.sd,
                        by = by10,
                        over = over10
                    )
                d1b <- d1b[complete.cases(d1b),]

                plot.b <- ggplot(d1b, aes(fill = over, y = atet, x = by)) +
                    geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
                    geom_errorbar(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), width= .2, position = position_dodge(0.8)) +
                    labs(x = xlab, y = ylab) +
                    geom_point(size = point_size, position = position_dodge(0.8)) +
                    scale_fill_discrete(
                        name = over.lab
                    ) +
                    theme_bw() +
                    theme(text = element_text(size = text_size))

                # cat(" factor double 'by': end\n")

                ## TTa ----

                by2 <- by2[obj$TTa.positions.in.TTb]

                over2 <- over2[obj$TTa.positions.in.TTb]

                n.by.levels <-  n.levels
                atet <- atet.sd <- bY <- oY <- numeric(n.by.levels*n.over.levels)
                by10 <- over10 <-  character(n.by.levels*n.over.levels)

                for (b in 1:n.by.levels) {
                    for (o in 1:n.over.levels) {
                        sele <- by2 == by2.levels[b] & over2 == over2.levels[o]
                        index <- (b-1)*(n.over.levels) + o
                        if ( sum(sele) == 0 ) {
                            atet[index] <- NA
                            atet.sd[index] <- NA
                        } else if (sum(sele) == 1) {
                            atet[index] <- obj$TTa.i[sele]
                            atet.sd[index] <- sd(obj$TTa.i.boot[sele,])
                        } else {
                            atet[index] <- mean(obj$TTa.i[sele])
                            atet.sd[index] <- sd(colMeans(obj$TTa.i.boot[sele,]))
                        }
                        by10[index] <- as.character(by2.levels[b])
                        # cat.print(as.character( by2.levels[b]) )
                        # cat.print(by10[index])
                        over10[index] <- over2.levels[o]
                        bY[index] <- b
                        oY[index] <- o
                    }
                }

                d1a <-
                    data.frame(
                        atet = atet,
                        atet.sd = atet.sd,
                        by = by10,
                        over = over10
                    )
                d1a <- d1a[complete.cases(d1a),]

                plot.a <- ggplot(d1a, aes(fill = over, y = atet, x = by)) +
                    geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
                    # viridis::scale_fill_viridis(discrete = TRUE, option = "A") +
                    geom_errorbar(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), width= .2, position = position_dodge(0.8)) +
                    labs(x = xlab, y = ylab) +
                    geom_point(size = point_size, position = position_dodge(0.8)) +
                    scale_fill_discrete(
                        name = over.lab
                    ) +
                    theme_bw() +
                    theme(text = element_text(size = text_size))

            }

            tymch <- list(plot.a = plot.a, plot.b = plot.b, data.a = d1a, data.b = d1b)
            class(tymch) <- c("didnpplot", "didnp")
            return(tymch)

        } else {

            # TTa ----

            if (is.null(over)) {

                # single "by" ----

                atet <- atet.sd <- numeric(n.levels)

                for(i in 1:n.levels){
                    atet[i] <- mean(obj$TTa.i[by == by.levels[i]])
                    atet.sd[i] <- sd(colMeans(obj$TTa.i.boot[by == by.levels[i],]))
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
                        name = xlab#,
                        # labels = c("Traditional", "Comprehensive", "Technology")
                    ) +
                    theme_bw() +
                    theme(legend.position = "none", text = element_text(size = text_size))

            } else {

                # double "by" ----

                # cat(" factor double 'by'\n")

                n.by.levels <-  n.levels
                atet <- atet.sd <- bY <- oY <- numeric(n.by.levels*n.over.levels)
                by10 <- over10 <-  character(n.by.levels*n.over.levels)

                for (b in 1:n.by.levels) {
                    for (o in 1:n.over.levels) {
                        sele <- by2 == by2.levels[b] & over2 == over2.levels[o]
                        index <- (b-1)*(n.over.levels) + o
                        if ( sum(sele) == 0 ) {
                            atet[index] <- NA
                            atet.sd[index] <- NA
                        } else if (sum(sele) == 1) {
                            atet[index] <- obj$TTa.i[sele]
                            atet.sd[index] <- sd(obj$TTa.i.boot[sele,])
                        } else {
                            atet[index] <- mean(obj$TTa.i[sele])
                            atet.sd[index] <- sd(colMeans(obj$TTa.i.boot[sele,]))
                        }
                        by10[index] <- as.character(by2.levels[b])
                        # cat.print(as.character( by2.levels[b]) )
                        # cat.print(by10[index])
                        over10[index] <- over2.levels[o]
                        bY[index] <- b
                        oY[index] <- o
                    }
                }

                d1a <-
                    data.frame(
                        atet = atet,
                        atet.sd = atet.sd,
                        by = by10,
                        over = over10
                    )
                d1a <- d1a[complete.cases(d1a),]

                plot.a <- ggplot(d1a, aes(fill = over, y = atet, x = by)) +
                    geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
                    # viridis::scale_fill_viridis(discrete = TRUE, option = "A") +
                    geom_errorbar(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), width= .2, position = position_dodge(0.8)) +
                    labs(x = xlab, y = ylab) +
                    geom_point(size = point_size, position = position_dodge(0.8)) +
                    scale_fill_discrete(
                        name = xlab
                    ) +
                    theme_bw() +
                    theme(text = element_text(size = text_size))

                # cat(" factor double 'by': end\n")

            }

            tymch <- list(plot.a = plot.a, data.a = d1a)
            class(tymch) <- c("didnpplot", "didnp")
            return(tymch)

        }

    } else if (class(by) == "numeric"){

        # 'by' is a continuous ----

        if (obj$do.TTb) {

            ## TTa and TTb ----

            ## TTb ----

            # cat("TTb begin\n")

            my.by <- by

            by <- base::cut(my.by, n.intervals)

            by.levels <- sort( levels(by) )

            n.levels <-  length(by.levels)

            if (is.null(over)) {

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
                d1b <- d1b[complete.cases(d1b),]

                plot.b <- ggplot(d1b, aes(x = by, y = atet, group = 1)) +
                    geom_ribbon(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), alpha = 0.3) +
                    geom_line(linewidth = line_width) +
                    geom_point(size = point_size, shape = 16, color = "black") +
                    labs(x = xlab, y = ylab) +
                    theme_bw() +
                    theme(legend.position = "none", text = element_text(size = text_size))

            } else {

                # double "by" ----

                n.by.levels <-  n.levels
                atet <- atet.sd <- numeric(n.by.levels*n.over.levels)
                by10 <- over10 <-  character(n.by.levels*n.over.levels)

                for (b in 1:n.by.levels) {
                    for (o in 1:n.over.levels) {
                        sele <- by == by.levels[b] & over2 == over2.levels[o]
                        index <- (b-1)*(n.over.levels) + o
                        if ( sum(sele) == 0 ) {
                            atet[index] <- NA
                            atet.sd[index] <- NA
                        } else if (sum(sele) == 1) {
                            atet[index] <- obj$TTb.i[sele]
                            atet.sd[index] <- sd(obj$TTb.i.boot[sele,])
                        } else {
                            atet[index] <- mean(obj$TTb.i[sele])
                            atet.sd[index] <- sd(colMeans(obj$TTb.i.boot[sele,]))
                        }
                        by10[index] <- by.levels[b]
                        over10[index] <- over2.levels[o]
                    }
                }

                d1b <-
                    data.frame(
                        atet = atet,
                        atet.sd = atet.sd,
                        by = by10,
                        over = over10
                    )
                # cat.print(d1b)
                d1b <- d1b[complete.cases(d1b),]

                # cat.print(d1b)

                plot.b <- ggplot(d1b,aes(x = by, y = atet, color = over, group = over)) +
                    geom_line(linewidth = line_width) +
                    geom_point(size = point_size, shape = 16, color = "black") +
                    labs(x = xlab, y = ylab) +
                    geom_ribbon(aes(ymin = atet - 2*atet.sd,
                                    ymax = atet + 2*atet.sd,
                                    fill = over), alpha = 0.3) +
                    guides(color = guide_legend(paste0(over.lab)), fill = guide_legend(paste0(over.ci.lab))) +
                    theme_bw() +
                    theme(legend.position = "right", text = element_text(size = 17))

            }

            # cat("TTb end\n")


            ## TTa ----

            # cat("TTa begin\n")


            by <- my.by[obj$TTa.positions.in.TTb]

            my.by <- by

            by <- base::cut(my.by, n.intervals)

            by.levels <- sort( levels(by) )

            n.levels <-  length(by.levels)

            if (is.null(over)) {

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
                d1a <- d1a[complete.cases(d1a),]

                plot.a <- ggplot(d1a, aes(x = by, y = atet, group = 1)) +
                    geom_ribbon(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), alpha = 0.3) +
                    geom_line(linewidth = line_width) +
                    geom_point(size = point_size, shape = 16, color = "black") +
                    labs(x = xlab, y = ylab) +
                    theme_bw() +
                    theme(legend.position = "none", text = element_text(size = text_size))

            } else {

                # double "by" ----

                over2 <- over2[obj$TTa.positions.in.TTb]

                n.by.levels <-  n.levels
                atet <- atet.sd <- numeric(n.by.levels*n.over.levels)
                by10 <- over10 <-  character(n.by.levels*n.over.levels)

                for (b in 1:n.by.levels) {
                    for (o in 1:n.over.levels) {
                        sele <- by == by.levels[b] & over2 == over2.levels[o]
                        index <- (b-1)*(n.over.levels) + o
                        if ( sum(sele) == 0 ) {
                            atet[index] <- NA
                            atet.sd[index] <- NA
                        } else if (sum(sele) == 1) {
                            atet[index] <- obj$TTa.i[sele]
                            atet.sd[index] <- sd(obj$TTa.i.boot[sele,])
                        } else {
                            atet[index] <- mean(obj$TTa.i[sele])
                            atet.sd[index] <- sd(colMeans(obj$TTa.i.boot[sele,]))
                        }
                        by10[index] <- by.levels[b]
                        over10[index] <- over2.levels[o]
                    }
                }

                d1a <-
                    data.frame(
                        atet = atet,
                        atet.sd = atet.sd,
                        by = by10,
                        over = over10
                    )
                # cat.print(d1a)
                d1a <- d1a[complete.cases(d1a),]

                # cat.print(d1a)

                plot.a <- ggplot(d1a,aes(x = by, y = atet, color = over, group = over)) +
                    geom_line(linewidth = line_width) +
                    geom_point(size = point_size, shape = 16, color = "black") +
                    labs(x = xlab, y = ylab) +
                    geom_ribbon(aes(ymin = atet - 2*atet.sd,
                                    ymax = atet + 2*atet.sd,
                                    fill = over), alpha = 0.3) +
                    guides(color = guide_legend(paste0(over.lab)), fill = guide_legend(paste0(over.ci.lab))) +
                    theme_bw() +
                    theme(legend.position = "right", text = element_text(size = 17))

            }



            # cat("TTa end\n")

            tymch <- list(plot.a = plot.a, plot.b = plot.b, data.a = d1a, data.b = d1b)
            class(tymch) <- c("didnpplot", "didnp")
            return(tymch)

        } else {

            # TTa ----

            my.by <- by

            by <- base::cut(my.by, n.intervals)

            by.levels <- sort( levels(by) )

            # cat.print(by.levels)

            n.levels <-  length(by.levels)

            if (is.null(over)) {

                # single "by" ----

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
                d1 <- d1[complete.cases(d1),]

                plot.a <- ggplot(d1, aes(x = by, y = atet, group = 1)) +
                    geom_ribbon(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), alpha = 0.3) +
                    geom_line(linewidth = line_width) +
                    geom_point(size = point_size, shape = 16, color = "black") +
                    labs(x = xlab, y = ylab) +
                    theme_bw() +
                    theme(legend.position = "none", text = element_text(size = text_size))

            } else {

                # double "by" ----

                n.by.levels <-  n.levels
                atet <- atet.sd <- bY <- oY <- numeric(n.by.levels*n.over.levels)
                by10 <- over10 <-  character(n.by.levels*n.over.levels)

                for (b in 1:n.by.levels) {
                    for (o in 1:n.over.levels) {
                        sele <- by == by.levels[b] & over2 == over2.levels[o]
                        # cat.print(table(sele))
                        # cat.print(sum(sele))
                        # cat.print(dim(obj$TTa.i.boot))
                        # cat.print(dim(obj$TTa.i.boot[sele,]))
                        index <- (b-1)*(n.over.levels) + o
                        # print(c( b, o, 1111, index))
                        if ( sum(sele) == 0 ) {
                            atet[index] <- NA
                            atet.sd[index] <- NA
                        } else if (sum(sele) == 1) {
                            # cat.print(obj$TTa.i.boot[sele,])
                            # cat.print(obj$TTa.i[sele])
                            atet[index] <- obj$TTa.i[sele]
                            atet.sd[index] <- sd(obj$TTa.i.boot[sele,])
                        } else {
                            atet[index] <- mean(obj$TTa.i[sele])
                            atet.sd[index] <- sd(colMeans(obj$TTa.i.boot[sele,]))
                        }
                        by10[index] <- by.levels[b]
                        over10[index] <- over2.levels[o]
                        bY[index] <- b
                        oY[index] <- o
                    }
                }

                d1 <-
                    data.frame(
                        atet = atet,
                        atet.sd = atet.sd,
                        by = by10,
                        over = over10
                    )
                # cat.print(d1)
                d1 <- d1[complete.cases(d1),]

                # cat.print(d1)

                plot.a <- ggplot(d1,aes(x = by, y = atet, color = over, group = over)) +
                    geom_line(linewidth = line_width) +
                    geom_point(size = point_size, shape = 16, color = "black") +
                    labs(x = xlab, y = ylab) +
                    geom_ribbon(aes(ymin = atet - 2*atet.sd,
                                    ymax = atet + 2*atet.sd,
                                    fill = over), alpha = 0.3) +
                    guides(color = guide_legend(paste0(over.lab)), fill = guide_legend(paste0(over.ci.lab))) +
                    theme_bw() +
                    theme(legend.position = "right", text = element_text(size = 17))

            }

            tymch <- list(plot.a = plot.a, data.a = d1)
            class(tymch) <- c("didnpplot", "didnp")
            return(tymch)


        }

    } else {
        stop("Inappropriate class of 'by'", call. = FALSE)
    }






}
