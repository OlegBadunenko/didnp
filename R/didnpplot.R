#' Plotting Objects After Estimation of the Heterogenous Treatment Effects
#'
#' The \code{didnpplot} take output created by \link{didnpreg} and produces plots of Heterogeneous Treatment Effects with their corresponding confidence bounds. Average effects per group are produced for discrete covariates and a continuum of effects are produced for continuous covariates.
#'
# see examples here
# https://friendly.github.io/nestedLogit/articles/plotting-ggplot.html
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
#'
#' @param obj an object of class "didnp".
#' @param type type of the plot. Default is 'hte'
#' @param by categorical or continuous.
#' @param by.continuous.scale The scale of a continuous variable can be set to three values. (1) Setting it to NULL implies that each unique value in the ‘by’ variable will be treated separately. (2) If you set it to a numeric vector of length 1 or a scalar, it will split the range of the continuous ‘by’ variable into intervals of the specified length. (3) If you set it to a numeric vector, it will split the continuous ‘by’ variable into intervals defined by the specified vector. The default value is NULL.
#' @param over necessarily categorical.
#' @param xlab Label for horizontal axis. Default is "".
#' @param ylab Label for vertical axis. Default is "ATET".
#' @param point_size of ATET. Default is 3.
#' @param line_width of ATET for the numeric "by". Default is 2.
#' @param by.labels.values For the categorical “by” parameter, the dataframe should have two columns. The first column should contain unique values for the 'by' parameter, while the second column should contain corresponding values that would be displayed on a graph. By default, 'by.labels.values' is set to NULL, which means that the unique values from the 'by' parameter will be used.
#' #' @param over.labels.values For the categorical “over” parameter, the dataframe should have two columns. The first column should contain unique values for the 'over' parameter, while the second column should contain corresponding values that would be displayed on a graph. By default, 'over.labels.values' is set to NULL, which means that the unique values from the 'over' parameter will be used.
#' @param text_size for ggplot object. Default is 17.
#' @param print.level The amount of printed output can be set to 0, 1, or 2. When set to 0, nothing is printed. When set to 1, only the structure of the work is printed. When set to 2, both the structure and the additional working are printed. The default value is 1.
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
#' @rdname didnpplot
#' @export
didnpplot <- function(
    obj,
    type = "hte",
    level = 95,
    by = NULL,
    by.continuous.scale = NULL,
    over = NULL,
    xlab = "",
    ylab = "ATET",
    over.lab = "ATET over",
    over.ci.lab = "Conf.Int.",
    point_size = 3,
    line_width = 2,
    by.labels.values = NULL,
    over.labels.values = NULL,
    text_size = 17,
    print.level = 1){

  # print.level <- 1 # print structure
  # print.level <- 2 # print working


  if(sum(class(obj) == "didnpreg") != 1) stop ("Run 'didnpreg' before calling 'didnpreg'", call. = FALSE)

  if (obj$TTx == "TTa") {
    TTb <- FALSE
  } else {
    TTb <- TRUE
  }
  do.TTb <- TTb

  if(type == "hte"){

    if (is.null(by)) stop("Option 'by' can't be empty")

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

      ## only 'by' ----

      if (do.TTb) {
        # do two plots: for TTa + TTb
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

      ## 'by' + 'over' ----

      if (do.TTb) {
        # do two plots: for TTa + TTb
        if (length(by) != length(obj$TTb.i)) stop ("The length of 'by' must be equal to the length of 'TTb.i'", call. = FALSE)
        if (length(over) != length(obj$TTb.i)) stop ("The length of 'over' must be equal to the length of 'TTb.i'", call. = FALSE)

      } else {
        # do one plot
        if (length(by) != length(obj$TTa.i)) stop ("The length of 'by' must be equal to the length of 'TTa.i'", call. = FALSE)
        if (length(over) != length(obj$TTa.i)) stop ("The length of 'over' must be equal to the length of 'TTa.i'", call. = FALSE)

      }
    }

    # check missing values ----

    if ( sum(is.na(by)) ) stop ("'by' contains missing values", call. = FALSE)

    ## handle "over" ----

    if (!is.null(over)) {

      # give a vector of length 2, so I use
      # sum(class(over) == "factor") == 0
      # instead of
      # class(over) != "factor"


      if ( sum(class(over) == "factor") == 0 ) stop("Inappropriate class of 'over'; must be a 'factor'", call. = FALSE)
      if ( sum(is.na(over)) ) stop ("'over' contains missing values", call. = FALSE)

      over.levels <- levels(over) # why sort?
      n.over.levels <- length(over.levels)

      if (is.null(over.labels.values)) {

        over.labels.values.supplied <- FALSE

        over2 <- over
        over2.levels <- over.levels

        over.labels.values <- data.frame(over.levels, over.levels)

      } else {
        # cat.print(nrow(over.labels.values))
        # cat.print(n.over.levels)
        # cat.print(over)

        if (nrow(over.labels.values) != n.over.levels) stop("Inappropriate number of rows in 'over.labels.values'", call. = FALSE)
        if (ncol(over.labels.values) != 2) stop("Inappropriate number of cols in 'over.labels.values'", call. = FALSE)
        if (!all(sort(over.levels) == sort(over.labels.values[,1]) )) stop("Column 1 of 'over.labels.values' contains some inappropriate values", call. = FALSE)

        over.labels.values.supplied <- TRUE

        colnames(over.labels.values) <- c("old", "new")
        over2 <- merge.data.frame(
          data.frame(order = 10+1:length(over), old = over),
          over.labels.values)

        over2 <- factor( over2[order(over2$order), 3] )
        over2.levels <- levels(over2) # why sort?

      }

      if(print.level >= 2) print(over2.levels)

      over.over.levels <- over.labels.values
      colnames(over.over.levels) <- c("over", "overnew")
      over.over.levels <- over.over.levels[order(over.over.levels[,1]),]
      over.over.levels$overSorted <- 10+1:length(over2.levels)
      # data.frame(over = over.levels, overnew = over2.levels, overSorted = 10+1:length(over2.levels))
      if(print.level >= 2) cat.print(over.over.levels)

      # n.over.levels <- length(over2.levels)
    }
    # will work with over2

    # graph ----

    # 1. 'by' is factor ----


    # print(class(by))
    # give a vector of length 2, so I use
    # sum(class(by) == "factor") >= 1
    # instead of
    # class(by) == "factor"

    if (sum(class(by) == "factor") >= 1){

      if(print.level >= 1) print("1. 'by' is catergorical")

      by.levels <- unique(by) # why sort?
      n.by.levels <-  length(by.levels)

       if (is.null(by.labels.values)) {

        by2 <- by
        by2.levels <- by.levels

        by.labels.values.supplied <- FALSE

      } else {

        if (nrow(by.labels.values) != n.by.levels) stop("Inappropriate number of rows in 'by.labels.values'", call. = FALSE)
        if (ncol(by.labels.values) != 2) stop("Inappropriate number of cols in 'by.labels.values'", call. = FALSE)
        if (!all(sort(by.levels) == sort(by.labels.values[,1])) ) stop("Column 1 of 'by.labels.values' contains some inappropriate values", call. = FALSE)

        by.labels.values.supplied <- TRUE

        colnames(by.labels.values) <- c("old", "new")
        by2.levels <- merge.data.frame(
          data.frame(old = by.levels),
          by.labels.values)[,2]
        by2 <- merge.data.frame(
          data.frame(order = 1:length(by), old = by),
          by.labels.values)
        by2 <- factor( by2[order(by2$order), 3] )
        by2.levels <- levels(by2) # why sort?

      }

      # to avoid incorrect sorting of levels in 'ggplot'
      by.levels.sorted <- as.character(10+1:n.by.levels)
      by.by.sorted <- data.frame(by = by.levels, bySorted = by.levels.sorted)
      if(print.level >= 2) print(by.by.sorted)
      if(n.by.levels > 10){
        myAngle = 90
        myVjust = 0.5
        myHjust = 1
      } else {
        myAngle = NULL
        myVjust = NULL
        myHjust = NULL
      }

      # cat.print(table(by2))
      # cat.print(by2.levels)

      if (do.TTb) {

        ## 1.1 TTa + TTb ----
        if(print.level >= 1) print("1.1 TTa + TTb")

        if (is.null(over)) {

          # 1.1.1 only 'by' ----

          ## 1.1.1.1 TTb ----
          if(print.level >= 1) print("1.1.1.1 TTb")

          atet <- atet.sd <- myCount <- numeric(n.by.levels)

          for(i in 1:n.by.levels){
            sele <- by == by.levels[i]
            myCount[i] <- sum(sele)
            atet[i] <- mean(obj$TTb.i[ sele ])
            atet.sd[i] <- sd(colMeans(obj$TTb.i.boot[sele,]))
          }

          d1b <-
            data.frame(
              atet = atet,
              atet.sd = atet.sd,
              count = myCount,
              by = by.levels
            )

          if ( by.labels.values.supplied ) {

            colnames(by.labels.values) <- c("by", "by2")
            d1b <- merge.data.frame(d1b, by.labels.values)
            # figure indices
            which( colnames(d1b) == "by" ) -> by.index
            which( colnames(d1b) == "by2" ) -> by2.index
            # rename
            colnames(d1b)[by.index] <- "byold"
            colnames(d1b)[by2.index] <- "by"
            d1b <- d1b[, c("atet", "atet.sd", "count", "by", "byold")]

          }

          plot.b <- ggplot(d1b, aes(x = by, y = atet, fill = by)) +
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

          ## 1.1.1.2 TTa ----

          by.a <- by[obj$TTa.positions.in.TTb]

          atet <- atet.sd <- myCount <- numeric(n.by.levels)

          for(i in 1:n.by.levels){
            sele <- by.a == by.levels[i]
            myCount[i] <- sum(sele)
            atet[i] <- mean(obj$TTa.i[sele])
            atet.sd[i] <- sd(colMeans(obj$TTa.i.boot[sele,]))
          }

          d1a <-
            data.frame(
              atet = atet,
              atet.sd = atet.sd,
              count = myCount,
              by = by.levels
            )

          if ( by.labels.values.supplied ) {

            colnames(by.labels.values) <- c("by", "by2")
            d1a <- merge.data.frame(d1a, by.labels.values)
            # figure indices
            which( colnames(d1a) == "by" ) -> by.index
            which( colnames(d1a) == "by2" ) -> by2.index
            # rename
            colnames(d1a)[by.index] <- "byold"
            colnames(d1a)[by2.index] <- "by"
            d1a <- d1a[, c("atet", "atet.sd", "count", "by", "byold")]

          }

          plot.a <- ggplot(d1a, aes(x = by, y = atet, fill = by)) +
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

          # 1.1.2 'by' + 'over' ----
          if(print.level >= 1) print("1.1.2 'by' + 'over'")

          ## TTb ----

          # cat(" factor double 'by'\n")

          # n.by.levels <-  n.levels
          atet <- atet.sd <- bY <- oY <- myCount <- numeric(n.by.levels*n.over.levels)
          by10 <- over10 <-  character(n.by.levels*n.over.levels)

          for (b in 1:n.by.levels) {
            for (o in 1:n.over.levels) {
              sele <- by == by.levels[b] & over == over.levels[o]
              index <- (b-1)*(n.over.levels) + o
              myCount[index] <- sum(sele)
              if ( myCount[index] == 0 ) {
                atet[index] <- NA
                atet.sd[index] <- NA
              } else if ( myCount[index] == 1) {
                atet[index] <- obj$TTb.i[sele]
                atet.sd[index] <- sd(obj$TTb.i.boot[sele,])
              } else {
                atet[index] <- mean(obj$TTb.i[sele])
                atet.sd[index] <- sd(colMeans(obj$TTb.i.boot[sele,]))
              }
              by10[index] <- as.character(by.levels[b])
              over10[index] <- over.levels[o]
              bY[index] <- b
              oY[index] <- o
            }
          }

          d1b <-
            data.frame(
              atet = atet,
              atet.sd = atet.sd,
              count = myCount,
              by = by10,
              over = over10
            )

          # cat.print(d1b)
          # cat.print(over.over.levels)

          d1b <- d1b[complete.cases(d1b),]
          d1b <- merge(d1b, over.over.levels)
          d1b <- merge(d1b, by.by.sorted)
          # cat.print(d1b$by)
          # cat.print(is.ordered(d1b$by))
          # cat.print(levels(d1b$by))
          # if(!keep.continuous)
          d1b <- d1b[complete.cases(d1b),]

          # cat.print(d1b)

          if ( by.labels.values.supplied & over.labels.values.supplied ) {

            colnames(by.labels.values) <- c("by", "by2")
            colnames(over.labels.values) <- c("over", "over2")
            d1b <- merge.data.frame(d1b, by.labels.values)
            d1b <- merge.data.frame(d1b, over.labels.values)
            # figure indices
            which( colnames(d1b) == "by" ) -> by.index
            which( colnames(d1b) == "by2" ) -> by2.index
            which( colnames(d1b) == "over" ) -> over.index
            which( colnames(d1b) == "over2" ) -> over2.index
            # rename
            colnames(d1b)[by.index] <- "byold"
            colnames(d1b)[by2.index] <- "by"
            colnames(d1b)[over.index] <- "overold"
            colnames(d1b)[over2.index] <- "over"
            d1b <- d1b[, c("atet", "atet.sd", "count", "by", "byold", "over", "overold", "overSorted", "bySorted")]

          }

          if ( !by.labels.values.supplied & over.labels.values.supplied ) {

            colnames(over.labels.values) <- c("over", "over2")
            d1b <- merge.data.frame(d1b, over.labels.values)
            # figure indices
            which( colnames(d1b) == "over" ) -> over.index
            which( colnames(d1b) == "over2" ) -> over2.index
            # rename
            colnames(d1b)[over.index] <- "overold"
            colnames(d1b)[over2.index] <- "over"
            d1b <- d1b[, c("atet", "atet.sd", "count", "by", "over", "overold", "overSorted", "bySorted")]

          }

          if ( by.labels.values.supplied & !over.labels.values.supplied ) {

            colnames(by.labels.values) <- c("by", "by2")
            d1b <- merge.data.frame(d1b, by.labels.values)
            # figure indices
            which( colnames(d1b) == "by" ) -> by.index
            which( colnames(d1b) == "by2" ) -> by2.index
            # rename
            colnames(d1b)[by.index] <- "byold"
            colnames(d1b)[by2.index] <- "by"
            d1b <- d1b[, c("atet", "atet.sd", "count", "by", "byold", "over", "overSorted", "bySorted")]

          }

          d1b <- d1b[order(d1b$overSorted, d1b$bySorted),]
          if(print.level >= 2) cat.print(d1b)

          plot.b <- ggplot(d1b, aes(fill = factor(over, levels = over2.levels), y = atet, x = bySorted)) +
            geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
            geom_errorbar(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), width= .2, position = position_dodge(0.8)) +
            labs(x = xlab, y = ylab) +
            scale_x_discrete(label = d1b$by) +
            geom_point(size = point_size, position = position_dodge(0.8)) +
            scale_fill_discrete(
              name = over.lab
            ) +
            theme_bw() +
            theme(text = element_text(size = text_size))

          # cat(" factor double 'by': end\n")

          ## TTa ----

          by <- droplevels( by[obj$TTa.positions.in.TTb] )
          by.levels <- levels(by)
          # cat.print(by)
          n.by.levels <- length(by.levels)
          # cat.print(n.by.levels)

          over <- droplevels( over[obj$TTa.positions.in.TTb] )
          over.levels <- levels(over)
          # cat.print(over)
          n.over.levels <- length(over.levels)
          # cat.print(n.over.levels)

          # n.by.levels <-  n.levels
          atet <- atet.sd <- bY <- oY <- myCount <- numeric(n.by.levels*n.over.levels)
          by10 <- over10 <-  character(n.by.levels*n.over.levels)

          for (b in 1:n.by.levels) {
            for (o in 1:n.over.levels) {
              sele <- by == by.levels[b] & over == over.levels[o]
              index <- (b-1)*(n.over.levels) + o
              # cat.print(index)
              myCount[index] <- sum(sele)
              if ( myCount[index] == 0 ) {
                atet[index] <- NA
                atet.sd[index] <- NA
              } else if ( myCount[index] == 1) {
                atet[index] <- obj$TTa.i[sele]
                atet.sd[index] <- sd(obj$TTa.i.boot[sele,])
              } else {
                atet[index] <- mean(obj$TTa.i[sele])
                atet.sd[index] <- sd(colMeans(obj$TTa.i.boot[sele,]))
              }
              # cat.print(atet[index])
              by10[index] <- as.character(by.levels[b])
              # cat.print(as.character( by2.levels[b]) )
              # cat.print(by10[index])
              over10[index] <- over.levels[o]
              bY[index] <- b
              oY[index] <- o
            }
          }

          d1a <-
            data.frame(
              atet = atet,
              atet.sd = atet.sd,
              count = myCount,
              by = by10,
              over = over10
            )
          # cat.print(d1a)

          d1a <- d1a[complete.cases(d1a),]
          d1a <- merge(d1a, over.over.levels)
          # cat.print(d1b$by)
          # cat.print(is.ordered(d1b$by))
          # cat.print(levels(d1b$by))
          # if(!keep.continuous)
          d1a <- d1a[complete.cases(d1a),]

          if ( by.labels.values.supplied & over.labels.values.supplied ) {

            colnames(by.labels.values) <- c("by", "by2")
            colnames(over.labels.values) <- c("over", "over2")
            d1a <- merge.data.frame(d1a, by.labels.values)
            d1a <- merge.data.frame(d1a, over.labels.values)
            # figure indices
            which( colnames(d1a) == "by" ) -> by.index
            which( colnames(d1a) == "by2" ) -> by2.index
            which( colnames(d1a) == "over" ) -> over.index
            which( colnames(d1a) == "over2" ) -> over2.index
            # rename
            colnames(d1a)[by.index] <- "byold"
            colnames(d1a)[by2.index] <- "by"
            colnames(d1a)[over.index] <- "overold"
            colnames(d1a)[over2.index] <- "over"
            d1a <- d1a[, c("atet", "atet.sd", "count", "by", "byold", "over", "overold", "overSorted")]

          }

          if ( !by.labels.values.supplied & over.labels.values.supplied ) {

            colnames(over.labels.values) <- c("over", "over2")
            d1a <- merge.data.frame(d1a, over.labels.values)
            # figure indices
            which( colnames(d1a) == "over" ) -> over.index
            which( colnames(d1a) == "over2" ) -> over2.index
            # rename
            colnames(d1a)[over.index] <- "overold"
            colnames(d1a)[over2.index] <- "over"
            d1a <- d1a[, c("atet", "atet.sd", "count", "by", "over", "overold", "overSorted")]

          }

          if ( by.labels.values.supplied & !over.labels.values.supplied ) {

            colnames(by.labels.values) <- c("by", "by2")
            d1a <- merge.data.frame(d1a, by.labels.values)
            # figure indices
            which( colnames(d1a) == "by" ) -> by.index
            which( colnames(d1a) == "by2" ) -> by2.index
            # rename
            colnames(d1a)[by.index] <- "byold"
            colnames(d1a)[by2.index] <- "by"
            d1a <- d1a[, c("atet", "atet.sd", "count", "by", "byold", "over", "overSorted")]

          }

          d1a <- d1a[order(d1a$overSorted),]
          if(print.level >= 2) print(d1a)

          plot.a <- ggplot(d1a, aes(fill = factor(over, levels = over2.levels), y = atet, x = by)) +
            geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
            geom_errorbar(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), width= .2, position = position_dodge(0.8)) +
            labs(x = xlab, y = ylab) +
            geom_point(size = point_size, position = position_dodge(0.8)) +
            scale_fill_discrete(
              name = over.lab
            ) +
            theme_bw() +
            theme(text = element_text(size = text_size))


          # plot.a <- ggplot(d1a, aes(fill = factor(over, levels = over2.levels), y = atet, x = by)) +
          #   geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
          #   # viridis::scale_fill_viridis(discrete = TRUE, option = "A") +
          #   geom_errorbar(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), width= .2, position = position_dodge(0.8)) +
          #   labs(x = xlab, y = ylab) +
          #   geom_point(size = point_size, position = position_dodge(0.8)) +
          #   scale_fill_discrete(
          #     name = over.lab
          #   ) +
          #   theme_bw() +
          #   theme(text = element_text(size = text_size))



        }

        tymch <- list(plot.a = plot.a, plot.b = plot.b, data.a = d1a, data.b = d1b)
        class(tymch) <- c("didnpplot", "didnp")
        return(tymch)

      } else {

        # 1.2 TTa ----

        if (is.null(over)) {

          # 1.2.1 only 'by' ----

          atet <- atet.sd <- myCount <- numeric(n.by.levels)

          for(i in 1:n.by.levels){
            sele <- by == by.levels[i]
            myCount[i] <- sum(sele)
            atet[i] <- mean(obj$TTa.i[sele])
            atet.sd[i] <- sd(colMeans(obj$TTa.i.boot[sele,]))
          }

          d1a <-
            data.frame(
              atet = atet,
              atet.sd = atet.sd,
              count = myCount,
              by = by.levels
            )

          if ( by.labels.values.supplied ) {

            colnames(by.labels.values) <- c("by", "by2")
            d1a <- merge.data.frame(d1a, by.labels.values)
            # figure indices
            which( colnames(d1a) == "by" ) -> by.index
            which( colnames(d1a) == "by2" ) -> by2.index
            # rename
            colnames(d1a)[by.index] <- "byold"
            colnames(d1a)[by2.index] <- "by"
            d1a <- d1a[, c("atet", "atet.sd", "count", "by", "byold")]

          }

          plot.a <- ggplot(d1a, aes(x = by, y = atet, fill = by)) +
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

          # 1.2.2 'by' + 'over' ----

          # cat(" factor 'by' + 'over'\n")

          n.by.levels <-  n.by.levels
          atet <- atet.sd <- bY <- oY <- myCount <- numeric(n.by.levels*n.over.levels)
          by10 <- over10 <-  character(n.by.levels*n.over.levels)

          for (b in 1:n.by.levels) {
            for (o in 1:n.over.levels) {
              sele <- by == by.levels[b] & over == over.levels[o]
              index <- (b-1)*(n.over.levels) + o
              myCount[index] <- sum(sele)
              if ( myCount[index] == 0 ) {
                atet[index] <- NA
                atet.sd[index] <- NA
              } else if ( myCount[index] == 1) {
                atet[index] <- obj$TTa.i[sele]
                atet.sd[index] <- sd(obj$TTa.i.boot[sele,])
              } else {
                atet[index] <- mean(obj$TTa.i[sele])
                atet.sd[index] <- sd(colMeans(obj$TTa.i.boot[sele,]))
              }
              by10[index] <- as.character(by.levels[b])
              # cat.print(as.character( by2.levels[b]) )
              # cat.print(by10[index])
              over10[index] <- over.levels[o]
              bY[index] <- b
              oY[index] <- o
            }
          }

          d1a <-
            data.frame(
              atet = atet,
              atet.sd = atet.sd,
              count = myCount,
              by = by10,
              over = over10
            )
          d1a <- d1a[complete.cases(d1a),]

          if ( by.labels.values.supplied & over.labels.values.supplied ) {
            cat("Case 1\n")

            colnames(by.labels.values) <- c("by", "by2")
            colnames(over.labels.values) <- c("over", "over2")
            # cat.print(head(d1a,1))
            d1a <- merge.data.frame(d1a, by.labels.values)
            # cat.print(head(d1a,2))
            d1a <- merge.data.frame(d1a, over.labels.values)
            # cat.print(head(d1a,3))
            # figure indices
            which( colnames(d1a) == "by" ) -> by.index
            which( colnames(d1a) == "by2" ) -> by2.index
            which( colnames(d1a) == "over" ) -> over.index
            which( colnames(d1a) == "over2" ) -> over2.index
            # rename
            colnames(d1a)[by.index] <- "byold"
            colnames(d1a)[by2.index] <- "by"
            colnames(d1a)[over.index] <- "overold"
            colnames(d1a)[over2.index] <- "over"
            # cat.print(head(d1a,4))
            d1a <- d1a[, c("atet", "atet.sd", "count", "by", "byold", "over", "overold")]
            # cat.print(head(d1a,5))
          }

          if ( !by.labels.values.supplied & over.labels.values.supplied ) {
            cat("Case 2\n")

            colnames(over.labels.values) <- c("over", "over2")
            d1a <- merge.data.frame(d1a, over.labels.values)
            # figure indices
            which( colnames(d1a) == "over" ) -> over.index
            which( colnames(d1a) == "over2" ) -> over2.index
            # rename
            colnames(d1a)[over.index] <- "overold"
            colnames(d1a)[over2.index] <- "over"
            d1a <- d1a[, c("atet", "atet.sd", "count", "by", "over", "overold")]

          }

          if ( by.labels.values.supplied & !over.labels.values.supplied ) {
            cat("Case 3\n")

            colnames(by.labels.values) <- c("by", "by2")
            d1a <- merge.data.frame(d1a, by.labels.values)
            # figure indices
            which( colnames(d1a) == "by" ) -> by.index
            which( colnames(d1a) == "by2" ) -> by2.index
            # rename
            colnames(d1a)[by.index] <- "byold"
            colnames(d1a)[by2.index] <- "by"
            d1a <- d1a[, c("atet", "atet.sd", "count", "by", "byold", "over")]

          }

          if(print.level >= 2) cat.print(d1a)

          plot.a <- ggplot(d1a, aes(fill = factor(over, levels = over2.levels), y = atet, x = by)) +
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

      # 2. 'by' is continuous ----
      if(print.level >= 1) print("2. 'by' is continuous")

      my.by0 <- my.by <- by


      if(is.null(by.continuous.scale)){
        print("Scale of the continuous 'by' is its range")

        keep.continuous <- TRUE

        by.levels <- sort( unique(by) ) # why sort?

        n.by.levels <-  length(by.levels)

      } else if (class(by.continuous.scale) == "numeric") {

        keep.continuous <- FALSE

        if(length(by.continuous.scale) == 1){
          print("Scale of the continuous 'by' is determined by the number of intervals 'by.continuous.scale'")

          by <- base::cut(my.by, by.continuous.scale, ordered_result = TRUE)#, dig.lab = 1)
          # print(by)
          by.levels <- levels(by) # why sort?
          # print(by.levels)

          n.by.levels <-  length(by.levels)

        } else {
          # check if the given 'by.continuous.scale' is within the range of 'by'
          inside <- (min(by) < by.continuous.scale) & (max(by) > by.continuous.scale)
          if(sum( inside ) > 1){
            print("Scale of the continuous 'by' is determined by the vector in 'by.continuous.scale'")

            by.continuous.scale.work <-
              unique(  c(min(by), sort(by.continuous.scale[inside]), max(by)) )

            # print(summary(by))
            # print(by.continuous.scale)
            # print(inside)
            # print(by.continuous.scale.work)

            by <- base::cut(my.by, breaks = by.continuous.scale.work, include.lowest = TRUE, ordered_result = TRUE)#, dig.lab = 1)

            by.levels <- levels(by) # why sort?

            n.by.levels <-  length(by.levels)

          } else {
            stop("Values of 'by.continuous.scale' are outside the range of 'by'", call. = FALSE)
          }
        }
      } else {
        stop("The argument 'by.continuous.scale' should be numeric", call. = FALSE)
      }

      # to avoid incorrect sorting of levels in 'ggplot'
      by.levels.sorted <- as.character(10+1:n.by.levels)
      by.by.sorted <- data.frame(by = by.levels, bySorted = by.levels.sorted)
      if(print.level >= 2)  print(by.by.sorted)
      if(n.by.levels > 10){
        myAngle = 90
        myVjust = 0.5
        myHjust = 1
      } else {
        myAngle = NULL
        myVjust = NULL
        myHjust = NULL
      }

      if (do.TTb) {

        ## 2.1 TTa + TTb ----
        if(print.level >= 1) print("2.1 TTa + TTb")

        # cat("TTb begin\n")

        if (is.null(over)) {

          # 2.1.1 only 'by' ----
          if(print.level >= 1) print("2.1.1 only 'by'")

            ## 2.1.1.1 TTb ----
          if(print.level >= 1) print("2.1.1.1 TTb")

          # cat.print(by[1:20])
          # cat.print(length(by))

          atet <- atet.sd <- myCount <- numeric(n.by.levels)

          for(i in 1:n.by.levels){
            sele <- by == by.levels[i]
            myCount[i] <- sum(sele)
            if(myCount[i] == 1){
              atet[i] <- obj$TTb.i[sele]
              atet.sd[i] <- sd(obj$TTb.i.boot[sele,])
            } else {
              atet[i] <- mean(obj$TTb.i[sele])
              atet.sd[i] <- sd(colMeans(obj$TTb.i.boot[sele,]))
            }
          }

          d1b <-
            data.frame(
              atet = atet,
              atet.sd = atet.sd,
              count = myCount,
              by = by.levels
            )
          levels(d1b$by) <- by.levels
          d1b <- merge(d1b, by.by.sorted)
          # cat.print(d1b$by)
          # cat.print(is.ordered(d1b$by))
          # cat.print(levels(d1b$by))
          # if(!keep.continuous)
          d1b <- d1b[complete.cases(d1b),]
          d1b <- d1b[order(d1b$bySorted),]
          if(print.level >= 2) print(head(d1b))

          if(keep.continuous){

            plot.b <- ggplot(d1b, aes(x = by, y = atet, group = 1)) +
              geom_ribbon(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), alpha = 0.3) +
              geom_line(linewidth = line_width) +
              geom_point(size = point_size, shape = 16, color = "black") +
              labs(x = xlab, y = ylab) +
              theme_bw() +
              theme(legend.position = "none", text = element_text(size = text_size))

          } else {

            # print(by.levels)
            plot.b <- ggplot(d1b, aes(x = bySorted, y = atet, group = 1)) +
              geom_ribbon(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), alpha = 0.3) +
              geom_line(linewidth = line_width) +
              geom_point(size = point_size, shape = 16, color = "black") +
              labs(x = xlab, y = ylab) +
              scale_x_discrete(label = d1b$by) +
              theme_bw() +
              theme(legend.position = "none", text = element_text(size = text_size), axis.text.x = element_text(angle = myAngle, vjust = myVjust, hjust=myHjust))

          }

          # plot.b

          ## 2.1.1.2 TTa ----
          if(print.level >= 1) print("2.1.1.2 TTa")

          # return(plot.b)

          # cat.print(length(obj$TTa.positions.in.TTb))
          # cat.print(length(by))
          # my.by0 is the original 'by'
          by <- my.by0[obj$TTa.positions.in.TTb]
          by -> my.by
          # cat.print(length(my.by))
          # cat.print(length(by))

          if(is.null(by.continuous.scale)){
            # print("Scale of the continuous 'by' is its range")

            keep.continuous <- TRUE

            by.levels <- sort( unique(by) ) # why sort?

            n.by.levels <-  length(by.levels)

          } else if (class(by.continuous.scale) == "numeric") {

            keep.continuous <- FALSE

            if(length(by.continuous.scale) == 1){
              # print("Scale of the continuous 'by' is determined by the number of intervals in 'by.continuous.scale'")

              by <- base::cut(my.by, by.continuous.scale, ordered_result = TRUE)#, dig.lab = 1)

              by.levels <- levels(by) # why sort?

              n.by.levels <-  length(by.levels)

            } else {
              # check if the given 'by.continuous.scale' is within the range of 'by'
              inside <- (min(by) < by.continuous.scale) & (max(by) > by.continuous.scale)
              if(sum( inside ) > 1){
                # print("Scale of the continuous 'by' is determined by the vector in 'by.continuous.scale'")

                by.continuous.scale.work <-
                  unique( sort( c(min(by), sort(by.continuous.scale[inside]), max(by)) ))

                by <- base::cut(my.by, breaks = by.continuous.scale.work, include.lowest = TRUE, ordered_result = TRUE)#, dig.lab = 1)

                by.levels <- levels(by) # why sort?

                n.by.levels <-  length(by.levels)

              } else {
                stop("Values of 'by.continuous.scale' are outside the range of 'by'", call. = FALSE)
              }
            }
          } else {
            stop("The argument 'by.continuous.scale' should be numeric", call. = FALSE)
          }

          # to avoid incorrect sorting of levels in 'ggplot'
          by.levels.sorted <- as.character(10+1:n.by.levels)
          by.by.sorted <- data.frame(by = by.levels, bySorted = by.levels.sorted)
          if(print.level >= 2)  print(by.by.sorted)
          if(n.by.levels > 10){
            myAngle = 90
            myVjust = 0.5
            myHjust = 1
          } else {
            myAngle = NULL
            myVjust = NULL
            myHjust = NULL
          }


          atet <- atet.sd <- myCount <- numeric(n.by.levels)


          for(i in 1:n.by.levels){
            sele <- by == by.levels[i]
            myCount[i] <- sum(sele)
            if(myCount[i] == 1){
              atet[i] <- obj$TTa.i[sele]
              atet.sd[i] <- sd(obj$TTa.i.boot[sele,])
            } else {
              atet[i] <- mean(obj$TTa.i[sele])
              atet.sd[i] <- sd(colMeans(obj$TTa.i.boot[sele,]))
            }
          }


          d1a <-
            data.frame(
              atet = atet,
              atet.sd = atet.sd,
              count = myCount,
              by = by.levels
            )
          # d1a <- d1a[complete.cases(d1a),]

          d1a <- merge(d1a, by.by.sorted)
          # cat.print(d1b$by)
          # cat.print(is.ordered(d1b$by))
          # cat.print(levels(d1b$by))
          d1a <- d1a[complete.cases(d1a),]
          d1a <- d1a[order(d1a$bySorted),]
          if(print.level >= 2) print(head(d1a))

          if(keep.continuous){

            plot.a <- ggplot(d1a, aes(x = by, y = atet, group = 1)) +
              geom_ribbon(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), alpha = 0.3) +
              geom_line(linewidth = line_width) +
              geom_point(size = point_size, shape = 16, color = "black") +
              labs(x = xlab, y = ylab) +
              theme_bw() +
              theme(legend.position = "none", text = element_text(size = text_size))

          } else {

            plot.a <- ggplot(d1a, aes(x = bySorted, y = atet, group = 1)) +
              geom_ribbon(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), alpha = 0.3) +
              geom_line(linewidth = line_width) +
              geom_point(size = point_size, shape = 16, color = "black") +
              labs(x = xlab, y = ylab) +
              scale_x_discrete(label = d1a$by) +
              theme_bw() +
              theme(legend.position = "none", text = element_text(size = text_size), axis.text.x = element_text(angle = myAngle, vjust = myVjust, hjust=myHjust))
          }

        } else {

          # 2.1.2 'by' + 'over' ----
          if(print.level >= 1) print("2.1.2 'by' + 'over'")

          ## 2.1.2.1 TTb ----
          if(print.level >= 1) print("2.1.2.1 TTb")


          # n.by.levels <-  n.levels
          atet <- atet.sd <- myCount <- numeric(n.by.levels*n.over.levels)
          by10 <- over10 <-  character(n.by.levels*n.over.levels)
          if(keep.continuous){
            by10 <- numeric(n.by.levels*n.over.levels)
          }

          for (b in 1:n.by.levels) {
            for (o in 1:n.over.levels) {
              sele <- by == by.levels[b] & over == over.levels[o]
              index <- (b-1)*(n.over.levels) + o
              myCount[index] <- sum(sele)
              if ( myCount[index] == 0 ) {
                atet[index] <- NA
                atet.sd[index] <- NA
              } else if ( myCount[index] == 1) {
                atet[index] <- obj$TTb.i[sele]
                atet.sd[index] <- sd(obj$TTb.i.boot[sele,])
              } else {
                atet[index] <- mean(obj$TTb.i[sele])
                atet.sd[index] <- sd(colMeans(obj$TTb.i.boot[sele,]))
              }
              by10[index] <- by.levels[b]
              over10[index] <- over.levels[o]
            }
          }

          d1b <-
            data.frame(
              atet = atet,
              atet.sd = atet.sd,
              count = myCount,
              by = by10,
              over = over10
            )
          levels(d1b$by) <- by.levels
          d1b <- merge(d1b, by.by.sorted)
          # cat.print(d1b$by)
          # cat.print(is.ordered(d1b$by))
          # cat.print(levels(d1b$by))
          # if(!keep.continuous)
          d1b <- d1b[complete.cases(d1b),]
          d1b <- d1b[order(d1b$bySorted),]
          # print(head(d1b))

          if ( over.labels.values.supplied ) {

            # cat.print(head(d1b,2))

            colnames(over.labels.values) <- c("over", "over2")
            # cat.print(over.labels.values)
            d1b <- merge.data.frame(d1b, over.labels.values)
            # cat.print(head(d1b,2))
            # figure indices
            which( colnames(d1b) == "over" ) -> over.index
            which( colnames(d1b) == "over2" ) -> over2.index
            # rename
            colnames(d1b)[over.index] <- "overold"
            colnames(d1b)[over2.index] <- "over"
            # cat.print(head(d1b,2))
            d1b <- d1b[, c("atet", "atet.sd", "count", "by", "over", "overold", "bySorted")]
            d1b <- d1b[order(d1b$bySorted),]
            # cat.print(head(d1b,2))
          }

          if(print.level >= 2) cat.print(d1b)

          # print(class(d1b$by))

          if(keep.continuous){

            plot.b <- ggplot(d1b, aes(x = by, y = atet, color = over, group = over)) +
              geom_line(linewidth = line_width) +
              geom_point(size = point_size, shape = 16, color = "black") +
              labs(x = xlab, y = ylab) +
              geom_ribbon(aes(ymin = atet - 2*atet.sd,
                              ymax = atet + 2*atet.sd,
                              fill = over), alpha = 0.3) +
              guides(color = guide_legend(paste0(over.lab)), fill = guide_legend(paste0(over.ci.lab))) +
              theme_bw() +
              theme(legend.position = "right", text = element_text(size = text_size))

          } else {

            plot.b <- ggplot(d1b, aes(x = bySorted, y = atet, color = over, group = over)) +
              geom_line(linewidth = line_width) +
              geom_point(size = point_size, shape = 16, color = "black") +
              labs(x = xlab, y = ylab) +
              scale_x_discrete(label = d1b$by) +
              geom_ribbon(aes(ymin = atet - 2*atet.sd,
                              ymax = atet + 2*atet.sd,
                              fill = over), alpha = 0.3) +
              guides(color = guide_legend(paste0(over.lab)), fill = guide_legend(paste0(over.ci.lab))) +
              theme_bw() +
              theme(legend.position = "right", text = element_text(size = text_size), axis.text.x = element_text(angle = myAngle, vjust = myVjust, hjust=myHjust))

          }


          ## 2.1.2.2 TTa ----
          if(print.level >= 1) print("2.1.2.2 TTa")

          # cat.print(length(obj$TTa.positions.in.TTb))
          # cat.print(length(by))
          # my.by0 is the original 'by'
          by <- my.by0[obj$TTa.positions.in.TTb]
          by -> my.by
          # cat.print(length(my.by))
          # cat.print(length(by))

          if(is.null(by.continuous.scale)){
            # print("Scale of the continuous 'by' is its range")

            keep.continuous <- TRUE

            by.levels <- sort( unique(by) ) # why sort?

            n.by.levels <-  length(by.levels)

          } else if (class(by.continuous.scale) == "numeric") {

            keep.continuous <- FALSE

            if(length(by.continuous.scale) == 1){
              # print("Scale of the continuous 'by' is determined by the number of intervals in 'by.continuous.scale'")

              by <- base::cut(my.by, by.continuous.scale, ordered_result = TRUE)#, dig.lab = 1)

              by.levels <- levels(by) # why sort?

              n.by.levels <-  length(by.levels)

            } else {
              # check if the given 'by.continuous.scale' is within the range of 'by'
              inside <- (min(by) < by.continuous.scale) & (max(by) > by.continuous.scale)
              if(sum( inside ) > 1){
                # print("Scale of the continuous 'by' is determined by the vector in 'by.continuous.scale'")

                by.continuous.scale.work <-
                  unique( sort( c(min(by), sort(by.continuous.scale[inside]), max(by)) ))

                by <- base::cut(my.by, breaks = by.continuous.scale.work, include.lowest = TRUE, ordered_result = TRUE)#, dig.lab = 1)

                by.levels <- levels(by) # why sort?

                n.by.levels <-  length(by.levels)

              } else {
                stop("Values of 'by.continuous.scale' are outside the range of 'by'", call. = FALSE)
              }
            }
          } else {
            stop("The argument 'by.continuous.scale' should be numeric", call. = FALSE)
          }

          # to avoid incorrect sorting of levels in 'ggplot'
          by.levels.sorted <- as.character(10+1:n.by.levels)
          by.by.sorted <- data.frame(by = by.levels, bySorted = by.levels.sorted)
          if(print.level >= 2)  print(by.by.sorted)
          if(n.by.levels > 10){
            myAngle = 90
            myVjust = 0.5
            myHjust = 1
          } else {
            myAngle = NULL
            myVjust = NULL
            myHjust = NULL
          }

          # by <- my.by0[obj$TTa.positions.in.TTb]
          # by -> my.by

          # # by.a <- droplevels ( by[obj$TTa.positions.in.TTb] )
          over <- droplevels( over[obj$TTa.positions.in.TTb] )

          # cat.print(length(obj$TTa.positions.in.TTb))
          # cat.print(length(by))
          # cat.print(length(by.a))

          # cat.print(levels(by))
          # cat.print(levels(by.a))
          # cat.print(levels(droplevels(by.a)))

          # by.levels <- levels(by)

          # cat.print(by.levels)
          # cat.print(over.levels)

          # n.levels <-  length(by.levels)
          over.levels <- levels(over)
          n.over.levels <-  length(over.levels)


          # n.by.levels <-  n.levels
          atet <- atet.sd <- bY <- oY <- myCount <- numeric(n.by.levels*n.over.levels)
          by10 <- over10 <-  character(n.by.levels*n.over.levels)
          if(keep.continuous){
            by10 <- numeric(n.by.levels*n.over.levels)
          }

          for (b in 1:n.by.levels) {
            for (o in 1:n.over.levels) {
              sele <- by == by.levels[b] & over == over.levels[o]
              # cat.print(table(sele))
              # cat.print(length(sele))
              # cat.print(dim(obj$TTa.i.boot))
              # cat.print(dim(obj$TTa.i.boot[sele,]))
              index <- (b-1)*(n.over.levels) + o
              # print(c( b, o, 1111, index))
              myCount[index] <- sum(sele)
              # cat("b=",b,"; o=",o,"; myCount[index]=",myCount[index],"\n")
              if ( myCount[index] == 0 ) {
                atet[index] <- NA
                atet.sd[index] <- NA
              } else if ( myCount[index] == 1) {
                # cat.print(obj$TTa.i.boot[sele,])
                # cat.print(obj$TTa.i[sele])
                atet[index] <- obj$TTa.i[sele]
                atet.sd[index] <- sd(obj$TTa.i.boot[sele,])
              } else {
                atet[index] <- mean(obj$TTa.i[sele])
                atet.sd[index] <- sd(colMeans(obj$TTa.i.boot[sele,]))
              }
              by10[index] <- by.levels[b]
              over10[index] <- over.levels[o]
              bY[index] <- b
              oY[index] <- o
            }
          }

          d1a <-
            data.frame(
              atet = atet,
              atet.sd = atet.sd,
              count = myCount,
              by = by10,
              over = over10
            )
          # cat.print(d1)
          d1a <- d1a[complete.cases(d1a),]

          d1a <- merge(d1a, by.by.sorted)
          # cat.print(d1b$by)
          # cat.print(is.ordered(d1b$by))
          # cat.print(levels(d1b$by))
          # if(!keep.continuous)
          d1a <- d1a[complete.cases(d1a),]
          d1a <- d1a[order(d1a$bySorted),]
          # print(head(d1a))

          if ( over.labels.values.supplied ) {

            colnames(over.labels.values) <- c("over", "over2")
            d1a <- merge.data.frame(d1a, over.labels.values)
            # figure indices
            which( colnames(d1a) == "over" ) -> over.index
            which( colnames(d1a) == "over2" ) -> over2.index
            # rename
            colnames(d1a)[over.index] <- "overold"
            colnames(d1a)[over2.index] <- "over"
            d1a <- d1a[, c("atet", "atet.sd", "count", "by", "over", "overold", "bySorted")]
            d1a <- d1a[order(d1a$bySorted),]
          }

          if(print.level >= 2) cat.print(d1a)

          if(keep.continuous){

            plot.a <- ggplot(d1a, aes(x = by, y = atet, color = over, group = over)) +
              geom_line(linewidth = line_width) +
              geom_point(size = point_size, shape = 16, color = "black") +
              labs(x = xlab, y = ylab) +
              geom_ribbon(aes(ymin = atet - 2*atet.sd,
                              ymax = atet + 2*atet.sd,
                              fill = over), alpha = 0.3) +
              guides(color = guide_legend(paste0(over.lab)), fill = guide_legend(paste0(over.ci.lab))) +
              theme_bw() +
              theme(legend.position = "right", text = element_text(size = text_size))

          } else {

            plot.a <- ggplot(d1a, aes(x = bySorted, y = atet, color = over, group = over)) +
              geom_line(linewidth = line_width) +
              geom_point(size = point_size, shape = 16, color = "black") +
              labs(x = xlab, y = ylab) +
              scale_x_discrete(label = d1a$by) +
              geom_ribbon(aes(ymin = atet - 2*atet.sd,
                              ymax = atet + 2*atet.sd,
                              fill = over), alpha = 0.3) +
              guides(color = guide_legend(paste0(over.lab)), fill = guide_legend(paste0(over.ci.lab))) +
              theme_bw() +
              theme(legend.position = "right", text = element_text(size = text_size), axis.text.x = element_text(angle = myAngle, vjust = myVjust, hjust=myHjust))

          }



        }

        tymch <- list(plot.a = plot.a, plot.b = plot.b, data.a = d1a, data.b = d1b)
        class(tymch) <- c("didnpplot", "didnp")
        return(tymch)

      } else {

        # 2.2 TTa ----
        if(print.level >= 1) print("2.2 TTa")


        if (is.null(over)) {

          # 2.2.1 only 'by' ----
          if(print.level >= 1) print("2.2.1 only 'by'")

          atet <- atet.sd <- myCount <- numeric(n.by.levels)

          for(i in 1:n.by.levels){
            sele <- by == by.levels[i]
            myCount[i] <- sum(sele)
            atet[i] <- mean(obj$TTa.i[sele])
            atet.sd[i] <- sd(colMeans(obj$TTa.i.boot[sele,]))
          }

          d1a <-
            data.frame(
              atet = atet,
              atet.sd = atet.sd,
              count = myCount,
              by = by.levels
            )
          d1a <- d1a[complete.cases(d1a),]

          if(print.level >= 2) print(d1a)
          if(print.level >= 1)
            plot.a <- ggplot(d1a, aes(x = by, y = atet, group = 1)) +
            geom_ribbon(aes(ymin = atet - crit.value*atet.sd, ymax = atet + crit.value*atet.sd), alpha = 0.3) +
            geom_line(linewidth = line_width) +
            geom_point(size = point_size, shape = 16, color = "black") +
            labs(x = xlab, y = ylab) +
            theme_bw() +
            theme(legend.position = "none", text = element_text(size = text_size))

        } else {

          # 2.2.2 'by' + 'over' ----
          if(print.level >= 1) print("2.2.2 'by' + 'over'")

          # n.by.levels <-  n.levels
          atet <- atet.sd <- bY <- oY <- myCount <- numeric(n.by.levels*n.over.levels)
          by10 <- over10 <-  character(n.by.levels*n.over.levels)

          for (b in 1:n.by.levels) {
            for (o in 1:n.over.levels) {
              sele <- by == by.levels[b] & over == over.levels[o]
              # cat.print(table(sele))
              # cat.print(sum(sele))
              # cat.print(dim(obj$TTa.i.boot))
              # cat.print(dim(obj$TTa.i.boot[sele,]))
              index <- (b-1)*(n.over.levels) + o
              # print(c( b, o, 1111, index))
              myCount[index] <- sum(sele)

              if ( myCount[index] == 0 ) {
                atet[index] <- NA
                atet.sd[index] <- NA
              } else if ( myCount[index] == 1) {
                # cat.print(obj$TTa.i.boot[sele,])
                # cat.print(obj$TTa.i[sele])
                atet[index] <- obj$TTa.i[sele]
                atet.sd[index] <- sd(obj$TTa.i.boot[sele,])
              } else {
                atet[index] <- mean(obj$TTa.i[sele])
                atet.sd[index] <- sd(colMeans(obj$TTa.i.boot[sele,]))
              }
              by10[index] <- by.levels[b]
              over10[index] <- over.levels[o]
              bY[index] <- b
              oY[index] <- o
            }
          }

          d1a <-
            data.frame(
              atet = atet,
              atet.sd = atet.sd,
              count = myCount,
              by = by10,
              over = over10
            )
          # cat.print(d1a)
          d1a <- d1a[complete.cases(d1a),]

          if ( over.labels.values.supplied ) {

            colnames(over.labels.values) <- c("over", "over2")
            d1a <- merge.data.frame(d1a, over.labels.values)
            # figure indices
            which( colnames(d1a) == "over" ) -> over.index
            which( colnames(d1a) == "over2" ) -> over2.index
            # rename
            colnames(d1a)[over.index] <- "overold"
            colnames(d1a)[over2.index] <- "over"
            d1a <- d1a[, c("atet", "atet.sd", "count", "by", "over", "overold")]

          }

          if(print.level >= 2)  cat.print(d1a)

          plot.a <- ggplot(d1a, aes(x = by, y = atet, color = over, group = over)) +
            geom_line(linewidth = line_width) +
            geom_point(size = point_size, shape = 16, color = "black") +
            labs(x = xlab, y = ylab) +
            geom_ribbon(aes(ymin = atet - 2*atet.sd,
                            ymax = atet + 2*atet.sd,
                            fill = over), alpha = 0.3) +
            guides(color = guide_legend(paste0(over.lab)), fill = guide_legend(paste0(over.ci.lab))) +
            theme_bw() +
            theme(legend.position = "right", text = element_text(size = text_size))

        }

        tymch <- list(plot.a = plot.a, data.a = d1a)
        class(tymch) <- c("didnpplot", "didnp")
        return(tymch)


      }

    } else {
      stop("Inappropriate class of 'by'", call. = FALSE)
    }


  }



}
