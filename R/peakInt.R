############################################
#' @title peakInt
############################################

#' @name peakInt
#' @description peak integration
#' @param x (for peakInt) numeric vector, typically a time-series of
#' data; (for PeakInt methods) an object of class 'PeakInt'
#' @param peak.wd.asym peak property, peak width asymmetry
#' @param peak.ht.asym peak property, peak ht asymmetry
#' @param peak.cluster peak property, peak clustering
#' @param peak.cutoff peak property, peak cutoff
#' @param peak.cutoff.value peak property, peak cutoff value
#' @param peak.col (for plot.PeakInt) peak color
#' @param peak.border (for plot.PeakInt) peak border
#' @param data.col (for plot.PeakInt) data point color
#' @param report.lim (for plot.PeakInt) plot range limits
#' @param ... other arguments


##################
#this needs tidying,
#not sure where these are used
##################

#' @importFrom graphics axis legend lines locator polygon
#' @importFrom methods is
#' @importFrom stats lm median predict
#' @importFrom utils flush.console
#undefined globals
#might not need all these anymore...
utils::globalVariables(c("ref.hour.offset",
                         "peak.marker", "x"))

#peakInt
###########################
#kr 2020/07/28
#   old code from Heathrow project

#' @rdname peakInt
#' @export
peakInt <-
function (x, peak.wd.asym = 1000, peak.ht.asym = 1000,
    peak.cluster = 0, peak.cutoff = "none", peak.cutoff.value = 0,
    ...)
{
    #setup
#missing data handling
    ans <- peak_locate(x)
    ans <- peak_shape(ans, peak.wd.asym, peak.ht.asym)
    ans <- peak_base(ans, peak.cluster, peak.cutoff, peak.cutoff.value)
    class(ans) <- "PeakInt"
    out <- ans
}

#' @rdname peakInt
#' @export
#' @method plot PeakInt
plot.PeakInt <-
    function (x, report.lim = c(1, -1), peak.col = "grey",
              peak.border = NA, data.col = "black", ...)
    {
        peak.int <- x
        ifelse(report.lim[1] < 1, a <- 1, a <- report.lim[1])
        ifelse(report.lim[2] < 0, b <- nrow(peak.int$report), b <- report.lim[2])
        ifelse(b > nrow(peak.int$report), b <- nrow(peak.int$report),
               b <- b)
        y1 <- peak.int$report$start[a]
        y2 <- peak.int$report$end[b]
        x <- y1:y2
        y <- peak.int$output$data[y1:y2]
        plot(x, y, type = "l", col = peak.col, ...)
        if (length(peak.col) == 1) {
            peak.col = rep(peak.col, b - a + 1)
        }
        for (i in a:b) {
            x1 <- peak.int$report$start[i]
            x2 <- peak.int$report$end[i]
            polygon(c((x1:x2), (x2:x1)), c(peak.int$output[x1:x2,
                                                           1], (peak.int$output[x2:x1, (ncol(peak.int$output) -
                                                                                            1)])), col = peak.col[i - a + 1], border = peak.border)
        }
        lines(x, y, col = data.col)
    }

#' @rdname peakInt
#' @export
#' @method print PeakInt
print.PeakInt <- function(x, ...){
#very minimal at this stage
    cat("PeakInt report: ", nrow(x$report), " peaks...", sep="")
    invisible(x)
}


#unexported functions
#############################

#peak_locate
peak_locate <- function (data, ...)
{
    d2 <- c(diff(data), 0)
    d3 <- c(0, d2[1:length(d2) - 1])
    e <- ifelse(d2 < 0 & d3 >= 0, 0.4, 0) +
         ifelse(d2 > 0 & d3 <= 0, -0.1, 0)
    f <- rep(0, length(e))
    g <- -1
    for (i in 1:length(e)) {
        if (g < 0) {
            if (e[i] > 0) {
                f[i] = 0
            }
            else {
                f[i] = e[i]
            }
            if (e[i] < 0) {
                g <- 1
            }
        }
        else {
            if (e[i] < 0) {
                f[i] = 0
            }
            else {
                f[i] = e[i]
            }
            if (e[i] > 0) {
                g <- -1
            }
        }
    }
    out <- data.frame(data = data, peak.marker = f)
    x <- subset(out, peak.marker == 0.4)
    x1 <- as.numeric(row.names(x))
    y1 <- x$data
    x <- subset(out, peak.marker == -0.1)
    x2 <- as.numeric(row.names(x))
    y2 <- x$data
    x3 <- x2[2:length(x2)]
    y3 <- y2[2:length(y2)]
    x1 <- x1[1:min(length(x1), length(x2), length(x3))]
    x2 <- x2[1:min(length(x1), length(x2), length(x3))]
    x3 <- x3[1:min(length(x1), length(x2), length(x3))]
    y1 <- y1[1:min(length(y1), length(y2), length(y3))]
    y2 <- y2[1:min(length(y1), length(y2), length(y3))]
    y3 <- y3[1:min(length(y1), length(y2), length(y3))]
    xxx <- peak_assign(data, x2, x3, y2, y3)
    initial.baseline <- xxx$baseline
    initial.peak <- xxx$peak
    xxx <- peak_quant(initial.peak, x1, x2, x3)
    x4 <- xxx$peak.ht
    x5 <- xxx$peak.area
    x6 <- xxx$peak.wd
    output <- cbind(out, initial.baseline, initial.peak)
    report <- data.frame(top = x1, start = x2, end = x3, top.y = y1,
                         start.y = y2, end.y = y3, ht = x4, area = x5, wd = x6)
    out <- list(output = output, report = report)
    class(out) <- "PeakInt"
    invisible(out)
}


#peak_shape
peak_shape <- function (peak.record, peak.wd.asm, peak.ht.asm, ...)
{
    output <- peak.record$output
    report <- peak.record$report
    shape.log <- rep("", length(report$top))
    for (i in 1:length(report$top)) {
        if ((report$top[i] - report$start[i]) > (1 + ((peak.wd.asm/100))) *
            (report$end[i] - report$top[i])) {
            report$start[i] <- round(report$top[i] - (1 + ((peak.wd.asm/100))) *
                                         (report$end[i] - report$top[i]))
            shape.log[i] <- paste(shape.log[i], "1fs", sep = "")
            report$start.y[i] <- output$data[round(report$start[i])]
        }
        else {
            if ((report$end[i] - report$top[i]) > (1 + ((peak.wd.asm/100))) *
                (report$top[i] - report$start[i])) {
                report$end[i] <- round(report$top[i] + (1 +
                                                            ((peak.wd.asm/100))) * (report$top[i] - report$start[i]))
                shape.log[i] <- paste(shape.log[i], "1fe", sep = "")
                report$end.y[i] <- output$data[round(report$end[i])]
            }
            else {
                shape.log[i] <- paste(shape.log[i], "1nn", sep = "")
            }
        }
    }
    for (i in 1:length(report$top)) {
        if ((report$top.y[i] - report$start.y[i]) < (1 - ((peak.ht.asm/100))) *
            (report$top.y[i] - report$end.y[i])) {
            report$start.y[i] <- round(report$top.y[i] - (1 -
                                                              ((peak.ht.asm/100))) * (report$top.y[i] - report$end.y[i]))
            shape.log[i] <- paste(shape.log[i], "2ds", sep = "")
        }
        else {
            if ((report$top.y[i] - report$end.y[i]) < (1 - ((peak.ht.asm/100))) *
                (report$top.y[i] - report$start.y[i])) {
                report$end.y[i] <- round(report$top.y[i] - (1 -
                                                                ((peak.ht.asm/100))) * (report$top.y[i] -
                                                                                            report$start.y[i]))
                shape.log[i] <- paste(shape.log[i], "2de", sep = "")
            }
            else {
                shape.log[i] <- paste(shape.log[i], "2nn", sep = "")
            }
        }
    }
    xxx <- peak_assign(output$data, report$start, report$end,
                         report$start.y, report$end.y)
    shape.baseline <- xxx$baseline
    shape.peak <- xxx$peak
    xxx <- peak_quant(shape.peak, report$top, report$start,
                        report$end)
    report$ht[1:length(report$ht)] <- xxx$peak.ht
    report$area[1:length(report$area)] <- xxx$peak.area
    report$wd[1:length(report$wd)] <- xxx$peak.wd
    output <- cbind(output, shape.baseline, shape.peak)
    report <- cbind(report, shape.log)
    out <- list(output = output, report = report)
    class(out) <- "PeakInt"
    out <- out
}


#peak_base
peak_base <- function (peak.record, peak.cluster = 0,
                       peak.cutoff = "none",
                       peak.cutoff.value = 0, ...)
{
    output <- peak.record$output
    report <- peak.record$report
    base.log <- rep("1", length(report$top))
    base.log[1] <- paste(base.log[1], "n", sep = "")
    for (i in length(report$top):2) {
        if ((report$top[i] - report$top[i - 1]) < peak.cluster) {
            report$start.y[i] <- min(report$start.y[i], report$end.y[i],
                                     report$start.y[i - 1], report$end.y[i - 1])
            report$end.y[i] <- report$start.y[i]
            report$start.y[i - 1] <- report$start.y[i]
            report$end.y[i - 1] <- report$start.y[i]
            base.log[i] <- paste(base.log[i], "c", sep = "")
        }
        else {
            base.log[i] <- paste(base.log[i], "n", sep = "")
        }
    }
    for (i in 1:(length(report$top) - 1)) {
        if ((report$top[i + 1] - report$top[i]) < peak.cluster) {
            report$end.y[i] <- min(report$start.y[i], report$end.y[i],
                                   report$start.y[i + 1], report$end.y[i + 1])
            report$end.y[i] <- report$start.y[i]
            report$start.y[i + 1] <- report$start.y[i]
            report$end.y[i + 1] <- report$start.y[i]
            base.log[i] <- paste(base.log[i], "c", sep = "")
        }
        else {
            base.log[i] <- paste(base.log[i], "n", sep = "")
        }
    }
    base.log[length(base.log)] <- paste(base.log[length(base.log)],
                                        "n", sep = "")
    if (peak.cutoff == "none") {
    }
    else {
        if (peak.cutoff == "median") {
            xxx <- median(output[, (ncol(output) - 1)])
            report$start.y <- ifelse(report$start.y > xxx, xxx,
                                     report$start.y)
            report$end.y <- ifelse(report$end.y > xxx, xxx,
                                   report$end.y)
            base.log <- paste(base.log, "2median", sep = "")
        }
        if (peak.cutoff == "fixed") {
            report$start.y <- ifelse(report$start.y > peak.cutoff.value,
                                     peak.cutoff.value, report$start.y)
            report$end.y <- ifelse(report$end.y > peak.cutoff.value,
                                   peak.cutoff.value, report$end.y)
            base.log <- paste(base.log, "2fixed(", peak.cutoff.value,
                              ")", sep = "")
        }
    }
    xxx <- peak_assign(output$data, report$start, report$end,
                         report$start.y, report$end.y)
    base.baseline <- xxx$baseline
    base.peak <- xxx$peak
    xxx <- peak_quant(base.peak, report$top, report$start,
                        report$end)
    report$ht[1:length(report$ht)] <- xxx$peak.ht
    report$area[1:length(report$area)] <- xxx$peak.area
    report$wd[1:length(report$wd)] <- xxx$peak.wd
    output <- cbind(output, base.baseline, base.peak)
    report <- cbind(report, base.log)
    out <- list(output = output, report = report)
    class(out) <- "PeakInt"
    out <- out
}

#peak_assign
peak_assign <- function (data, x2, x3, y2, y3)
{
    baseline = data
    for (i in 1:length(x2)) {
        for (j in x2[i]:x3[i]) {
            baseline[j] = (((x3[i] - j)/(x3[i] - x2[i])) * y2[i] +
                               ((j - x2[i])/(x3[i] - x2[i])) * y3[i])
            if (baseline[j] > data[j]) {
                baseline[j] = data[j]
            }
        }
    }
    peak <- data - baseline
    out <- data.frame(baseline = baseline, peak = peak)
}

#old peak plotting
############################



#plot.peak.int <-
#    function (peak.int, report.lim = c(1, -1), peak.col = "grey",
#              peak.border = NA, data.col = "black", ...)
#    {
#        ifelse(report.lim[1] < 1, a <- 1, a <- report.lim[1])
#        ifelse(report.lim[2] < 0, b <- nrow(peak.int$report), b <- report.lim[2])
#        ifelse(b > nrow(peak.int$report), b <- nrow(peak.int$report),
#               b <- b)
#        y1 <- peak.int$report$start[a]
#        y2 <- peak.int$report$end[b]
#        x <- y1:y2
#        y <- peak.int$output$data[y1:y2]
#        plot(x, y, type = "l", col = peak.col, ...)
#        if (length(peak.col) == 1) {
#            peak.col = rep(peak.col, b - a + 1)
#        }
#        for (i in a:b) {
#            x1 <- peak.int$report$start[i]
#            x2 <- peak.int$report$end[i]
#            polygon(c((x1:x2), (x2:x1)),
#                    c(peak.int$output[x1:x2,1],
#                      (peak.int$output[x2:x1,
#                    (ncol(peak.int$output) - 1)])), col = peak.col[i - a + 1], border = peak.border)
#        }
#        lines(x, y, col = data.col)
#    }


view <- function (a = 2200, b = 2400, peak.int = x, ...)
{
    t <- peak.int$output
    plot(t$data[a:b], type = "l", ylim = c(0, max(t$data[a:b])),
         ...)
    lines(t$initial.baseline[a:b], col = "red", ...)
    lines(t$shape.baseline[a:b], col = "blue")
    lines(t$base.baseline[a:b], col = "green")
}

view.new <- function (peak.int, report.lim = c(1, -1), peak.col = "grey",
                      peak.border = NA, data.col = "black", ...)
{
    ifelse(report.lim[1] < 1, a <- 1, a <- report.lim[1])
    ifelse(report.lim[2] < 0, b <- nrow(peak.int$report), b <- report.lim[2])
    ifelse(b > nrow(peak.int$report), b <- nrow(peak.int$report),
           b <- b)
    y1 <- peak.int$report$start[a]
    y2 <- peak.int$report$end[b]
    x <- y1:y2
    y <- peak.int$output$data[y1:y2]
    plot(x, y, type = "l", col = peak.col, ...)
    if (length(peak.col) == 1) {
        peak.col = rep(peak.col, b - a + 1)
    }
    for (i in a:b) {
        x1 <- peak.int$report$start[i]
        x2 <- peak.int$report$end[i]
        polygon(c((x1:x2), (x2:x1)), c(peak.int$output[x1:x2,
                                                       1], (peak.int$output[x2:x1, (ncol(peak.int$output) -
                                                                                        1)])), col = peak.col[i - a + 1], border = peak.border)
    }
    lines(x, y, col = data.col)
}

view.peak <- function (peak.int, a = 1, range = 10, peak.col = "red", peak.border = "black",
                       other.peak.col = "lightgrey", label.offset = c(0, 0), ...)
{
    x1 <- peak.int$report$start[a]
    x2 <- peak.int$report$end[a]
    ifelse(min(a) - range < 1, a1 <- 1, a1 <- min(a) - range)
    ifelse(max(a) + range > nrow(peak.int$report), a2 <- nrow(peak.int$report),
           a2 <- max(a) + range)
    a1 <- peak.int$report$start[a1]
    a2 <- peak.int$report$end[a2]
    x <- a1:a2
    y <- peak.int$output$data[a1:a2]
    plot(x, y, type = "n", axes = FALSE, ...)
    axis(1)
    axis(2)
    polygon(c((a1:a2), (a2:a1)), c(peak.int$output[a1:a2, 1],
                                   (peak.int$output[a2:a1, (ncol(peak.int$output) - 1)])),
            col = other.peak.col, border = NA)
    lines(x, y, col = other.peak.col)
    for (i in 1:length(a)) {
        polygon(c((x1[i]:x2[i]), (x2[i]:x1[i])), c(peak.int$output[x1[i]:x2[i],
                                                                   1], (peak.int$output[x2[i]:x1[i], (ncol(peak.int$output) -
                                                                                                          1)])), col = peak.col, border = peak.border)
    }
    for (i in 1:length(a)) {
        legend((peak.int$report$top[a[i]] + label.offset[1]),
               (peak.int$report$top.y[a[i]] + label.offset[2]),
               c(paste("PEAK: ", a[i], sep = ""), paste("AREA: ",
                                                        signif(peak.int$report$area[a[i]], digits = 6),
                                                        sep = ""), paste("HT:       ", signif(peak.int$report$ht[a[i]],
                                                                                              digits = 6), sep = ""), paste("WD:     ", signif(peak.int$report$wd[a[i]],
                                                                                                                                               digits = 6), sep = "")), bty = "n")
    }
}

#old
###########################

#peak_quant
peak_quant <- function (peak, x1, x2, x3)
{
    x4 <- rep(0, length(x1))
    x5 <- x4
    x6 <- x4
    for (i in 1:length(x1)) {
        x4[i] <- peak[x1[i]]
        xx <- peak[x2[i]:x3[i]]
        x5[i] <- sum(subset(xx, xx > 0))
        x6[i] <- x3[i] - x2[i]
    }
    out <- data.frame(peak.ht = x4, peak.area = x5, peak.wd = x6)
}

#old peak code
old_peak_code <- function (data, ...)
{
    d2 <- c(diff(data), 0)
    d3 <- c(0, d2[1:length(d2) - 1])
    e <- ifelse(d2 < 0 & d3 >= 0, 0.4, 0) + ifelse(d2 > 0 &
                                                       d3 <= 0, -0.1, 0)
    f <- rep(0, length(e))
    g <- -1
    for (i in 1:length(e)) {
        if (g < 0) {
            if (e[i] > 0) {
                f[i] = 0
            }
            else {
                f[i] = e[i]
            }
            if (e[i] < 0) {
                g <- 1
            }
        }
        else {
            if (e[i] < 0) {
                f[i] = 0
            }
            else {
                f[i] = e[i]
            }
            if (e[i] > 0) {
                g <- -1
            }
        }
    }
    out <- data.frame(data = data, peak.marker = f)
    x <- subset(out, peak.marker == 0.4)
    x1 <- as.numeric(row.names(x))
    x <- subset(out, peak.marker == -0.1)
    x2 <- as.numeric(row.names(x))
    x3 <- x2[2:length(x2)]
    x1 <- x1[1:min(length(x1), length(x2), length(x3))]
    x2 <- x2[1:min(length(x1), length(x2), length(x3))]
    x2 <- x2[1:min(length(x1), length(x2), length(x3))]
    report <- data.frame(top = x1, start = x2, end = x3)
    initial.baseline <- data
    for (i in 1:length(x2)) {
        for (j in x2[i]:x3[i]) {
            initial.baseline[j] = (((x3[i] - j)/(x3[i] - x2[i])) *
                                       data[x2[i]] + ((j - x2[i])/(x3[i] - x2[i])) *
                                       data[x3[i]])
            if (initial.baseline[j] > data[j]) {
                initial.baseline[j] = data[j]
            }
        }
    }
    initial.peak <- data - initial.baseline
    x4 <- rep(0, length(x1))
    x5 <- x4
    for (i in 1:length(x1)) {
        x4[i] <- initial.peak[x1[i]]
        xx <- initial.peak[x2[i]:x3[i]]
        x5[i] <- sum(subset(xx, xx > 0))
    }
    report <- cbind(report, initial.ht = x4, initial.area = x5)
    out <- cbind(out, initial.baseline, initial.peak)
    out <- list(output = out, report = report)
}


