

##' Run a correspondence analysis on the model data and return it and
##' a list of colors for the labels. Uses both main and supp vars in the
##' analysis. (i.e. combines them so they are all mainvars.)
##'
##' Simple wrapper for multiple use. Requires Friendly et al's ca package.
##' @title run.ca
##' @param data Data frame
##' @param mainvars Character vector of Main Variables
##' @param suppvars Character vector of Supplementary Variables
##' @param ... Other arguments to ca()
##' @return ca object
##' @author Kieran Healy
##' @export
run.ca <- function(data, mainvars, suppvars, ...){
  out <- data[,c(mainvars, suppvars)]
  out.ca <- mjca(out, lambda="adjusted")
  out.vnames <- out.ca$levelnames
  out.vnames.ed <- clean.calabels(out.vnames)
  out.ca$levelnames <- out.vnames.ed
  nlevels.suppvars <- sum(sapply(data[,suppvars], len.lev))
  nlevels.mainvars <- sum(sapply(data[,mainvars], len.lev))
  label.colors <- c(rep(my.colors()[6], nlevels.mainvars), rep(my.colors()[7],
                                                            nlevels.suppvars))
  out.list <- list(model=out.ca, colors=label.colors)
  return(out.list)
}



##' Run a correspondence analysis on the model data using suppvars as supplementary
##'
##' Simple wrapper for multiple use. Requires Friendly et al's ca package.
##' @title run.supp.ca
##' @param data Character vector of Main Variables
##' @param mainvars Main variables
##' @param suppvars Supplementary variables
##' @param ... Other arguments to ca()
##' @return ca object
##' @author Kieran Healy
##' @export
run.supp.ca <- function(data, mainvars, suppvars, ...){
  all.vars <- c(mainvars, suppvars)
  supcol.start <- length(mainvars)+1
  supcol.end <- length(all.vars)

  out <- data[,c(mainvars, suppvars)]
  out.ca <- mjca(out, supcol=c(supcol.start:supcol.end), lambda="adjusted")
  out.vnames <- out.ca$levelnames
  out.vnames.ed <- clean.calabels(out.vnames)
  out.ca$levelnames <- out.vnames.ed
  nlevels.suppvars <- sum(sapply(data[,suppvars], len.lev))
  nlevels.mainvars <- sum(sapply(data[,mainvars], len.lev))
  label.colors <- c(rep(my.colors()[6], nlevels.mainvars), rep(my.colors()[7],
                                                            nlevels.suppvars))
  out.list <- list(model=out.ca, colors=label.colors)
  return(out.list)
  ## return(out.ca)
}



##' Clean ca clabels
##'
##' tiding up labels from the fin survey
##' @title Clean ca labels
##' @param vnames vector of label names
##' @return Scrubbed labels for ca plot
##' @author Kieran Healy
##' @export
clean.calabels <- function(vnames){
  vnames.ed <- sub("_", ":", vnames)
  vnames.ed <- sub("Gender", "Gender:\\\n", vnames.ed)
  vnames.ed <- sub("Age", "Age:", vnames.ed)
  vnames.ed <- sub("Interest\\.Q\\.ans", "FinLitQ1:\\\n", vnames.ed)
  vnames.ed <- sub("Inflation\\.Q\\.ans", "FinLitQ2:\\\n", vnames.ed)
  vnames.ed <- sub("Mortgage\\.Q\\.ans", "FinLitQ3:\\\n", vnames.ed)
  vnames.ed <- sub("Tax\\.Refund\\.Advance\\.5yr", "TRAL",
                   vnames.ed)
  vnames.ed <- sub("Payday\\.Loan\\.5yr", "Payday Loan", vnames.ed)
  vnames.ed <- sub("RTO\\.5yr", "RTO", vnames.ed)
  vnames.ed <- sub("Education", "Education:\\\n", vnames.ed)
  vnames.ed <- sub("Ethnicity:", "Ethnicity:\\\n", vnames.ed)
  vnames.ed <- sub("Marital", "Marital:\\\n", vnames.ed)
  vnames.ed <- sub("Employment\\.Spouse", "Spouse:", vnames.ed)
  vnames.ed <- sub("Employment\\.Self", "", vnames.ed)
  vnames.ed <- sub("Check\\.Cashing\\.Outlet", "CCO", vnames.ed)
  vnames.ed <- sub("Check\\.Cashing\\.Grocery", "CCG", vnames.ed)
  vnames.ed <- sub("Mortgage\\.Late", "Mortgage Late:\\\n", vnames.ed)
  vnames.ed <- sub("Fixed\\.Adjustable", "Mortgage Type:\\\n",
                 vnames.ed)
  vnames.ed <- sub("Adjustable rate mortgage", "Adjustable",
                 vnames.ed)
  vnames.ed <- sub("Interest\\.OnlyYes - Interest only mortgage or interest-only option",
                 "IOARMYes", vnames.ed)
  vnames.ed <- sub("Interest\\.OnlyNo", "IOARMNo", vnames.ed)
  vnames.ed <- sub("No", ":No", vnames.ed)
  vnames.ed <- sub("Yes", ":Yes", vnames.ed)
  vnames.ed <- sub("Income4", "Income\\\n", vnames.ed)
  vnames.ed <- sub("Kids", "Kids:", vnames.ed)
  vnames.ed <- sub("Cards", "Cards:", vnames.ed)
  vnames.ed <- sub("CC\\.", "", vnames.ed)
  vnames.ed <- sub("Credit\\.Score", "Credit Score:\\\n", vnames.ed)
  vnames.ed <- sub("Health\\.Insurance", "Health\\\nInsurance",
                   vnames.ed)
  vnames.ed <- sub("Investment\\.Advice", "Investment\\\nAdvice",
                   vnames.ed)
  vnames.ed <- sub("Tax\\.Planning", "Tax\\\nPlanning",
                   vnames.ed)
  vnames.ed <- sub("\\.YND_", "", vnames.ed)
  vnames.ed <- sub("\\.YN_", "", vnames.ed)
  vnames.ed <- sub("\\.YND", "", vnames.ed)
  vnames.ed <- sub("\\.YN", "", vnames.ed)
  vnames.ed <- gsub(":>", ">", vnames.ed)
  vnames.ed <- gsub(":<", "<", vnames.ed)
  vnames.ed <- gsub(":\\\n:", ":\\\n", vnames.ed)
  vnames.ed <- gsub("^:", "", vnames.ed)
  vnames.ed <- gsub(":\\$", "\\$", vnames.ed)
  vnames.ed <- gsub("\\.", " ", vnames.ed)
  vnames.ed <- gsub("::", ":", vnames.ed)
  return(vnames.ed)
}


##' Count the number of levels in a factor
##'
##' Useful for plotting color ca vals
##' @title len.lev
##' @param x a factor
##' @return the number of levels in \code{x}
##' @author Kieran Healy
##' @export
len.lev <- function(x){
  o <- length(as.vector(levels(x)))
  return(o)
}


##' Count the number of levels in a factor
##'
##' Count factor levels
##' @title len.lev2
##' @param x a factor
##' @return the number of levels in \code{x}
##' @author Kieran Healy
##' @export
len.lev2 <- function(x){
  o <- nlevels(as.factor(x))
  return(o)
}




##' Tweak the plot.ca function to handle a vector of colors for the labels
##'
##' Written by Michael Greenacre, Oleg Nenadic, and Michael Friendly, and trivially modified by Kieran Healy.
##' @title colorca.plot
##' @param x ca object. See plot.mjca for details on this and other arguments below
##' @param dim See plot.mjca
##' @param map See plot.mjca
##' @param centroids See plot.mjca
##' @param what See plot.mjca
##' @param mass See plot.mjca
##' @param contrib See plot.mjca
##' @param col Vector of colors
##' @param pch See plot.mjca
##' @param labels See plot.mjca
##' @param arrows See plot.mjca
##' @param labcols Vector of colors for the labels (main and supplementary)
##' @param txtsize See plot.mjca
##' @param ... See plot.mjca
##' @return an mjca plot
##' @author Kieran Healy
##' @export
colorca.plot <- function(x, dim = c(1, 2), map = "symmetric", centroids = FALSE,
    what = c("none", "all"), mass = c(FALSE, FALSE), contrib = c("none",
        "none"), col = c("#000000", "#FF0000"), pch = c(16, 1,
        17, 24), labels = c(2, 2), arrows = c(FALSE, FALSE),
                          labcols=NULL, txtsize=0.75, ...)
{
    require(ca)
    obj <- x
    if (what[1] != "none")
        what[1] <- "none"
    if (length(what) != 2)
        what <- rep(what, length = 2)
    if (length(mass) != 2)
        mass <- rep(mass, length = 2)
    if (length(contrib) != 2)
        contrib <- rep(contrib, length = 2)
    if (length(labels) != 2)
        labels <- rep(labels, length = 2)
    if (length(pch) != 4)
        pch <- rep(pch, length = 4)
    col.temp <- length(obj$colnames)
    if (length(col) < col.temp + 1)
        col <- c(col[1], rep(col[-1], length = col.temp))
    col <- col[1:(1 + col.temp)]
    if (!is.numeric(x$suprow)) {
        if (map == "colgab" | map == "colgreen") {
            if (what[1] != "none")
                what[1] <- "active"
        }
    }
    if (!is.numeric(x$supcol)) {
        if (map == "rowgab" | map == "rowgreen") {
            if (what[2] != "none")
                what[2] <- "active"
        }
    }
    K <- dim(obj$rowcoord)[2]
    I <- dim(obj$rowcoord)[1]
    J <- dim(obj$colcoord)[1]
    evF <- matrix(rep(obj$sv[1:K], I), I, K, byrow = TRUE)
    evG <- matrix(rep(obj$sv[1:K], J), J, K, byrow = TRUE)
    rpc <- obj$rowcoord * evF
    cpc <- obj$colcoord * evG
    symrpc <- obj$rowcoord * sqrt(evF)
    symcpc <- obj$colcoord * sqrt(evG)
    mt <- c("symmetric", "rowprincipal", "colprincipal", "symbiplot",
        "rowgab", "colgab", "rowgreen", "colgreen")
    mti <- 1:length(mt)
    mtlut <- list(symmetric = list(x = rpc, y = cpc), rowprincipal = list(x = rpc,
        y = obj$colcoord), colprincipal = list(x = obj$rowcoord,
        y = cpc), symbiplot = list(x = symrpc, y = symcpc), rowgab = list(x = rpc,
        y = obj$colcoord * obj$colmass), colgab = list(x = obj$rowcoord *
        obj$rowmass, y = cpc), rowgreen = list(x = rpc, y = obj$colcoord *
        sqrt(obj$colmass)), rowgreen = list(x = obj$rowcoord *
        sqrt(obj$rowmass), y = cpc))
    x <- mtlut[[mti[mt == map]]][[1]]
    y <- mtlut[[mti[mt == map]]][[2]]
    x.names <- obj$rownames
    y.names <- obj$levelnames
    rm(mt, mti, mtlut)
    indx <- dim(x)[1]
    indy <- dim(y)[1]
    pch.x <- rep(pch[1], dim(x)[1])
    pch.y <- rep(pch[3], dim(y)[1])
    pr <- c("none", "active", "passive", "all")
    pri <- 1:4
    sup.x <- NA
    act.x <- x
    xn.sup <- NA
    xn.act <- x.names
    if (is.na(obj$colsup[1])) {
        sup.y <- NA
        act.y <- y
        yn.sup <- NA
        yn.act <- y.names
    }
    else {
        sup.y <- y[obj$colsup, ]
        act.y <- y[-obj$colsup, ]
        pch.y[obj$colsup] <- pch[4]
        yn.sup <- y.names[obj$colsup]
        yn.act <- y.names[-obj$colsup]
    }
    prlut <- list(none = list(x = NA, y = NA), active = list(x = act.x,
        y = act.y), supplementary = list(x = sup.x, y = sup.y),
        all = list(x = x, y = y))
    nameslut <- list(none = list(x.names = NA, y.names = NA),
        active = list(x.names = xn.act, y.names = yn.act), supplementary = list(x.names = xn.sup,
            y.names = yn.sup), all = list(x.names = x.names,
            y.names = y.names))
    pchlut <- list(none = list(x.pch = NA, y.pch = NA), active = list(x.pch = rep(pch[1],
        dim(x)[1]), y.pch = rep(pch[3], dim(y)[1])), supplementary = list(x.pch = rep(pch[2],
        dim(x)[1]), y.pch = rep(pch[4], dim(y)[1])), all = list(x.pch = pch.x,
        y.pch = pch.y))
    x <- prlut[[pri[pr == what[1]]]][[1]]
    y <- prlut[[pri[pr == what[2]]]][[2]]
    x.names <- nameslut[[pri[pr == what[1]]]][[1]]
    y.names <- nameslut[[pri[pr == what[2]]]][[2]]
    x.pch <- pchlut[[pri[pr == what[1]]]][[1]]
    y.pch <- pchlut[[pri[pr == what[2]]]][[2]]
    if (is.matrix(x)) {
        x <- x[, dim]
    }
    else {
        x <- matrix(x[dim], ncol = length(dim), nrow = 1)
    }
    if (is.matrix(y)) {
        y <- y[, dim]
    }
    else {
        y <- matrix(y[dim], ncol = length(dim), nrow = 1)
    }
    if (mass[1])
        cex.x <- 0.5 + obj$rowmass^(1/3)/max(obj$rowmass^(1/3))
    else cex.x <- 0.5
    if (mass[2])
        cex.y <- 0.5 + obj$colmass^(1/3)/max(obj$colmass^(1/3))
    else cex.y <- 1
    nc0 <- 50
    cst <- 230
    col.x <- col[1]
    col.y <- rep(col[-1], obj$levels.n)
    if (contrib[1] == "relative") {
        cind <- obj$rowmass * (rpc[, dim[1]]^2 + rpc[, dim[2]]^2)/obj$rowinertia
        cb.x <- col2rgb(col[1])
        collut.x <- rgb(seq(cst, cb.x[1, 1], length = nc0), seq(cst,
            cb.x[2, 1], length = nc0), seq(cst, cb.x[3, 1], length = nc0),
            maxColorValue = 255)
        xtemp <- nc0 * (cind)
        col.x <- collut.x[xtemp]
    }
    else if (contrib[1] == "absolute") {
        cind <- obj$rowmass * (rpc[, dim[1]]^2 + rpc[, dim[2]]^2)/(obj$sv[dim[1]]^2 +
            obj$sv[dim[2]]^2)
        cb.x <- col2rgb(col[1])
        p.x <- cb.x[, 1] + (cst - cb.x[, 1])/indx
        collut.x1 <- rgb(seq(cst, p.x[1], length = nc0/2), seq(cst,
            p.x[2], length = nc0/2), seq(cst, p.x[3], length = nc0/2),
            maxColorValue = 255)
        collut.x2 <- rgb(seq(p.x[1], cb.x[1, 1], length = nc0/2),
            seq(p.x[2], cb.x[2, 1], length = nc0/2), seq(p.x[3],
                cb.x[3, 1], length = nc0/2), maxColorValue = 255)
        collut.x <- c(collut.x1, collut.x2)
        xtemp <- nc0 * (cind)
        col.x <- collut.x[xtemp]
    }
    if (contrib[2] == "relative") {
        cind <- obj$colmass * (cpc[, dim[1]]^2 + cpc[, dim[2]]^2)/obj$colinertia
        cb.y <- col2rgb(col[2])
        collut.y <- rgb(seq(cst, cb.y[1, 1], length = nc0), seq(cst,
            cb.y[2, 1], length = nc0), seq(cst, cb.y[3, 1], length = nc0),
            maxColorValue = 255)
        ytemp <- nc0 * cind
        col.y <- collut.y[ytemp]
    }
    if (contrib[2] == "absolute") {
        cind <- obj$colmass * (cpc[, dim[1]]^2 + cpc[, dim[2]]^2)/(obj$sv[dim[1]]^2 +
            obj$sv[dim[2]]^2)
        cb.y <- col2rgb(col[2])
        p.y <- cb.y[, 1] + (cst - cb.y[, 1])/indy
        collut.y1 <- rgb(seq(cst, p.y[1], length = nc0/2), seq(cst,
            p.y[2], length = nc0/2), seq(cst, p.y[3], length = nc0/2),
            maxColorValue = 255)
        collut.y2 <- rgb(seq(p.y[1], cb.y[1, 1], length = nc0/2),
            seq(p.y[2], cb.y[2, 1], length = nc0/2), seq(p.y[3],
                cb.y[3, 1], length = nc0/2), maxColorValue = 255)
        collut.y <- c(collut.y1, collut.y2)
        ytemp <- nc0 * cind
        col.y <- collut.y[ytemp]
    }
    q1 <- (1:dim(x)[1])
    q2 <- (1:dim(y)[1])
    l1 <- c(x[q1, 1], y[q2, 1])
    l1 <- l1[!is.na(l1)]
    l2 <- c(x[q1, 2], y[q2, 2])
    l2 <- l2[!is.na(l2)]
    if (length(l1) == 0)
        l1 <- c(-0.1, 0.1)
    if (length(l2) == 0)
        l2 <- c(-0.1, 0.1)
    lim1 <- range(l1) + c(-0.05, 0.05) * diff(range(l1))
    lim2 <- range(l2) + c(-0.05, 0.05) * diff(range(l2))
    pty.backup <- par()$pty
    plot(c(x[, 1], y[, 1]), c(x[, 2], y[, 2]), xlab = "", ylab = "",
        type = "n", axes = FALSE, asp = 1, ...)
    box()
    abline(h = 0, v = 0, lty = 3)
    axis(1, col = col[1])
    axis(2, col = col[1])
    if (!is.na(x[1]) & labels[1] != 1) {
        if (arrows[1]) {
            arrows(rep(0, length(x[, 1])), rep(0, length(x[,
                1])), x[, 1], x[, 2], col = col.x, length = 0.1)
        }
        else {
            points(x[, 1], x[, 2], cex = cex.x, col = col.x,
                pch = x.pch)
        }
    }
    if (labels[1] > 0) {
        xoff1 <- 0.5 * strwidth(x.names, cex = txtsize) + 0.5 *
            strwidth("o", cex = txtsize)
        xoff2 <- 0.5 * strheight(x.names, cex = txtsize) + 0.5 *
            strheight("o", cex = txtsize)
        text(x[, 1] + xoff1, x[, 2] + xoff2, x.names, cex = txtsize,
            xpd = TRUE, col=labcols)
    }
    if (!is.na(y[1]) & labels[2] != 1) {
        if (arrows[2]) {
            arrows(rep(0, length(y[, 1])), rep(0, length(y[,
                1])), y[, 1], y[, 2], col = col.y, length = 0.1)
        }
        else {
            points(y[, 1], y[, 2], cex = cex.y, col = col.y,
                pch = y.pch)
        }
    }
    if (labels[2] > 0) {
        yoff1 <- 0.5 * strwidth(y.names, cex = txtsize) + 0.5 *
            strwidth("o", cex = txtsize)
        yoff2 <- 0.5 * strheight(y.names, cex = txtsize) + 0.5 *
            strheight("o", cex = txtsize)
        text(y[, 1] + yoff1, y[, 2] + yoff2, y.names, cex = txtsize,
            xpd = TRUE, col=labcols)
    }
    par(pty = pty.backup)
}
