##' Inverse Hyperbolic Sine formatter for ggplot.
##' From http://wresch.github.io/2013/03/08/asinh-scales-in-ggplot2.html
##' @title Inverse Hyperbolic Sine formatter
##' @param x Vector
##' @return Vector
##' @author Wolfgang Resch
##' @export
asinh_breaks <- function(x) {
  br <- function(r) {
    lmin <- round(log10(r[1]))
    lmax <- round(log10(r[2]))
    lbreaks <- seq(lmin, lmax, by = 1)
    breaks <- 10 ^ lbreaks
  }
  p.rng <- range(x[x > 0], na.rm = TRUE)
  breaks <- br(p.rng)
  if (min(x) <= 0) {breaks <- c(0, breaks)}
  if (sum(x < 0) > 1) { #more negative values that expected from expanding scale that includes zero
    n.rng <- -range(x[x < 0], na.rm = TRUE)
    breaks <- c(breaks, -br(n.rng))
  }
  return(sort(breaks))
}
test_that("asinh_breaks make sense", {
  expect_equal(asinh_breaks(c(-0.05, 0, 1, 101)), c(0, 1, 10, 100))
  expect_equal(asinh_breaks(c(-0.11, -0.05, 0, 1, 101)), c(-0.1, 0, 1, 10, 100))
  expect_equal(asinh_breaks(c(0, 10, 1001)), c(0, 10, 100, 1000))
  expect_equal(asinh_breaks(c(0, 0.05, 0.07, 0.1, 0.2)), c(0, 0.1))
  expect_equal(asinh_breaks(c(0.01, 0.02)), c(0.01))
})

##' Inverse Hyperbolic Sine transformer for ggplot.
##' From http://wresch.github.io/2013/03/08/asinh-scales-in-ggplot2.html
##' @title Inverse Hyperbolic Sine transformer
##' @param x Vector
##' @return Vector
##' @author Wolfgan Resch
##' @export
asinh_trans <- function() {
  trans_new("asinh",
            transform = asinh,
            inverse   = sinh)
}
