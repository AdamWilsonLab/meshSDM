library(scales)
#library(testthat)

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
#test_that("asinh_breaks make sense", {
#  expect_equal(asinh_breaks(c(-0.05, 0, 1, 101)), c(0, 1, 10, 100))
#  expect_equal(asinh_breaks(c(-0.11, -0.05, 0, 1, 101)), c(-0.1, 0, 1, 10, 100))
#  expect_equal(asinh_breaks(c(0, 10, 1001)), c(0, 10, 100, 1000))
#  expect_equal(asinh_breaks(c(0, 0.05, 0.07, 0.1, 0.2)), c(0, 0.1))
#  expect_equal(asinh_breaks(c(0.01, 0.02)), c(0.01))
#})
asinh_trans <- function() {
  trans_new("asinh",
            transform = asinh,
            inverse   = sinh,
            breaks = asinh_breaks)
}
