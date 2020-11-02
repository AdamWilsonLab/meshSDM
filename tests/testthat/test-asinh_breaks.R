test_that("asinh_breaks make sense", {
 expect_equal(asinh_breaks(c(-0.05, 0, 1, 101)), c(0, 1, 10, 100))
 expect_equal(asinh_breaks(c(-0.11, -0.05, 0, 1, 101)), c(-0.1, 0, 1, 10, 100))
 expect_equal(asinh_breaks(c(0, 10, 1001)), c(0, 10, 100, 1000))
 expect_equal(asinh_breaks(c(0, 0.05, 0.07, 0.1, 0.2)), c(0, 0.1))
 expect_equal(asinh_breaks(c(0.01, 0.02)), c(0.01))
})
