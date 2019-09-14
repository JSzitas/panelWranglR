test_that("Correlation helper works", {
  set.seed(1071)
  X_1 <- rnorm(1000)
  X_2 <- rnorm(1000) + 0.6 * X_1
  X_3 <- rnorm(1000) - 0.4 * X_1

  data_fm <- do.call( cbind, list( X_1,
                                   X_2,
                                   X_3 ))

   test_corr_f <- corr_finder( df = data_fm,
                               corr_cutoff = 0.3 )



  expect_true(is.numeric(test_corr_f))
  expect_equal(test_corr_f, 1)
})
