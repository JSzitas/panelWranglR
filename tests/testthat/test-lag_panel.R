test_that("Lagging a panel works", {


    X <- matrix(rnorm(800000),8000,100)
    tim <- seq(1:4000)
    geo_AT <- rep(c("AT"), length = 4000)
    geo_NO <- rep(c("NO"), length = 4000)
    both_vec_1 <- cbind(tim,geo_NO)
    both_vec_2 <- cbind(tim,geo_AT)
    both <- rbind(both_vec_1,both_vec_2)
    names(both[,"geo_NO"]) <- "geo"
    X <- cbind(both,X)

lagged_result_1 <- panel_lag(data = X,
                           cross.section = "geo_NO",
                           time.variable = "tim",
                           lags = 5,
                           keep.original = TRUE)

expect_equal(ncol(lagged_result_1), 602)
expect_equal(nrow(lagged_result_1), 8000)
expect_equal(sum(is.na(lagged_result_1)), 3000)
expect_equal(unname(unlist(lagged_result_1[2,103])), as.numeric(unlist(X[4001,3])))

lagged_result_2 <- panel_lag(data = X,
                             cross.section = "geo_NO",
                             lags = 5,
                             keep.original = FALSE)

expect_equal(ncol(lagged_result_2), 505)
expect_equal(nrow(lagged_result_2), 8000)
expect_equal(sum(is.na(lagged_result_2)), 3030)
expect_equal(unname(unlist(lagged_result_2[2,6])), as.numeric(unlist(X[4001,3])))

lagged_result_3 <- panel_lag(data = X,
                             cross.section = "geo_NO",
                             lags = 5,
                             variables.selected = c("tim","V5"),
                             keep.original = FALSE)

expect_equal(ncol(lagged_result_3), 10)
expect_equal(nrow(lagged_result_3), 8000)
expect_equal(sum(is.na(lagged_result_3)), 60)
expect_equal(unname(unlist(lagged_result_2[2,6])), as.numeric(unlist(X[4001,3])))



})
