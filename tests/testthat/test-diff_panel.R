test_that("Differencing a panel works", {

  X <- matrix(rnorm(800000),8000,100)
  tim <- seq(1:4000)
  geo_AT <- rep(c("AT"), length = 4000)
  geo_NO <- rep(c("NO"), length = 4000)
  both_vec_1 <- cbind(tim,geo_NO)
  both_vec_2 <- cbind(tim,geo_AT)
  both <- rbind(both_vec_1,both_vec_2)
  names(both[,"geo_NO"]) <- "geo"
  X <- cbind(both,X)


  diff_test_1 <- panel_diff(data = X,
                                cross.section = "geo_NO",
                                time.variable = "tim",
                                lags = 1)


 test_manual_lag <-  as.numeric(X[2,3]) - as.numeric(stats::lag(X[,3]))[1]

 expect_equal( unname(unlist(diff_test_1[2,3])), test_manual_lag )
 expect_equal( sum(is.na(diff_test_1)), 200)
 expect_equal( nrow(diff_test_1), 8000 )
 expect_equal (ncol(diff_test_1), 102 )


 diff_test_2 <- panel_diff(data = X,
                           cross.section = "geo_NO",
                           lags = 5)

 test_manual_lag_2 <-  as.numeric(X[6,3]) - as.numeric(stats::lag(X[,3],5))[1]

 expect_equal( unname(unlist(diff_test_2[6,3])), test_manual_lag_2 )
 expect_equal( sum(is.na(diff_test_2)), 1010)
 expect_equal( nrow(diff_test_2), 8000 )
 expect_equal (ncol(diff_test_2), 102 )


 diff_test_3 <- panel_diff(data = X,
                           cross.section = "geo_NO",
                           time.variable = "tim",
                           diff.order = 2,
                           lags = 1)

 test_manual_lag_3 <-  as.numeric(X[,3])[2:8000] - as.numeric(X[,3])[1:7999]
 test_manual_lag_3 <-  as.numeric(test_manual_lag_3)[2:7999] - as.numeric(test_manual_lag_3)[1:7998]


 expect_equal( unname(unlist(diff_test_3[3,3])), test_manual_lag_3[1] )
 expect_equal( sum(is.na(diff_test_3)), 400)
 expect_equal( nrow(diff_test_3), 8000 )
 expect_equal (ncol(diff_test_3), 102 )

 diff_test_4 <- panel_diff(data = X,
                           cross.section = "geo_NO",
                           time.variable = "tim",
                           diff.order = 4,
                           lags = 2)




 expect_equal( sum(is.na(diff_test_4)), 1600)
 expect_equal( nrow(diff_test_4), 8000 )
 expect_equal( ncol(diff_test_4), 102 )




})
