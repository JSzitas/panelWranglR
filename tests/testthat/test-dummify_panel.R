test_that("Panel dummies work", {

  set.seed(1071)
  x_1 <- rnorm( 10000 )
  x_2 <- rnorm( 10000 ) + 0.5 * x_1
  x_3 <- rnorm( 10000 ) + 0.1 * x_1
  x_4 <- rnorm( 10000 ) - 0.7 * x_3
  x_5 <- rnorm( 10000 ) + 0.5 * x_4
  x_6 <- rnorm( 10000 )
  x_7 <- rnorm( 10000 )
  x_8 <- rnorm( 10000 )
  x_9 <- rnorm( 10000 )
  cross_levels <- c( "AT",
                     "DE",
                     "NO",
                     "SWE",
                     "GB",
                     "CZE",
                     "ES",
                     "IS",
                     "PL",
                     "HU")

  time <- seq(1:1000)
  time <- rep(time, 10)
  geo_list <- list()
  for(i in 1:length(cross_levels))
  {
    geo <- rep( cross_levels[i],
                1000 )
    geo_list[[i]] <- geo
  }
  geo <- unlist(geo_list)
  geo <- as.data.frame(geo)


  correl_data <-  do.call ( cbind,
                            list( time,
                                  x_1,
                                  x_2,
                                  x_3,
                                  x_4,
                                  x_5,
                                  x_6,
                                  x_7,
                                  x_8,
                                  x_9)
  )
  correl_data <- as.data.frame(correl_data)

  correl_data <- cbind( geo,
                        correl_data)

  names(correl_data) <- c("geo",
                          "time",
                          "x_1",
                          "x_2",
                          "x_3",
                          "x_4",
                          "x_5",
                          "x_6",
                          "x_7",
                          "x_8",
                          "x_9")


test_dummies <- panel_dummify( data = correl_data,
                               cross.section = "geo",
                               time.variable = NULL)

expect_equal(ncol(test_dummies), 20)
expect_equal(as.numeric(unlist(test_dummies[1,11])), 1)
expect_equal(as.numeric(unlist(test_dummies[1,12])), 0)
expect_equal(sum(unlist(test_dummies[1001,12:20])),1)
expect_equal(sum(unlist(test_dummies[3001,12:20])),1)
expect_equal(sum(unlist(test_dummies[5001,12:20])),1)
expect_equal(sum(unlist(test_dummies[7001,12:20])),1)


set.seed(1071)
x_1 <- rnorm( 10000 )
x_2 <- rnorm( 10000 ) + 0.5 * x_1
x_3 <- rnorm( 10000 ) + 0.1 * x_1
x_4 <- rnorm( 10000 ) - 0.7 * x_3
x_5 <- rnorm( 10000 ) + 0.5 * x_4
x_6 <- rnorm( 10000 )
x_7 <- rnorm( 10000 )
x_8 <- rnorm( 10000 )
x_9 <- rnorm( 10000 )
cross_levels <- c( "AT",
                   "DE",
                   "NO",
                   "SWE",
                   "GB",
                   "CZE",
                   "ES",
                   "IS",
                   "PL",
                   "HU")

time <- seq(1:5)
time <- rep(time, 2000)
geo_list <- list()
for(i in 1:length(cross_levels))
{
  geo <- rep( cross_levels[i],
              1000 )
  geo_list[[i]] <- geo
}
geo <- unlist(geo_list)
geo <- as.data.frame(geo)


correl_data <-  do.call ( cbind,
                          list( time,
                                x_1,
                                x_2,
                                x_3,
                                x_4,
                                x_5,
                                x_6,
                                x_7,
                                x_8,
                                x_9)
)
correl_data <- as.data.frame(correl_data)

correl_data <- cbind( geo,
                      correl_data)

names(correl_data) <- c("geo",
                        "time",
                        "x_1",
                        "x_2",
                        "x_3",
                        "x_4",
                        "x_5",
                        "x_6",
                        "x_7",
                        "x_8",
                        "x_9")

test_dummies_both <- panel_dummify( data = correl_data,
                               cross.section = "geo",
                               time.variable = "time")

expect_equal(ncol(test_dummies_both), 24)

expect_equal( sum(as.numeric(unlist(test_dummies_both[1,10:24]))), 2)


expect_equal(sum(as.numeric(unlist(test_dummies_both[1001,10:24]))), 2)

expect_equal(sum(unlist(test_dummies_both[3001, 12:24])), 2)


test_dummies_both <- panel_dummify( data = correl_data,
                                    cross.section = NULL,
                                    time.variable = "time")

expect_equal(ncol(test_dummies_both), 15)

expect_equal( sum(as.numeric(unlist(test_dummies_both[1,11:15]))), 1)


expect_equal(sum(unlist(test_dummies_both[1001,11:15])),1)

expect_equal(sum(unlist(test_dummies_both[3001, 11:15])), 1)


})
