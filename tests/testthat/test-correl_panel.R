test_that("Panel correlation works", {

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


test_correl <- panel_correl( data = correl_data,
                             cross.section = "geo",
                             time.variable = "time",
                             corr.threshold = 0.2)

expect_true(is.list(test_correl))
expect_equal(names(test_correl), c( "Baseline",
                                    "Time dependency",
                                    "Cross sectional" ))
expect_equal( test_correl[[1]], c(4,3,1) )

expect_equal( class( test_correl[[2]] ), "list" )

expect_equal( sum(
                  unlist(
                         lapply(
                                test_correl, is.na
                                                  ))),
              0)




})
