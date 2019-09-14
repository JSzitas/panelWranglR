test_that("Collecting cross sections or time variables in a panel works", {


   set.seed(1071)
   x_1 <- rnorm( 10000 )
   cross_levels <- c( "AT", "DE", "NO", "SWE", "GB", "CZE", "ES", "IS", "PL", "HU")
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

    correl_data <- cbind( time,
                          x_1 )
    correl_data <- as.data.frame(correl_data)

    correl_data <- cbind( geo,
                          correl_data)
    names(correl_data) <- c("geo", "time", "x_1")

    test <- panel_dummify( data = correl_data,
                                        cross.section = "geo",
                                        time.variable = "time")


   collect_data <- test



  expect_error( panel_collect(collect_data),
                "You have specified nothing. Please specify a column name and columns to gather before proceeding.")
  expect_error( panel_collect(collect_data,
                              cross.section = "Thisisntactuallyintherebutitdoesntmatter"),
                "Please specify column name and columns to collect. You have only specified one argument.")
  expect_error( panel_collect(collect_data,
                cross.section.columns = "Neitherdononexistentcolumnsyet"),
                "Please specify column name and columns to collect. You have only specified one argument.")


  expect_error( panel_collect(collect_data,
                              time.variable = "Wecancallthiswhatever"),
                "Please specify column name and columns to collect. You have only specified one argument.")
  expect_error( panel_collect(collect_data,
                time.variable.columns = "Andthereisnocheckingreally"),
                "Please specify column name and columns to collect. You have only specified one argument.")

   cross_levels <- c( "AT", "DE", "NO", "SWE", "GB", "CZE", "ES", "IS", "PL", "HU")

   test_functionality_cs <- panel_collect( test,
                                        cross.section = "geo",
                                        cross.section.columns = c( "AT", "DE", "NO",
                                                                   "SWE", "GB", "CZE",
                                                                   "ES", "IS", "PL",
                                                                   "HU" ))

   expect_equal( ncol( test_functionality_cs ), 7 )
   expect_equal( nrow( test_functionality_cs ), 10000 )
   expect_true( is.character( unlist( test_functionality_cs[,"geo"] ) ) )


   test_functionality_time <- panel_collect( test,
                                             time.variable = "time",
                                             time.variable.columns = c( "Time_Var_1",
                                                                        "Time_Var_2",
                                                                        "Time_Var_3",
                                                                        "Time_Var_4",
                                                                        "Time_Var_5"))

   expect_equal( ncol( test_functionality_time ), 12 )
   expect_equal( nrow( test_functionality_time ), 10000 )
   expect_equal( length(levels(as.factor(unlist(test_functionality_time[,"time"])))), 5)



   time_levels <- c( "Time_Var_1",
                     "Time_Var_2",
                     "Time_Var_3",
                     "Time_Var_4",
                     "Time_Var_5")

   test_functionality_both <- panel_collect( test,
                                             cross.section = "geo",
                                             cross.section.columns = cross_levels,
                                             time.variable = "time",
                                             time.variable.columns = time_levels)


   expect_equal( ncol( test_functionality_both ), 3 )
   expect_equal( nrow( test_functionality_both ), 10000 )
   expect_true( is.character( unlist( test_functionality_both[,"geo"] ) ) )

   expect_equal( length(levels(as.factor(unlist(test_functionality_both[,"time"])))), 5)
   expect_equal( length(levels(as.factor(unlist(test_functionality_both[,"geo"])))), 10)


})
