#' Tidy time/variable dummies for panel data
#'
#' @description A simple function to dummify cross sections or time variables in panel data.
#'
#' @param data The panel to dummify
#' @param cross.section The cross section variable in the panel. Defaults to NULL.
#' @param time.variable The variable to indicate time in your panel. Defaults to NULL.
#' @return A new data.table, with the original variables to dummify removed, and new dummy columns included.
#' @details The encoding is binary, whether this is more appropriate than using a factor variable is up to the user.
#' @import data.table
#' @export
#' @examples
#'
#' x_1 <- rnorm( 10 )
#' cross_levels <- c( "AT", "DE" )
#' time <- seq(1:5)
#' time <- rep(time, 2)
#' geo_list <- list()
#' for(i in 1:length(cross_levels))
#' {
#'   geo <- rep( cross_levels[i],
#'                 100 )
#'                   geo_list[[i]] <- geo
#'                   }
#'                   geo <- unlist(geo_list)
#'                   geo <- as.data.frame(geo)
#'
#'  example_data <- cbind( time,
#'                         x_1 )
#'  example_data <- as.data.frame(example_data)
#'
#'  example_data <- cbind( geo,
#'                         example_data)
#'  names(example_data) <- c("geo", "time", "x_1")
#'
#'  test_dummies <- panel_dummify( data = example_data,
#'                                 cross.section = "geo",
#'                                 time.variable = "time")
#'
#'
#'
#'
#'





panel_dummify <- function( data,
                           cross.section = NULL,
                           time.variable = NULL)
  {
  data <- data.table::data.table(data)
  if(!is.null(cross.section))
  {
    cross_list <- levels(as.factor(unlist(data[,c(cross.section), with = FALSE])))

    dummy_panel <- data[, (cross_list) := lapply(cross_list, function(i)
    {
      as.numeric( eval(as.name(paste(cross.section))) == i)
    })]

    dummy_panel <- dummy_panel[, c(cross.section) := NULL]
  }


  if(!is.null(time.variable))
   {
    time_list <- levels(as.factor(unlist(data[,c(time.variable), with = FALSE])))

    time_vars <- paste("Time_Var_", time_list, sep = "")

    dummy_panel <- data[, (time_vars) := lapply(time_list, function(i)
    {
      as.numeric( eval(as.name(paste(time.variable))) == i)
    })]

    dummy_panel <- dummy_panel[, c(time.variable) := NULL]

    return(dummy_panel)
   }
   else if(is.null(cross.section) && is.null(time.variable))
    {
    stop("Please specify the type of dummy you require - time or cross sectional.")
    }

  return(dummy_panel)
  }
