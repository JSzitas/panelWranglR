#' Collect a panel, from wide to long
#'
#' @description Transforms cross sectional/time dummies to unified variables
#'
#' @param data The panel to transform
#' @param cross.section The name of the transformed cross sectional variable supply as chracter.
#' @param cross.section.columns The names of the columns indicating cross sections to collect.
#' @param time.variable The name of the transformed time variable supply as character.
#' @param time.variable.columns The names of the columns indicating time variables to collect.
#' @return A collected data.table, with new columns constructed by collecting from the wide format.
#' @details For time variables named like "Time_Var_i" with arbitrary i, the program will check that all time variables are named using this convention, and strip this convention
#' @import data.table
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #'    x_1 <- rnorm( 10000 )
#'    x_2 <- rnorm( 10000 ) + 0.5 * x_1
#'    x_3 <- rnorm( 10000 ) + 0.1 * x_1
#'    x_4 <- rnorm( 10000 ) - 0.7 * x_3
#'    x_5 <- rnorm( 10000 ) + 0.5 * x_4
#'    x_6 <- rnorm( 10000 )
#'    x_7 <- rnorm( 10000 )
#'    x_8 <- rnorm( 10000 )
#'    x_9 <- rnorm( 10000 )
#'    cross_levels <- c( "AT", "DE", "NO", "SWE", "GB", "CZE", "ES", "IS", "PL", "HU")
#'    time <- seq(1:1000)
#'    time <- rep(time, 10)
#'    geo_list <- list()
#'    for(i in 1:length(cross_levels))
#'    {  geo <- rep( cross_levels[i],
#'                  1000 )
#'                    geo_list[[i]] <- geo }
#'    geo <- unlist(geo_list)
#'    geo <- as.data.frame(geo)
#'
#'    correl_data <-  do.call ( cbind, list( time, x_1, x_2,
#'                                            x_3, x_4, x_5,
#'                                            x_6, x_7, x_8,
#'                                            x_9))
#'    correl_data <- as.data.frame(correl_data)
#'    correl_data <- cbind( geo,
#'                          correl_data)
#'
#'                          names(correl_data) <- c("geo", "time", "x_1",
#'                                                  "x_2", "x_3",  "x_4",
#'                                                  "x_5", "x_6",  "x_7",
#'                                                  "x_8", "x_9")
#'
#'  X <- correl_data
#'
#' panel_collect( X,
#'                cross.section = "geo",
#'                cross.section.columns = c( "SWE", "AT", "NO" ))
#'
#' panel_collect( X,
#'                cross.section = "geo",
#'                cross.section.columns = c( "AT", "CZE", "DE",
#'                                           "ES", "GB",  "HU",
#'                                          "IS", "NO",  "PL",
#'                                           "SWE"),
#'                time.variable = "time",
#'                time.variable.columns = c( "Time_Var_1",
#'                                           "Time_Var_2",
#'                                           "Time_Var_3",
#'                                           "Time_Var_4",
#'                                           "Time_Var_5" ))
#'
#' panel_collect( X,
#'                time.variable = "time",
#'                time.variable.columns = c( "Time_Var_1",
#'                                           "Time_Var_2",
#'                                           "Time_Var_3",
#'                                           "Time_Var_4",
#'                                           "Time_Var_5" ))
#'
#' }
#'
#'



panel_collect <- function( data,
                           cross.section = NULL,
                           cross.section.columns = NULL,
                           time.variable = NULL,
                           time.variable.columns = NULL)
{
  if(!is.null(cross.section) && is.null(cross.section.columns) ||
     is.null(cross.section) && !is.null(cross.section.columns))
  {
    stop(" Please specify column name and columns to collect. You have only specified one argument.")
  }
  else if(!is.null(time.variable) && is.null(time.variable.columns) ||
     is.null(time.variable) && !is.null(time.variable.columns))
  {
    stop(" Please specify column name and columns to collect. You have only specified one argument.")
  }
  else if(is.null(time.variable) && is.null(time.variable.columns) &&
          is.null(cross.section) && is.null(cross.section.columns))
  {
    stop("You have specified nothing. Please specify a column name and columns to gather before proceeding.")
  }

  data <- data.table::as.data.table(data)
  # due to cran check
  time <- NULL
  cross <- NULL
  i.to <- NULL
  if(!is.null(cross.section) && !is.null(cross.section.columns))
  {

    data <- data.table::as.data.table(data)
    data <- data[, cross := "0"]
    for (i in cross.section.columns)
    {
      data <- data[eval( as.name( paste( i ) ) ) == "1", cross := i]
    }

    data[, (cross.section.columns) := NULL]
    data.table::setnames(data, old = "cross", new = cross.section)
  }

  if(!is.null(time.variable) && !is.null(time.variable.columns))
  {


    data <- data[, time := "0"]
    for (i in time.variable.columns)
    {
      data <- data[eval( as.name( paste( i ) ) ) == "1", time := i]
    }

    data[, (time.variable.columns) := NULL]

    time_len <- length( levels( as.factor( data$time ) ) )

    check_time <- list()
    for (i in 1:time_len){
      check_time[[i]] <- strsplit(levels( as.factor( data$time) ), split = "ime_Var_")[[i]][2]
    }
    check_time <- unlist(check_time)
    if( as.numeric( check_time )[1] == 1 &&
        as.numeric( check_time )[2] == 2 &&
        as.numeric( check_time )[time_len] == time_len )
    {
      helper_table <- data.table::data.table( time = levels( as.factor( data$time)),
                                              to = check_time )


      data[ helper_table, on = "time", time := i.to]

    }



    data.table::setnames(data, old = "time", new = time.variable )

  }

  else
  {
    return( data )
  }
}

