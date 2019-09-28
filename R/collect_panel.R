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
#'                        x_1 )
#'  example_data <- as.data.frame(example_data)
#'
#'  example_data <- cbind( geo,
#'                        example_data)
#'  names(example_data) <- c("geo", "time", "x_1")
#'
#' # generate dummies using panel_dummify()
#'  test_dummies <- panel_dummify( data = example_data,
#'                                 cross.section = "geo",
#'                                 time.variable = "time")
#' panel_collect( data = test_dummies,
#'                cross.section = "geo",
#'                cross.section.columns = c( "AT", "DE"))
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

