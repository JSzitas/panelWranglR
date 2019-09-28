#' Tidy panel lagging
#'
#' @description Efficient, tidy panel lagging
#'
#' @param data The data input, anything coercible to a data.table.
#' @param cross.section The cross section argument, see examples.
#' @param time.variable The variable to indicate time in your panel. Defaults to NULL, though it is recommended to have a time variable.
#' @param lags The lags to use in panel lagging.
#' @param variables.selected A variable selection for variables to lag, defaults to NULL and lags ALL variables.
#' @param keep.original Whether to keep the original unlagged data, defaults to TRUE.
#' @return The lagged data.table which contains either only the lagged variables, or also the original variables.
#' @details Works on a full data.table backend for maximum speed wherever possible.
#' @import data.table
#' @export
#' @examples
#'
#'
#' X <- matrix(rnorm(4000),800,5)
#' tim <- seq(1:400)
#' geo_AT <- rep(c("AT"), length = 400)
#' geo_NO <- rep(c("NO"), length = 400)
#' both_vec_1 <- cbind(tim,geo_NO)
#' both_vec_2 <- cbind(tim,geo_AT)
#' both <- rbind(both_vec_1,both_vec_2)
#' names(both[,"geo_NO"]) <- "geo"
#' X <- cbind(both,X)
#'
#' panel_lag(data = X,
#'           cross.section = "geo_NO",
#'           time.variable = "tim",
#'           lags = 5,
#'           variables.selected = c("V5","tim", "V7"),
#'           keep.original = TRUE)
#'
#'
#'


panel_lag <- function(data,
                      cross.section,
                      time.variable = NULL,
                      lags = 1,
                      variables.selected = NULL,
                      keep.original = TRUE)
{

  data <- data.table::data.table(data, key = cross.section)

  if(!is.null(variables.selected))
  {
    variables.selected <- c(variables.selected,
                            cross.section,
                            time.variable)

    data_select <- data[,c( variables.selected ), with = FALSE]
  }
  else
  {
    data_select <- data
  }
  to_lag <- c(names(data_select))[ !c(names(data_select)) %in% c( cross.section,
                                                                  time.variable )]

  lag_result <- data_select[, shift(.SD, seq_len(lags), NA, "shift", TRUE),
                            .SDcols= to_lag,
                            by = cross.section]
  lag_result[ order( cross.section ) ]
  data[ order( cross.section ) ]
  lag_result[, c(cross.section) := NULL]

  result <- lag_result
  to_convert <- c(names(result))[ !c(names(result)) %in% c( cross.section,
                                                            time.variable )]
  for (col in to_convert)
  {
    set( result, j=col, value = as.numeric(result[[col]]))
  }

  if(keep.original == TRUE){
    result <- cbind( data,
                     lag_result )
    to_convert <- c(names(result))[ !c(names(result)) %in% c( cross.section,
                                                              time.variable )]

    for (col in to_convert)
    {
      set( result, j=col, value = as.numeric(result[[col]]))
    }

  }

  return(result)
}
