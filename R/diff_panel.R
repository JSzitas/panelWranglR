#' Tidy panel differencing
#'
#' @description Efficient, tidy panel differencing
#'
#' @param data The data input, anything coercible to a data.table.
#' @param cross.section The cross section argument, see examples.
#' @param time.variable The variable to indicate time in your panel. Defaults to NULL, though it is recommended to have a time variable.
#' @param diff.order The number of applications of the difference operator to use in panel differencing. Defaults to 1.
#' @param lags The number of lags to use for differences. Defaults to 1.
#' @param variables.selected A variable selection for variables to difference, defaults to NULL and differences ALL variables.
#' @param keep.original Whether to keep the original undifferenced data, defaults to FALSE.
#' @return The differenced data.table which contains either only the differenced variables, or also the original variables.
#' @details Works on a full data.table backend for maximum speed wherever possible.
#' @import data.table
#' @export
#' @examples
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
#' panel_diff(data = X,
#'            cross.section = "geo_NO",
#'            time.variable = "tim",
#'            diff.order = 1,
#'            lags = 1,
#'            variables.selected = c("V3","V4"),
#'            keep.original = TRUE)
#'
#'

panel_diff <- function(data,
                      cross.section,
                      time.variable = NULL,
                      diff.order = 1,
                      lags = 1,
                      variables.selected = NULL,
                      keep.original = FALSE)
{

  for( i in 1:diff.order)
  {

    helper_fun <- function(data_fm,
                           cross.section.var,
                           time.variable.var,
                           lags.n,
                           variables.selected.col,
                           keep.original.tf){
      data <- data.table::data.table(data)

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

      lag_result <- data_select[, shift(.SD, lags, NA, "shift", TRUE),
                                .SDcols= to_lag,
                                by = cross.section]
      lag_result[ order( cross.section ) ]
      lag_result[, c(cross.section) := NULL]
      base_mat <- data[order( cross.section )]
      base_mat <- apply(data[, setdiff( names(data),
                                        c(cross.section,
                                          time.variable)),
                             with = FALSE], 2, as.numeric)
      base_mat <- as.matrix( base_mat )
      lag_res <- apply( lag_result, 2, as.numeric)
      lag_res <- as.matrix( lag_res )
if( !is.null(variables.selected.col))
  {
  diff_result <- base_mat[,variables.selected.col] - lag_res
}
else
{
  diff_result <- base_mat - lag_res
}

      diff_result <- data.table::data.table(diff_result)
      diff_result <- cbind( data[,c(cross.section,
                                    time.variable),
                                 with = FALSE],
                            diff_result )


      if(keep.original == T)
      {
        diff_result <- cbind( data,
                              diff_result )
      }

      return(diff_result)
    }
    data <- helper_fun(data_fm = data,
                       cross.section.var = cross.section,
                       time.variable.var = time.variable,
                       lags.n = lags,
                       variables.selected.col = variables.selected,
                       keep.original.tf = keep.original)





  }
return(data)
}








