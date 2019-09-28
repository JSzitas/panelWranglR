#' Tidy panel leading
#'
#' @description Efficient, tidy panel leading
#'
#' @param data The data input, anything coercible to a data.table.
#' @param cross.section The cross section argument, see examples.
#' @param time.variable The variable to indicate time in your panel. Defaults to NULL, though it is recommended to have a time variable.
#' @param leads The leads to use in panel leading.
#' @param variables.selected A variable selection for variables to lead, defaults to NULL and leads ALL variables.
#' @param keep.original Whether to keep the original unleadged data, defaults to TRUE.
#' @return The leading data.table which contains either only the leading variables, or also the original variables.
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
#' panel_lead(data = X,
#'           cross.section = "geo_NO",
#'           time.variable = "tim",
#'           leads = 5,
#'           variables.selected = c("V5","tim", "V7"),
#'           keep.original = TRUE)
#'
#'
#'

panel_lead <- function(data,
                       cross.section,
                       time.variable = NULL,
                       leads = 1,
                       variables.selected = NULL,
                       keep.original = TRUE)
{

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
  to_lead <- c(names(data_select))[ !c(names(data_select)) %in% c( cross.section,
                                                                  time.variable )]

  lead_result <- data_select[, shift(.SD, seq_len(leads), NA, "lead", TRUE),
                            .SDcols= to_lead,
                            by = cross.section]
  lead_result[ order( cross.section ) ]
  data[ order( cross.section ) ]
  lead_result[, c(cross.section) := NULL]

  result <- lead_result
  to_convert <- c(names(result))[ !c(names(result)) %in% c( cross.section,
                                                            time.variable )]
  for (col in to_convert)
  {
    set( result, j=col, value = as.numeric(result[[col]]))
  }

  if(keep.original == TRUE){
    result <- cbind( data,
                     lead_result )
    to_convert <- c(names(result))[ !c(names(result)) %in% c( cross.section,
                                                              time.variable )]

    for (col in to_convert)
    {
      set( result, j=col, value = as.numeric(result[[col]]))
    }

  }

  return(result)
}








