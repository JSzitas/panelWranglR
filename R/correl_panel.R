#' Panel linear combinations
#'
#' @description A function to find highly correlated variables in a panel of data, both by cross sections and by time dummies.
#'
#' @param data The data to use, a data.frame or a data.table.
#' @param cross.section The name of the cross sectional variable.
#' @param time.variable The name of the time variable.
#' @param corr.threshold The correlation threshold for finding significant correlations in the base specification, disregarding time or cross sectional dependencies.
#' @param autocorr.threshold The correlation threshold for autocorrelation (splitting the pooled panel into cross sections).
#' @param cross.threshold The correlation threshold for finding significant correlations in the cross sections.
#' @param select.cross.sections An optional subset of cross sectional units.
#' @param select.time.periods An optional subset of time periods
#' @import data.table
#' @importFrom stats na.omit
#' @export
#' @examples
#'
#'    x_1 <- rnorm( 100 )
#'    x_2 <- rnorm( 100 ) + 0.5 * x_1
#'    cross_levels <- c( "AT", "DE")
#'    time <- seq(1:50)
#'    time <- rep(time, 2)
#'    geo_list <- list()
#'    for(i in 1:length(cross_levels))
#'    {  geo <- rep( cross_levels[i], 50 )
#'       geo_list[[i]] <- geo }
#'    geo <- unlist(geo_list)
#'    geo <- as.data.frame(geo)
#'
#'    example_data <-  do.call ( cbind, list( time, x_1, x_2))
#'    example_data <- as.data.frame(example_data)
#'    example_data <- cbind( geo,
#'                          example_data)
#'
#'                          names(example_data) <- c("geo", "time", "x_1",
#'                                                  "x_2")
#'
#'    panel_correl( data = example_data,
#'                  cross.section = "geo",
#'                  time.variable = "time",
#'                  corr.threshold = 0.2,
#'                  autocorr.threshold = 0.5,
#'                  cross.threshold = 0.1)
#'




panel_correl <- function(data,
                         cross.section = NULL,
                         time.variable = NULL,
                         corr.threshold = 0.7,
                         autocorr.threshold = 0.5,
                         cross.threshold = 0.7,
                         select.cross.sections = NULL,
                         select.time.periods = NULL)
  {


  data <- data.table::data.table(data)

  result_base <- data[, !c(cross.section,
                          time.variable), with = FALSE ]
  result_base <- corr_finder( result_base, corr.threshold )

  result_full <- list(result_base)
  names(result_full) <- c("Baseline")

  if (!is.null(time.variable)) {
    # this might seem weird - recognize that having a time variable implies having a time
    # dependency - it might therefore make sense to test autocorrelation in the data.

    time_obj <- list()
    for( i in levels(as.factor(unlist(data[, c(cross.section), with = FALSE]))) )
    {
      time_obj[[i]] <- list()
    }
  column_names <- colnames(data[, !c(cross.section, time.variable), with = FALSE ])

  new_obj <- list()

    for (i in names(time_obj)){


    time_obj[[i]] <- data[ eval(as.name(paste(cross.section))) == i,]
    time_obj[[i]] <- time_obj[[i]][, c(cross.section, time.variable) := NULL ]
    time_obj[[i]] <- data.table::shift( time_obj[[i]], n = seq_along(1:20),
                        give.names = TRUE)
    time_obj[[i]] <- data.table::as.data.table( time_obj[[i]] )
    # the above just generates the lags that we need.

    time_obj[[i]] <- cbind( data[ eval(as.name(paste(cross.section))) == i,],
                            time_obj[[i]] )
    time_obj[[i]] <- time_obj[[i]][, c(cross.section, time.variable) := NULL ]
    # then we bind them to the original data set, and we drop the cross section and the time variable

    time_obj[[i]] <- na.omit( time_obj[[i]] )
    # finally drop missing observations ( as the corr_finder will throw an error if there is
    # anything that is not a numeric )

 # this seems more complicated than it is, but it just goes through the column name patterns,
 # knowing that the data.table::shift lags a variable and names the new variable like
 # old_var >> data.table::shift >> old_var_Lag_1, so we just match the old_var pattern
 # using grep to limit dependencies
    for( j in column_names){
      new_obj[[j]] <- time_obj[[i]][, grep(j, names(time_obj[[i]])), with = FALSE]
      new_obj[[j]] <- corr_finder( new_obj[[j]], autocorr.threshold )
      # note that we are still within the map cycle! therefore, we still index with the i for
      # time_obj, and we need a new_obj to handle indexing through j

    }
    time_obj[[i]] <- new_obj

  }
  names(time_obj) <- levels(as.factor(unlist(data[,c(cross.section), with = FALSE])))


    result_full[[2]] <- time_obj

    names(result_full) <- c("Baseline", "Time dependency")
}



  if (!is.null(cross.section)){

    cs_obj <- list()
    for( i in levels(as.factor(unlist(data[, c(cross.section), with = FALSE]))) )
    {
      cs_obj[[i]] <- list()
      cs_obj[[i]] <- data[ eval(as.name(paste(cross.section))) == i,]
      cs_obj[[i]] <- cs_obj[[i]][, c(cross.section, time.variable) :=NULL ]
      cs_obj[[i]] <- corr_finder( cs_obj[[i]], cross.threshold)
    }


 if(!is.null(result_full[[2]]))
  {
      result_full[[3]] <- cs_obj
      names(result_full) <- c("Baseline", "Time dependency", "Cross sectional")
  }
 else if(!is.null(cs_obj))
  {
      result_full[[2]] <- cs_obj
      names(result_full) <- c("Baseline", "Cross sectional")

 }
 else
 {
   result_full <- result_full
 }
      }


    return(result_full)
  }


