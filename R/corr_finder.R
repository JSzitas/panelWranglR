#' Wrapper for find correlations
#'
#' @description Just a helper function for correl_panel.
#'
#' @param df The dataframe to use.
#' @param corr_cutoff The correlation cutoff to pass to findCorrelations
#' @importFrom Hmisc rcorr
#' @importFrom caret findCorrelation
#' @export
#' @examples
#'
#' X_1 <- rnorm(1000)
#' X_2 <- rnorm(1000) + 0.6 * X_1
#' X_3 <- rnorm(1000) - 0.4 * X_1
#'
#' data_fm <- do.call( cbind, list( X_1,
#'                                  X_2,
#'                                  X_3 ))
#'
#' corr_finder( df = data_fm,
#'              corr_cutoff = 0.3 )
#'
#'




corr_finder <- function(df, corr_cutoff){
  df_corr <- as.matrix(df)
  df_corr <- Hmisc::rcorr(df_corr)
  corrs <- caret::findCorrelation( df_corr[["r"]], corr_cutoff )
  return(corrs)
}
