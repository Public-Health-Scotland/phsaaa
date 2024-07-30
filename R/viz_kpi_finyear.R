#' Create crosstab of 'kpi' and 'fin_year' for dataframe.
#'
#' @param df A dataframe/tibble containing columns 'kpi' and 'fin_year'
#'
#' @return Frequency table of 'kpi' x 'fin_year' for the specified dataframe.
#' @export
#'
viz_kpi_finyear <- function(df) {
  table(df$kpi, df$fin_year)
}
