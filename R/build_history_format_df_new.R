#' Create and format new historical database for build_history()
#'
#' @param kpi_number Passed from build_history()
#' @param df_new Passed from build_history()
#'
#' @return New historical database in the build_history() environment
#'
build_history_format <- function(kpi_number, df_new) {

  if(kpi_number == "1.1-1.3"){
    df_new <- df_new |>
      dplyr::filter(kpi != "KPI 1.1 Sept coverage",
                    fin_year != year2)
  }
  else {
    df_new <- df_new |>
      dplyr::filter(fin_year == kpi_report_years[3])
  }
}
