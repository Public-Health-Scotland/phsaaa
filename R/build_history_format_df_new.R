#' Title
#'
#' @param kpi_number
#' @param df_new
#'
#' @return
#'
#' @examples
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
