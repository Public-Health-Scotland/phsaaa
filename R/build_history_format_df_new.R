#' Create and format new historical database for build_history()
#'
#' @param kpi_number KPI being added to the historical database, options are: "1.1-1.3", "1.4", "2", or "3"
#' @param df_new Dataframe/tibble containing new data for relevant KPI.
#' @param year_n Used for KPIs 1.1-1.3 or 1.4, character variable of a financial year. Default is 'year2', which corresponds to the financial year following that which is being published.
#' @param kpi_report_years Character vector, must be >= 3 length. Includes financial year being published, plus two prior.
#'
#' @return New historical database in the build_history() environment
#'
build_history_format_df_new <- function(kpi_number, df_new, year_n = year2, kpi_report_years = kpi_report_years) {

  if(kpi_number == "1.1-1.3"){
    df_new |>
      dplyr::filter(kpi != "KPI 1.1 Sept coverage",
                    fin_year != year_n)
  }
  else {
    df_new |>
      dplyr::filter(fin_year == kpi_report_years[3])
  }
}
