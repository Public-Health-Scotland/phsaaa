#' Helper to build_history() which creates new historical database and formats it
#'
#' @param df_hist Passed from build_history()
#' @param df_new Passed from build_history()
#' @param kpi_number Passed from build_history()
#'
#' @return Creates new hist_db within build_history() environment
#'
build_history_create_new_hist <- function(df_hist, df_new, kpi_number) {

  new_hist_db <- add_new_rows(df1 = df_hist, df2 = df_new, fin_year, kpi) |>
    dplyr::mutate(fin_year = forcats::fct_relevel(fin_year, c(fy_list)),
                  hbres = forcats::fct_relevel(hbres, c(hb_list)))

  if(kpi_number == "1.1-1.3"){
    new_hist_db <- new_hist_db |>
      dplyr::mutate(
        kpi = forcats::fct_relevel(
          kpi, c("KPI 1.1", "KPI 1.1 Scotland SIMD",
                 "KPI 1.1 Scotland SIMD Sept coverage",
                 "KPI 1.2a", "KPI 1.2a Sept coverage",
                 "KPI 1.2b", "KPI 1.3a Scotland SIMD",
                 "KPI 1.3a Sept coverage", "KPI 1.3a HB SIMD",
                 "KPI 1.3b Scotland SIMD", "KPI 1.3b HB SIMD",
                 "KPI 1.4a", "KPI 1.4b")))
  }

  new_hist_db <- new_hist_db |>
    dplyr::arrange(kpi, fin_year, hbres)

  return(new_hist_db)
}
