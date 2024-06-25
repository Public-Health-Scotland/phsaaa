#' Check global environment for build_history() function.
#'
#' Ensures all variables required to run build_history() exist within the global environment.
#'
#' @param df_hist Dataframe/tibble which contains historical data for relevant KPI.
#' @param df_new Dataframe/tibble containing new data for relevant KPI.
#' @param kpi KPI being added to the historical database, options are: "1.1-1.3", "1.4", "2", or "3"
#'
#' @return
#'
#' @examples
#' build_history_checks(old, new, "2")
build_history_checks <- function(df_hist, df_new, kpi){
  stopifnot(
    "KPI inputted is not included in accepted list, see documentation" = kpi %in% c("1.1-1.3", "1.4", "2", "3")
  )
  stopifnot(
    "no 'season' variable defined in global environment" = exists("season", envir = globalenv())
  )
  stopifnot(
    "no 'hist_path' variable defined in global environment" = exists("hist_path", envir = globalenv())
  )
  stopifnot(
    "no 'kpi_report_years' variable defined in global environment" = exists("kpi_report_years", envir = globalenv())
  )
  stopifnot(
    "no 'fy_list' variable defined in global environment" = exists("fy_list", envir = globalenv())
  )
  stopifnot(
    "no 'hb_list' variable defined in global environment" = exists("hb_list", envir = globalenv())
  )
  if (kpi %in% c("1.1-1.3", "1.4")) {
    stopifnot(
      "no 'year2' variable defined in global environment" = exists("year2", envir = globalenv())
    )
  }
}
