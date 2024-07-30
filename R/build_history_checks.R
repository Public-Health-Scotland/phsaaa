#' Check global environment for build_history() function.
#'
#' Ensures all variables required to run build_history() exist within the global environment.
#'
#' @param kpi_number KPI being added to the historical database, options are: "1.1-1.3", "1.4", "2", or "3"
#'
#' @return Nothing returned, just stops execution if error.
#'
#' @examples
#' build_history_checks(old, new, "2")
build_history_checks <- function(kpi_number){
  stopifnot(
    "KPI inputted is not included in accepted list, see documentation" = kpi_number %in% c("1.1-1.3", "1.4", "2", "3")
  )
  stopifnot(
    "no 'fy_list' variable defined in global environment" = exists("fy_list", envir = globalenv())
  )
  stopifnot(
    "no 'hb_list' variable defined in global environment" = exists("hb_list", envir = globalenv())
  )
  if (kpi_number %in% c("1.1-1.3", "1.4")) {
    stopifnot(
      "no 'year2' variable defined in global environment" = exists("year2", envir = globalenv())
    )
  }
}
