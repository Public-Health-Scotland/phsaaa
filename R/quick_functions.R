# quick single level functions
# AMc 2024
# PWB 4.1.2


# eval_seasonal_diff ------------------------------------------------------

#' Evaluate one of two expressions depending on the season.
#'
#' Function to evaluate spring- or autumn-specific expressions, cutting down on long ifelse statements.
#' Can be used to run functions alone, or assign values to variables in the global environment.
#' Note: function includes 'season' variable which should be defined as "spring" or "autumn" before using the function.
#'
#' @param season variable indicating season, either "spring" or "autumn"
#' @param expr_spring {expression} to be evaluated when season == "spring"
#' @param expr_autumn {expression} to be evaluated when season == "autumn"
#'
#' @return Runs function or assigns value to variable.
#' @export
#'
#' @examples
#'
#' season <- "autumn"
#'
#' x <- eval_seasonal_diff(season, {"it's spring!"}, {"it's autumn!"})
#'
#' print(x)
#' [1] "it's autumn!"
#'
#'
eval_seasonal_diff <- function(season, expr_spring, expr_autumn) {

  # # ensuring season exists
  # stopifnot(
  #   "no 'season' variable defined in global environment" = exists("season", envir = globalenv())
  #   )

  if (season == "spring") {

    # evaluates spring expression, output into global environment
    eval(substitute(expr_spring), envir = .GlobalEnv)

  } else if (season == "autumn") {

    # evaluates autumn expression, output into global environment
    eval(substitute(expr_autumn), envir = .GlobalEnv)

  } else {

    stop("Go check your calendar!") # prevents function running if season is something else

  }
}


# viz_kpi_finyear ---------------------------------------------------------

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


