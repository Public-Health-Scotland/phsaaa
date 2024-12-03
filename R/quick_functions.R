# quick single level functions
# AMc 2024
# PWB 4.1.2



# aaa-specific ------------------------------------------------------------

## eval_seasonal_diff ----

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

## viz_kpi_finyear ----

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



# general -----------------------------------------------------------------

## simplify_fy ----

#' Convert long-form financial year (20XX/YY) into short-form (XXYY)
#'
#' @param financial_year Character string object/vector of financial year in format "20XX/YY"
#'
#' @return Character string object/vector of financial year in format "XXYY"
#' @export
#'
#' @examples
#'
#' simplify_fy("2023/24")
#' [1] "2324"
#'
simplify_fy <- function(financial_year) {

  # checks
  stopifnot("financial_year input is not character object" = is.character(financial_year) == TRUE)
  stopifnot("financial_year input is not in format XXXX/YY" = nchar(financial_year) == 7)
  stopifnot("financial_year input is not in format XXXX/YY" = substr(financial_year, 5, 5) == "/")
  stopifnot("financial_year input years are not consecutive" = (as.numeric(substr(financial_year, 6, 7)) - as.numeric(substr(financial_year, 3, 4))) == 1)


  paste0(substr(financial_year, 3, 4), substr(financial_year, 6, 7))
}

## format_excel_hyperlink ----

#' Turn URL into hyperlink for Excel outputs
#'
#' @param name Character string text displayed for link
#' @param url Character string URL
#'
#' @return 1x1 dataframe containing hyperlink ready for writing with openxlsx
#' @export
#'
#' @examples
#' format_excel_hyperlink("Search engine", "www.google.com")
#'
format_excel_hyperlink <- function(name, url) {

  link <- data.frame(hyperlink = paste0("HYPERLINK(\"", url, "\", \"", name, "\")" ))
  class(link$hyperlink) <- c("formula","hyperlink")

  return(link)
}

## format_percentage ----

#' Formatting percentages for outputs
#'
#' Converts numerical values to XX.X% format
#'
#' @param df dataframe with numerical columns to convert
#' @param cols column names for application (uses tidy selection)
#'
#' @return formatted dataframe
#' @export
#'
#' @examples
#'
#' data <- tibble(x = c(2.4, 3.7), y = c(10.9, 82.0))
#'
#' format_perc(data, x)
#'
#' format_perc(data, contains("y"))
#' format_perc(data, everything())
#' format_perc(data, starts_with("x"))
#'
format_perc <- function(df, cols) {
  df |>
    dplyr::mutate(dplyr::across({{ cols }}, ~sprintf("%.1f", .))) |>
    dplyr::mutate(dplyr::across({{ cols }}, ~ paste0(., "%")))
}

