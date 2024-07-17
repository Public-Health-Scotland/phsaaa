#' Checks for add_new_rows() function
#'
#' @param df1 Dataframe, specified within add_new_rows()
#' @param df2 Dataframe, specified within add_new_rows()
#'
#' @return Nothing, unless issue - then error
#'
add_new_rows_tests <- function (df1, df2) {
  stopifnot(
    "df1 is not a dataframe/tibble" = ("data.frame" %in% class(df1))
  )
  stopifnot(
    "df2 is not a dataframe/tibble" = ("data.frame" %in% class(df2))
  )
  stopifnot(
    "column names are not the same in both dataframes" = length(setdiff(names(df1), names(df2))) == 0
  )
  stopifnot(
    "df1 has no observations" = nrow(df1) >= 1
  )
  stopifnot(
    "df2 has no observations" = nrow(df2) >= 1
  )
}
