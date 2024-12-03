# add_new_rows() function plus helpers
# AMc 2024
# PWB 4.1.2



# add_new_rows ------------------------------------------------------------

#' Bind rows of two data frames if no duplication within specified column(s)
#'
#' @param df1 pre-existing data frame/tibble that you want to add rows to
#' @param df2 new data frame/tibble that contain rows to add (must have exactly the same columns as df1)
#' @param ... unquoted column names which specify grouping variables across which you want to check for duplication (no limit on how many or few you can use)
#'
#' @return If no duplication, a tibble containing unique rows from data frame 1 and 2. If duplication exists, halts execution with error message.
#' @export
#'
#' @examples
#' df1 <- dplyr::tibble(fin_year = c("2021/22", "2022/23"), kpi = "KPI 1.1", value = 1:2)
#' df2 <- dplyr::tibble(fin_year = "2023/24", kpi = "KPI 1.1", value = 2)
#'
#' add_new_rows(df1, df2, fin_year, kpi)
#'
#' #  A tibble: 3 × 3
#' #        fin_year     kpi    value
#' #          <chr>    <chr>   <dbl>
#' #   1     2021/22  KPI 1.1     1
#' #   2     2022/23  KPI 1.1     2
#' #   3     2023/24  KPI 1.1     2
add_new_rows <- function(df1, df2, ...){

  # column data
  cols <- rlang::enquos(...) # turns col names into quosures
  num_cols <- length(cols) # gets number of cols in list


  # initial tests
  add_new_rows_tests(df1, df2)

  # check distinct values within columns in df2
  distinct_df2 <- df2 |> dplyr::distinct(!!!cols)

  # select full information in df1 for cols
  full_df1 <- df1 |> dplyr::select(!!!cols)

  # semi-join checks dups - returns values in both dfs
  dups <- full_df1 |>
    dplyr::semi_join(distinct_df2, by = names(distinct_df2))


  # if no dups, then dfs can be combined, if there are, execution halts
  if (nrow(dups) == 0){
    dplyr::bind_rows(df1, df2)
  } else{
    stop("Dataframe already includes these rows, check both data frames for replication.")
  }
}



# add_new_rows_tests ------------------------------------------------------

# FUNCTION TO CHECK ADD_NEW_ROWS()
# 2 dataframes as inputs, no outputs

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

