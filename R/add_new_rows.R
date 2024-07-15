#' Bind rows of two data frames if no duplication within specified column(s)
#'
#' @param df1 pre-existing data frame/tibble that you want to add rows to
#' @param df2 new data frame/tibble that contain rows to add (must have exactly the same columns as df1)
#' @param ... unquoted column names that you want to check duplication within
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
