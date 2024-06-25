add_new_rows_old <- function(df1, df2, col1, col2){
  stopifnot(
    ("data.frame" %in% class(df1))
  )


  # check distinct col1 + col2 combos in df2
  col1_col2 <- df2 |> dplyr::distinct({{col1}}, {{col2}})
  # assign these to character vectors
  col1_values <- col1_col2 |> dplyr::select({{col1}}) |> dplyr::pull()
  col2_values <- col1_col2 |> dplyr::select({{col2}}) |> dplyr::pull()

  # check these rows aren't already in df1
  dups <- df1 |> dplyr::filter({{col1}} %in% col1_values, {{col2}} %in% col2_values)

  # if no dups, then dfs can be combined, if there are, execution halts
  if (nrow(dups) == 0){
    dplyr::bind_rows(df1, df2)
  } else{
    stop("Dataframe already includes these rows, check both data frames for replication.")
  }
}
