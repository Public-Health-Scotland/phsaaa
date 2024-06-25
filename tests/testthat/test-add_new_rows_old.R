test_that("output contains sum of rows", {
  df1 <- data.frame(fin_year = c("2021/2022", "2022/23"), kpi = "KPI 1.1", value = 1:2)
  df2 <- data.frame(fin_year = c("2023/24"), kpi = "KPI 1.1", value = 3)

  combined <- add_new_rows_old(df1, df2, fin_year, kpi)
  expect_equal(nrow(combined), nrow(df1) + nrow(df2))
})

test_that("add_new_rows requires a dataframe", {
  df1 <- "not a data frame"
  df2 <- data.frame(fin_year = c("2023/24"), kpi = "KPI 1.1", value = 3)

  expect_error(add_new_rows_old(df1, df2, fin_year, kpi))
})
