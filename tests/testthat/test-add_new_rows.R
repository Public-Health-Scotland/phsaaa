# dataframe and column tests
test_that("add_new_rows requires a dataframe", {
  df1 <- "not a data frame"
  df2 <- data.frame(fin_year = c("2023/24"), kpi = "KPI 1.1", value = 3)

  expect_error(add_new_rows(df1, df2, fin_year, kpi))
})

test_that("column names need to be identical",{
  df1 <- data.frame(fin_year = c("2021/2022", "2022/23"), kpi = "KPI 1.1", value = 1:2)
  df2 <- data.frame(fin_year = c("2023/24"), kpi = "KPI 1.1", value = 3, extra = "add")

  expect_error(add_new_rows(df1, df2, kpi))
})

test_that("there are >= 1 observation in each dataframe", {
  df1 <- data.frame(fin_year = c("2021/2022", "2022/23"), kpi = "KPI 1.1", value = 1:2)
  df2 <- data.frame(fin_year = character(0), kpi = character(0), value = integer(0))

  expect_error(add_new_rows(df1, df2, kpi))
})

# intermediate tests

# test_that("distinct check has same number of columns as quoted", {
#   df1 <- data.frame(fin_year = c("2021/2022", "2022/23"), kpi = "KPI 1.1", value = 1:2)
#   df2 <- data.frame(fin_year = c("2023/24"), kpi = "KPI 1.1", value = 3, extra = "add")
#
# })


# output tests
test_that("output contains sum of rows", {
  df1 <- data.frame(fin_year = c("2021/2022", "2022/23"), kpi = "KPI 1.1", value = 1:2)
  df2 <- data.frame(fin_year = c("2023/24"), kpi = "KPI 1.1", value = 3)

  combined <- add_new_rows(df1, df2, fin_year, kpi)
  expect_equal(nrow(combined), nrow(df1) + nrow(df2))
})

test_that("output has same columns as input df1", {
  df1 <- data.frame(fin_year = c("2021/2022", "2022/23"), kpi = "KPI 1.1", value = 1:2)
  df2 <- data.frame(fin_year = c("2023/24"), kpi = "KPI 1.1", value = 3)

  combined <- add_new_rows(df1, df2, fin_year, kpi)

  expect_equal(names(combined), names(df1))
})

test_that("output has same columns as input df2", {
  df1 <- data.frame(fin_year = c("2021/2022", "2022/23"), kpi = "KPI 1.1", value = 1:2)
  df2 <- data.frame(fin_year = c("2023/24"), kpi = "KPI 1.1", value = 3)

  combined <- add_new_rows(df1, df2, fin_year, kpi)

  expect_equal(names(combined), names(df2))
})

# test_that("output is distinct based on variables supplied", {
#   df1 <- data.frame(fin_year = c("2021/22", "2022/23"), kpi = "KPI 1.1", value = 1:2)
#   df2 <- data.frame(fin_year = c("2023/24"), kpi = "KPI 1.1", value = 3)
#
#   distinct_vars <- c("fin_year", "kpi")
#
#   combined <- add_new_rows(df1, df2, fin_year, kpi)
#
#   test <- combined |>
#     dplyr::group_by(fin_year, kpi) |>
#     dplyr::summarise(num = n())
# # need some way to test that each value of test$n is the same
# })
