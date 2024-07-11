test_that("season var exists", {
  expect_error(eval_seasonal_diff({"it's spring!"},
                                  {"it's autumn!"}))
})

test_that("season var is spring or autumn", {
  season <- "summer"

  expect_error(eval_seasonal_diff({"it's spring!"},
                                  {"it's autumn!"}))
})
