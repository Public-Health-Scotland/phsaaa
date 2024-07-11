test_that("correct nchar outputs", {
  filenames <- build_history_filenames("2")

  combined_hist <- nchar(filenames$filename_hist)
  combined_bckp <- nchar(filenames$filename_bckp)

  sum_hist <- sum(nchar(filenames$historical) + nchar(filenames$theme) + nchar(filenames$reg))
  sum_bckp <- sum(nchar(filenames$historical) + nchar(filenames$theme) + nchar(filenames$bckp))

  expect_equal(combined_hist, sum_hist)
  expect_equal(combined_bckp, sum_bckp)
})

test_that("correct theme numbers", {
 filenames <- build_history_filenames("1.1-1.3")
 expect_equal(filenames$theme, "theme2")

 filenames <- build_history_filenames("1.4")
 expect_equal(filenames$theme, "theme2")

 filenames <- build_history_filenames("2")
 expect_equal(filenames$theme, "theme3")

 filenames <- build_history_filenames("3")
 expect_equal(filenames$theme, "theme4")

})
