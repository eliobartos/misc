context("Tests that there are no errors in any function!")
library(misc)
library(tibble)

df <- tribble(
  ~x, ~group,
  1, 1,
  3.2, 1,
  2.4, 1,
  3.1, 1,
  5, 2,
  6, 2,
  4.7, 2
)

df2 = tribble(
  ~x, ~y,
  1, 1,
  5, 5,
  10, 10
)

test_that("No errors test!", {
  expect_error(analyse_var(df, "x", "group"), NA)
  expect_error(get_value(df, 2), NA)
  expect_error(np_mean_test(c(1,2), c(3, 4)), NA)
  expect_error(np_stat_test(c(1,2), c(3, 4), mean), NA)
  expect_error(parametrized_query("lalax1xalf", c("InsertThis")), NA)
  expect_error(split_per_group(df, "x", "group"), NA)
  expect_error(write_excel(df), NA)
})


