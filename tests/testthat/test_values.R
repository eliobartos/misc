context("Test output values")
library(misc)


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

out = split_per_group(df, "x", "group")

query = "
select date, sum(revenue) as revenue
from my_sample_table
where date between 'x1x' and 'x2x'
group by 1
order by 1;
"

p_query = "
select date, sum(revenue) as revenue
from my_sample_table
where date between '2018-09-01' and '2018-09-29'
group by 1
order by 1;
"

test_that("Output values are correct!", {
  expect_equal(get_value(df2, 2), 1)
  expect_equal(get_value(df2, 2, "linear"), 2)
  expect_equal(parametrized_query(query, c('2018-09-01', as.character(Sys.Date()))), p_query)
})

