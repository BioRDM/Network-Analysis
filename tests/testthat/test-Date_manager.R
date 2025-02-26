test_that("get_years_from_to works correctly with split_per_year", {
  data <- data.frame(
    Year = c(2000, 2002, 2004, 2006, 2008, 2010)
  )
  config <- list(
    year_column_name = "Year",
    split_per_year = 2
  )

  result <- get_years_from_to(data, config)
  expect_equal(result$years_from, c(2000, 2002, 2004, 2006, 2008, 2010))
  expect_equal(result$years_to, c(2001, 2003, 2005, 2007, 2009, 2010))
})

test_that("get_years_from_to works correctly without split_per_year", {
  data <- data.frame(
    Year = c(2000, 2002, 2004, 2006, 2008, 2010)
  )
  config <- list(
    year_column_name = "Year",
    from_year = 2000,
    to_year = 2010,
    split_per_year = NULL
  )

  result <- get_years_from_to(data, config)
  expect_equal(result$years_from, 2000)
  expect_equal(result$years_to, 2010)
})

test_that("get_years_from_to handles invalid split_per_year", {
  data <- data.frame(
    Year = c(2000, 2002, 2004, 2006, 2008, 2010)
  )
  config <- list(
    year_column_name = "Year",
    split_per_year = "two"
  )

  expect_error(get_years_from_to(data, config), "split_per_year must be an integer value or NULL.")
})
