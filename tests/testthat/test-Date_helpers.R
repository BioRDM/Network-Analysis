test_that("get_years_from_to works correctly with split_per_year", {
  data <- data.frame(
    Year = c(2000, 2002, 2004, 2006, 2008, 2010)
  )
  config <- list(
    year_column_name = "Year",
    split_per_year = 2
  )

  result <- get_years_from_to(data, config)
  expect_equal(result$from, c(2000, 2002, 2004, 2006, 2008, 2010))
  expect_equal(result$to, c(2001, 2003, 2005, 2007, 2009, 2010))
})

test_that("get_years_from_to works correctly without split_per_year", {
  data <- data.frame(
    Year = c(2000, 2002, 2004, 2006, 2008, 2010)
  )
  config <- list(
    year_column_name = "Year",
    from_year = 2002,
    to_year = 2006,
    split_per_year = NULL
  )

  result <- get_years_from_to(data, config)
  expect_equal(result$from, 2002)
  expect_equal(result$to, 2006)
})

test_that("get_years_from_to handles invalid split_per_year", {
  data <- data.frame(
    Year = c(2000, 2002, 2004, 2006, 2008, 2010)
  )
  config <- list(
    year_column_name = "Year",
    split_per_year = "two"
  )

  expect_error(get_years_from_to(data, config), "must be an integer value or NULL.")
})

test_that("get_years_from_to handles all-NULL input", {
  data <- data.frame(
    Year = c(2000, 2002, 2004, 2006, 2008, 2010)
  )
  config <- list(
    year_column_name = "Year",
    from_year = NULL,
    to_year = NULL,
    split_per_year = NULL
  )

  result <- get_years_from_to(data, config)
  expect_equal(result$from, 2000)
  expect_equal(result$to, 2010)
})

test_that("parse_year gets years", {
  dates <- c("2000-01-01", "02/03/2004", "March 5, 2006", "2008.07.09", "2010")
  num_dates <- c(2000, 2004, 2006, 2008, 2010)

  expect_equal(parse_year(dates), num_dates)
})
