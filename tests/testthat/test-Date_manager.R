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
    from_year = 2002,
    to_year = 2006,
    split_per_year = NULL
  )

  result <- get_years_from_to(data, config)
  expect_equal(result$years_from, 2002)
  expect_equal(result$years_to, 2006)
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
  expect_equal(result$years_from, 2000)
  expect_equal(result$years_to, 2010)
})

test_that("year parser gets years", {
  data <- data.frame(
    dates = c("2000-01-01", "02/03/2004", "March 5, 2006", "2008.07.09", "2010"),
    num_dates = c(2000, 2004, 2006, 2008, 2010)
  )

  expect_equal(year_parser(data, year_column_name = "dates")$dates, data$num_dates)
  expect_equal(year_parser(data, year_column_name = "num_dates")$num_dates, data$num_dates)
})
