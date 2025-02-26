test_that("check_file_format works correctly", {
  expect_error(check_file_format("data.txt"), "Only CSV files are supported.")
  expect_error(check_file_format("data.csv"), NA)
})

test_that("check_split_per_year works correctly", {
  expect_error(check_split_per_year("two"), "split_per_year must be an integer value or NULL.")
  expect_error(check_split_per_year(2), NA)
  expect_error(check_split_per_year(NULL), NA)
})

test_that("check_author_column works correctly", {
  data <- data.frame(
    Author = c("Author1", "Author2"),
    Year = c(2000, 2001)
  )
  expect_error(check_author_column(data, "Author Name"), "Column name Author Name not found in the dataset.")
  expect_error(check_author_column(data, "Author"), NA)
})

test_that("check_year_column works correctly", {
  data <- data.frame(
    Author = c("Author1", "Author2"),
    Year = c(2000, 2001)
  )
  expect_error(check_year_column(data, "Year Published"), "Column name Year Published not found in the dataset.")
  expect_error(check_year_column(data, "Year"), NA)
})

test_that("check_year_filter works correctly", {
  expect_error(check_year_filter("2000", 2001), "from_year must be a numeric value or NULL.")
  expect_error(check_year_filter(2000, "2001"), "to_year must be a numeric value or NULL.")
  expect_error(check_year_filter(2002, 2001), "from_year must be less than or equal to to_year.")
  expect_error(check_year_filter(2000, 2001), NA)
  expect_error(check_year_filter(NULL, 2001), NA)
  expect_error(check_year_filter(2000, NULL), NA)
  expect_error(check_year_filter(NULL, NULL), NA)
})
