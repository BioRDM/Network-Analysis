test_that("check_file_format works correctly", {
  expect_error(check_file_format("data.txt"), ".txt format is not supported.")
  expect_error(check_file_format("data.csv"), NA)
})

test_that("check_split_per_year works correctly", {
  expect_error(check_split_per_year("two"), "must be an integer value or NULL.")
  expect_error(check_split_per_year(2), NA)
  expect_error(check_split_per_year(NULL), NA)
})

test_that("check_author_column works correctly", {
  data <- data.frame(
    Author = c("Author1", "Author2"),
    Year = c(2000, 2001)
  )
  expect_error(check_author_column(data, "Author Name"), "\"Author Name\" not found in the input table.")
  expect_error(check_author_column(data, "Author"), NA)
})

test_that("check_year_column works correctly", {
  data <- data.frame(
    Author = c("Author1", "Author2"),
    Year = c(2000, 2001)
  )
  expect_error(check_year_column(data, "Year Published"), "\"Year Published\" not found in the input table.")
  expect_error(check_year_column(data, "Year"), NA)
})

test_that("check_year_filter works correctly", {
  expect_error(check_year_filter("2000", 2001), "must be a numeric value or NULL.")
  expect_error(check_year_filter(2000, "2001"), "must be a numeric value or NULL.")
  expect_error(check_year_filter(2002, 2001), "must be less than or equal to")
  expect_error(check_year_filter(2000, 2001), NA)
  expect_error(check_year_filter(NULL, 2001), NA)
  expect_error(check_year_filter(2000, NULL), NA)
  expect_error(check_year_filter(NULL, NULL), NA)
})
