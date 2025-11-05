test_that("check_split_per_year works correctly", {
  expect_error(check_split_per_year("two"), "must be an integer value or NULL.")
  expect_error(check_split_per_year(2), NA)
  expect_error(check_split_per_year(NULL), NA)
})

test_that("check_column works correctly", {
  data <- data.frame(
    Author = c("Author1", "Author2"),
    Year = c(2000, 2001)
  )
  expect_error(check_column(data, "Author Name"), "\"Author Name\" not found in the input table.")
  expect_error(check_column(data, "Author"), NA)
})

test_that("check_delimiter works correctly", {
  expect_error(check_delimiter(123), "must be a non-empty string.")
  expect_error(check_delimiter(""), "must be a non-empty string.")
  expect_error(check_delimiter(NULL), NA)
  expect_error(check_delimiter(","), NA)
})

test_that("check_year_range works correctly", {
  expect_error(check_year_range("2000", 2001), "must be a numeric value or NULL.")
  expect_error(check_year_range(2000, "2001"), "must be a numeric value or NULL.")
  expect_error(check_year_range(2002, 2001), "must be less than or equal to")
  expect_error(check_year_range(2000, 2001), NA)
  expect_error(check_year_range(NULL, 2001), NA)
  expect_error(check_year_range(2000, NULL), NA)
  expect_error(check_year_range(NULL, NULL), NA)
})

test_that("check_palette_length works correctly", {
  expect_error(check_palette_length(c("red", "blue"), c("A", "B", "C")), "length must match")
  expect_error(check_palette_length(c("red", "blue", "green"), c("A", "B", "C")), NA)
})
