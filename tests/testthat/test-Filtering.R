sample_data <- data.frame(
  Author = c(
    "Author1;Author2;Author3",
    "Author2;Author3;Author4",
    "Author1;Author4",
    "Author5"
  ),
  stringsAsFactors = FALSE
)

test_that("filter_by_year works correctly", {
  data <- data.frame(
    Author = c("Author1", "Author2", "Author3"),
    Year = c(2000, 2002, 2004)
  )

  # Test filtering with both from_year and to_year
  filtered_data <- filter_by_year(data, "Year", 2001, 2003)
  expect_equal(nrow(filtered_data), 1)
  expect_equal(filtered_data$Year, 2002)

  # Test filtering with only from_year
  filtered_data <- filter_by_year(data, "Year", 2001, NULL)
  expect_equal(nrow(filtered_data), 2)
  expect_equal(filtered_data$Year, c(2002, 2004))

  # Test filtering with only to_year
  filtered_data <- filter_by_year(data, "Year", NULL, 2001)
  expect_equal(nrow(filtered_data), 1)
  expect_equal(filtered_data$Year, 2000)

  # Test filtering with neither from_year nor to_year
  filtered_data <- filter_by_year(data, "Year", NULL, NULL)
  expect_equal(nrow(filtered_data), 3)
  expect_equal(filtered_data$Year, c(2000, 2002, 2004))
})

test_that("papers are removed", {
  res <- filter_papers_by_authors(sample_data, column_name = "Author", delimiter = ";", max_authors = 2)
  expect_equal(nrow(res[[1]]), 1)
  expect_equal(res[[2]], 2)
  expect_equal(res[[3]], 1)
})

test_that("filter_infrequent_authors removes authors with fewer than min_occurrences", {
  # Filter authors that appear fewer than 2 times
  filtered_data <- filter_infrequent_authors(sample_data, min_occurrences = 2)[[1]]
  expect_equal(nrow(filtered_data), 3)
  expect_true(all(grepl("Author1|Author2|Author3|Author4", filtered_data$Author)))
  expect_false(any(grepl("Author5", filtered_data$Author)))

  # Filter authors that appear fewer than 3 times
  filtered_data <- filter_infrequent_authors(sample_data, min_occurrences = 3)[[1]]
  expect_equal(nrow(filtered_data), 0)
  expect_false(any(grepl("Author1|Author2|Author3|Author4|Author5", filtered_data$Author)))

  # Filter authors that appear fewer than 1 time (should return all data)
  filtered_data <- filter_infrequent_authors(sample_data, min_occurrences = 1)[[1]]
  expect_equal(nrow(filtered_data), 4)
  expect_true(all(grepl("Author1|Author2|Author3|Author4|Author5", filtered_data$Author)))
})

test_that("filter_infrequent_authors handles empty data", {
  empty_data <- data.frame(Author = character(0), stringsAsFactors = FALSE)
  filtered_data <- filter_infrequent_authors(empty_data, min_occurrences = 2)[[1]]
  expect_equal(nrow(filtered_data), 0)
})

test_that("filter_infrequent_authors handles no authors meeting the criteria", {
  # Filter authors that appear fewer than 10 times (none should meet this criteria)
  filtered_data <- filter_infrequent_authors(sample_data, min_occurrences = 10)[[1]]
  expect_equal(nrow(filtered_data), 0)
})
