test_that("get_author_stats calculates correct statistics", {
  data <- data.frame(Author = c("Author1;Author2", "Author1", "Author1;Author2;Author3"))
  result <- get_author_stats(data)

  expect_equal(result$sum, 3)
  expect_equal(result$average, 2)
  expect_equal(result$median, 2)
  expect_equal(result$min, 1)
  expect_equal(result$max, 3)
})
