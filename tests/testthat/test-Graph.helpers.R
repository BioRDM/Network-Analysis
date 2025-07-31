test_that("get_edges works correctly", {
  data <- data.frame(
    Author = c("A", "B", "C", "A", "B", "C"),
    Project_ID = c(1, 1, 1, 2, 2, 2)
  )

  vertices <- "Author"
  edges <- "Project_ID"

  result <- get_edges(data, vertices, edges)

  expected <- tibble::tibble(
    from = c("A", "A", "B"),
    to = c("B", "C", "C"),
    weight = c(2, 2, 2)
  )

  expect_equal(result, expected)
})
