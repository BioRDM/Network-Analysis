test_that("get_authors_per_paper gives correct result", {
  data <- data.frame(Project_ID = c(1, 1, 2, 3, 3, 3),
                     Investigator = c("Author1", "Author2", "Author3", "Author4", "Author5", "Author6"))
  result <- get_authors_per_paper(data, edge_id = "Project_ID")

  expect_equal(result, list(
    average = 2,
    median = 2,
    min = 1,
    max = 3
  ))
})

test_that("compute_metrics calculates correct metrics", {
  network <- list(
    raw = data.frame(Project_ID = c(1, 1, 2, 3, 3, 3),
                     Investigator = c("Author1", "Author2", "Author3", "Author4", "Author5", "Author6"),
                     Academic.Year = c(2019, 2019, 2020, 2020, 2020, 2020)),
    filtered = data.frame(Project_ID = c(2, 3, 3, 3),
                          Investigator = c("Author3", "Author4", "Author5", "Author6"),
                          Academic.Year = c(2020, 2020, 2020, 2020)),
    year_column = "Academic.Year",
    vertex_column = "Investigator",
    edge_id = "Project_ID"
  )

  metrics <- compute_metrics(network, raw = TRUE)
  expect_equal(metrics$Start_year, 2019)
  expect_equal(metrics$End_year, 2020)
  expect_equal(metrics$Total_Papers, 3)
  expect_equal(metrics$Total_Authors, 6)

  metrics <- compute_metrics(network, raw = FALSE)
  expect_equal(metrics$Start_year, 2020)
  expect_equal(metrics$End_year, 2020)
  expect_equal(metrics$Total_Papers, 2)
  expect_equal(metrics$Total_Authors, 4)
})

test_that("get_summary_stats returns correct summary", {
  network <- list(
    raw = data.frame(Project_ID = c(1, 1, 2, 3, 3, 3),
                     Investigator = c("Author1", "Author2", "Author3", "Author4", "Author5", "Author6"),
                     Academic.Year = c(2019, 2019, 2020, 2020, 2020, 2020)),
    filtered = data.frame(Project_ID = c(2, 3, 3, 3),
                          Investigator = c("Author3", "Author4", "Author5", "Author6"),
                          Academic.Year = c(2020, 2020, 2020, 2020)),
    year_column = "Academic.Year",
    vertex_column = "Investigator",
    edge_id = "Project_ID"
  )

  graph <- NULL # Assuming no graph is provided for this test

  summary_stats <- get_summary_stats(network, graph)

  expect_equal(summary_stats$Start_year, c(2019, 2020))
  expect_equal(summary_stats$End_year, c(2020, 2020))
  expect_equal(summary_stats$Total_Papers, c(3, 2))
  expect_equal(summary_stats$Total_Authors, c(6, 4))
})
