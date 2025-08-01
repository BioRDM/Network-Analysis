sample_data <- data.frame(
  Author = c("Author1", "Author2", "Author1", "Author3", "Author2", "Author4"),
  Year = c(2000, 2000, 2006, 2006, 2010, 2010),
  Project_ID = c("P1", "P1", "P2", "P2", "P3", "P3"),
  Success = c("Successful", "Successful", "Unsuccessful", "Unsuccessful", "Successful", "Successful"),
  stringsAsFactors = FALSE
)

test_that("plot function works correctly", {
  graph <- graph(data = sample_data, directed = FALSE) |>
    build(vertices = "Author", edges = "Project_ID")
  expect_s3_class(plot(graph), "gg")
})

test_that("plot_cutpoints function works correctly", {
  graph <- graph(data = sample_data, directed = FALSE) |>
    build(vertices = "Author", edges = "Project_ID")
  expect_s3_class(plot_cutpoints(graph), "gg")
})

test_that("plot_top_vertices function works correctly", {
  graph <- graph(data = sample_data, directed = FALSE) |>
    build(vertices = "Author", edges = "Project_ID") |>
    set_communities()
  expect_s3_class(plot_top_vertices(graph, n = 10), "gg")
})

test_that("get_palette function returns a list of colours of the right size", {
  graph <- graph(data = sample_data, directed = FALSE) |>
    build(vertices = "Author", edges = "Project_ID") |>
    set_communities()
  palette <- get_palette(graph, vertex_attr = "community", alpha = 0.6)
  expect_type(palette, "character")
  expect_length(palette, length(unique(get_communities(graph))))
})
