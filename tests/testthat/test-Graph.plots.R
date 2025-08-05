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

test_that("add_legend function adds a legend to the plot", {
  graph <- graph(data = sample_data, directed = FALSE) |>
    build(vertices = "Author", edges = "Project_ID") |>
    set_communities()
  p <- plot(graph)
  p_with_legend <- p + add_legend(graph$graph, vertex_color = "community")
  expect_s3_class(p_with_legend, "gg")
})

test_that("set_vertex_color sets vertex colors correctly", {
  graph <- graph(data = sample_data, directed = FALSE) |>
    build(vertices = "Author", edges = "Project_ID") |>
    set_communities()
  graph <- set_vertex_color(graph, "community")
  expect_equal(igraph::V(graph$graph)$color, c("#1E90FFCC", "#008000CC", "#1E90FFCC", "#008000CC"))
})

test_that("set_vertex_size sets vertex sizes correctly", {
  graph <- graph(data = sample_data, directed = FALSE) |>
    build(vertices = "Author", edges = "Project_ID") |>
    set_communities()
  graph <- set_vertex_size(graph, "weight")
  expect_true(all(igraph::V(graph$graph)$size > 0))
})

test_that("set_edge_color sets edge colors correctly", {
  graph <- graph(data = sample_data, directed = FALSE) |>
    build(vertices = "Author", edges = "Project_ID", edge_attr = "Success")
  graph <- set_edge_color(graph, "Success")
  expect_equal(igraph::E(graph$graph)$color, c("#1E90FFCC", "#008000CC", "#1E90FFCC"))
})

test_that("set_edge_width sets edge widths correctly", {
  graph <- graph(data = sample_data, directed = FALSE) |>
    build(vertices = "Author", edges = "Project_ID")
  graph <- set_edge_width(graph, "weight")
  expect_true(all(igraph::E(graph$graph)$width > 0))
})

test_that("get_circle_layout returns a layout for circular plotting", {
  graph <- graph(data = sample_data, directed = FALSE) |>
    build(vertices = "Author", edges = "Project_ID")
  layout <- get_circle_layout(graph$graph)
  expect_s3_class(layout, "data.frame")
  expect_equal(ncol(layout), 6)
})
