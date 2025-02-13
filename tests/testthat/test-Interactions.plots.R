folder <- tempdir()
file_path <- test_path("../data/SynthSysFinal_Direct_v2.csv")
institution <- Interactions(file_path = file_path)

test_that("graph plot is created", {
  test_file <- paste0(folder, "graph.png")
  plot_graph(institution, output_file = test_file)
  expect_true(file.exists(test_file))
})

test_that("top authors plot is created", {
  test_file <- paste0(folder, "top_authors.png")
  plot_top_authors(institution, n = 10, output_file = test_file)
  expect_true(file.exists(test_file))
})
