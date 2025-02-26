folder <- tempdir()
sample_data <- data.frame(
  Author = c(
    "Author1;Author2;Author3",
    "Author2;Author3;Author4",
    "Author1;Author4",
    "Author5"
  ),
  stringsAsFactors = FALSE
)
interactions <- Interactions(data = sample_data)

test_that("report is created", {
  report <- Report()
  expect_equal(class(report), "Report")
})

test_that("text is added to report", {
  report <- Report()
  intro <- report$text
  new_text <- "\nNew line of content"
  report <- add(report, new_text)
  expect_equal(report$text, paste(intro, new_text, sep = "\n"))
})

test_that("report is saved as md", {
  report <- Report()
  target_file <- paste0(folder, "/Report.md")
  save_md(report, target_file)
  expect_true(file.exists(target_file))
})

test_that("report is exported as pdf", {
  report <- Report()
  source_report <- paste0(folder, "/Report.md")
  target_file <- paste0(folder, "/Report.pdf")
  save_md(report, source_report)
  export_pdf(report, source_report, output_file = target_file)
  expect_true(file.exists(target_file))
  expect_false(file.exists("tmp_report.md"))
})

test_that("plot is added to the report", {
  report <- Report()
  intro <- report$text
  report <- add_figure(report, plot = plot_graph(interactions, output_file = "graph.png"))
  file.remove("graph.png")
  expect_equal(report$text, paste(paste0(intro, "\n\n",
                                         "![](graph.png)", "\n",
                                         "**Figure 1:** \n"), sep = "\n"))
})