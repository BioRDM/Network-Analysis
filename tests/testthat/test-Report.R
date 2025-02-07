folder <- tempdir()

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
  save(report, target_file)
  expect_true(file.exists(target_file))
})

test_that("report is exported as pdf", {
  report <- Report()
  source_report <- paste0(folder, "/Report.md")
  target_file <- paste0(folder, "/Report.pdf")
  export_pdf(report, source_report, output_file = target_file)
  expect_true(file.exists(target_file))
  expect_false(file.exists(paste0(folder, "tmp_report.md")))
})
