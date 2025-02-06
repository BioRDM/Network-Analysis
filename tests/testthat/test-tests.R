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
