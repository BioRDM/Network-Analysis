test_that("format_names adds spaces correctly", {
  # Multiple initials
  input_names <- c("SmithJ", "DoeJD", "BrownABC", "McDonaldJH", "O'ReillyJ")
  expected_output <- c("Smith J.", "Doe J. D.", "Brown A. B. C.", "McDonald J. H.", "O'Reilly J.")
  expect_true(all(format_names(input_names) == expected_output))

  # No initials
  input_names <- c("Johnson", "Williams", "Jones")
  expected_output <- c("Johnson", "Williams", "Jones")
  expect_true(all(format_names(input_names) == expected_output))
})

test_that("format_summary_stats works correctly", {
  # Create a sample summary_stats data frame
  summary_stats <- data.frame(
    Start_year = c(2000, 2002),
    End_year = c(2001, 2003),
    Total_Papers = c(10, 15),
    Total_Authors = c(5, 7),
    Average_Authors_per_Paper = c(2, 2.5),
    Density = c(0.5, 0.6),
    Transitivity = c(0.3, 0.4),
    Mean_shortest_path = c(2.5, 2.8),
    Number_of_cutpoints = c(1, 2)
  )

  # Format the summary_stats
  formatted_stats <- format_summary_stats(summary_stats)

  # Check the structure of the formatted_stats
  expect_equal(ncol(formatted_stats), 10)
  expect_true(all(c("Dates", "Papers", "Authors", "Density (%)", "Transitivity (%)", "Shortest Path", "Cutpoints") %in% colnames(formatted_stats)))

  # Check the values of the formatted_stats
  expect_equal(formatted_stats$Dates, c("2000-2001", "2002-2003"))
  expect_equal(formatted_stats$Papers, c(10, 15))
  expect_equal(formatted_stats$Authors, c(5, 7))
  expect_equal(formatted_stats$`Density (%)`, c(50, 60))
  expect_equal(formatted_stats$`Transitivity (%)`, c(30, 40))
  expect_equal(formatted_stats$`Shortest Path`, c(2.5, 2.8))
  expect_equal(formatted_stats$Cutpoints, c(1, 2))
})
