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