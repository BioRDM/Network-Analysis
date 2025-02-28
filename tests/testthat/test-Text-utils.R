test_that("add_space_after_last_name adds spaces correctly", {
  # Multiple initials
  input_names <- c("SmithJ", "DoeJD", "BrownABC", "McDonaldJH", "O'ReillyJ")
  # expected_output <- c(" Smith J", " Doe J D", " Brown A B C")
  expected_output <- c("Smith J.", "Doe J. D.", "Brown A. B. C.", "McDonald J. H.", "O'Reilly J.")
  expect_true(all(add_space_after_last_name(input_names) == expected_output))
  # expect_true(all(add_space_after_last_name(input_names) %in% expected_output))

  # No initials
  input_names <- c("Johnson", "Williams", "Jones")
  expected_output <- c("Johnson", "Williams", "Jones")
  expect_true(all(add_space_after_last_name(input_names) == expected_output))
  # expect_true(all(add_space_after_last_name(input_names) %in% expected_output))
})