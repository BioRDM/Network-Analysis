source("src/Report_text.r")

Report <- function() {
  report <- list(text = introduction())

  # Assign the class name
  class(report) <- "Report"

  return(report)
}

# Define a print method for the class
print.Report <- function(report) {
  print(report$text)
}

add.Report <- function(report, section) {
  report$text <- paste(report$text, section, sep = "\n")
  return(report)
}

add <- function(report, section) {
  UseMethod("add", report)
}

save.Report <- function(report, filename) {
  write(report$text, filename)
}

save <- function(report, filename) {
  UseMethod("save", report)
}