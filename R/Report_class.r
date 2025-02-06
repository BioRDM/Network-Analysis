library(rmarkdown)

#' @export
Report <- function() {
  report <- list(text = introduction())

  # Assign the class name
  class(report) <- "Report"

  return(report)
}

# Define a print method for the class
#' @export
print.Report <- function(report) {
  print(report$text)
}

#' @export
add.Report <- function(report, section) {
  report$text <- paste(report$text, section, sep = "\n")
  return(report)
}

add <- function(report, section) {
  UseMethod("add", report)
}

#' @export
save.Report <- function(report, filename) {
  write(report$text, filename)
}

save <- function(report, filename) {
  UseMethod("save", report)
}

#' @export
export_pdf.Report <- function(report, filename) {
  rmarkdown::render(report$text, output_format = "pdf_document")
}

export_pdf <- function(report, filename) {
  UseMethod("export_pdf", report)
}
