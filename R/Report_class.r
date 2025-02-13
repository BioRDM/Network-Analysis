library(rmarkdown)

#' @export
Report <- function() {
  report <- list(text = introduction(), fig_counter = 1)

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
add_figure.Report <- function(report, plot, fig_caption = "") {
  report$text <- paste0(report$text, "\n\n",
                        "![](", plot, ")", "\n",
                        "**Figure ", report$fig_counter, ":** ", fig_caption, "\n")
  report$fig_counter <- report$fig_counter + 1
  return(report)
}

add_figure <- function(report, plot, fig_caption) {
  UseMethod("add_figure", report)
}

#' @export
save.Report <- function(report, filename) {
  write(report$text, filename)
}

save <- function(report, filename) {
  UseMethod("save", report)
}

#' @export
export_pdf.Report <- function(report, report_path = "output/Report.md", output_file = "Report.pdf") {
  header <- "---
title: 'Network Analysis Report'
date: \\today
output:
  pdf_document:
    latex_engine: xelatex
    extra_dependencies: ['fontspec']
fontsize: 12pt
mainfont: 'DejaVu Sans'
toc: TRUE
---
"
  write(header, "tmp_report.md")
  write(report$text, "tmp_report.md", append = TRUE)
  rmarkdown::render("tmp_report.md", output_file = output_file, output_format = "pdf_document")
  file.remove("tmp_report.md")
}

export_pdf <- function(report, report_path, output_file) {
  UseMethod("export_pdf", report)
}
