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
save_md.Report <- function(report, filename) {
  write(report$text, filename)
}

save_md <- function(report, filename) {
  UseMethod("save_md", report)
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
toc: TRUE
---
"
  write(header, "tmp_report.md")
  write("\\pagebreak", "tmp_report.md", append = TRUE)
  write(report$text, "tmp_report.md", append = TRUE)
  print("Exporting PDF...")
  rmarkdown::render("tmp_report.md", output_file = output_file, output_format = "pdf_document", quiet = TRUE)
  file.remove("tmp_report.md")
  print("PDF exported successfully!")
}

export_pdf <- function(report, report_path, output_file) {
  UseMethod("export_pdf", report)
}
