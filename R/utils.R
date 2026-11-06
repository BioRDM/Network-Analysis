.ensure_tinytex <- function() {
  if (!tinytex::is_tinytex()) {
    message("TinyTeX not found. Installing now...")
    tinytex::install_tinytex()

    if (!tinytex::is_tinytex()) {
      cli::cli_abort(c("x" = "Failed to install TinyTeX.",
                       "i" = "Please run 'tinytex::install_tinytex()' manually in your R console."))
    }
    message("TinyTeX installed successfully.")
  }
}
