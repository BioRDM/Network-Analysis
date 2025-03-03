create_output_paths <- function(config) {
  output_path <- paste0(config$output_path, "/", tools::file_path_sans_ext(basename(config$file_path)))
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  figures_path <- paste0(output_path, "/figures")
  if (!dir.exists(figures_path)) {
    dir.create(figures_path, recursive = TRUE)
  }
  return(list(output = output_path, figures = figures_path))
}