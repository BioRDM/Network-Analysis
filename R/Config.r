#' @export
read_config <- function(config_path) {

  config <- yaml::read_yaml(config_path)

  # Make column names match their R-friendly versions
  for (nm in c("node_id", "year_column", "edge_id")) {
    config$data[[nm]] <- make.names(config$data[[nm]])
  }
  for (nm in c("node_id", "color")) {
    config$node_properties[[nm]] <- make.names(config$node_properties[[nm]])
  }

  default_config <- list(
    .meta = list(
      input_name = tools::file_path_sans_ext(basename(config$data$file_path)),
      package_version = as.character(utils::packageVersion("NetworkAnalysis"))
    ),
    metadata = list(
      Author = "Not provided",
      Email = "Not provided",
      "Data description" = "Not provided",
      "Data access date" = "Not provided",
      "Data source" = "Not provided",
      "Data source url" = ""
    ),
    data = list(
      output_path = "output",
      output_suffix = NULL,
      filters = NULL,
      node_id = NULL,
      node_delimiter = NULL,
      year_column = NULL,
      edge_id = NULL,
      max_authors_per_paper = NULL,
      min_papers_per_author = NULL,
      directed = FALSE,
      from_year = NULL,
      to_year = NULL,
      split_per_year = NULL
    ),
    plot = list(
      layout = "centrality"
    ),
    node_properties = list(
      file_path = NULL,
      filters = NULL,
      remove_NA = FALSE,
      node_id = NULL,
      color = NULL,
      order = NULL,
      palette = NULL
    )
  )

  # Merge the provided config with the default config
  config <- utils::modifyList(default_config, config, keep.null = TRUE)

  config$metadata[["Data source"]] <- paste0("\\href{", config$metadata[["Data source url"]], "}", "{", config$metadata[["Data source"]], "}")
  config$metadata[["Data source url"]] <- NULL
  config
}
