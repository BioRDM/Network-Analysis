#' @export
read_config <- function(config) {
  # Make column names match their R-friendly versions
  for (nm in c("author_column_name", "year_column_name", "edge_id", "node_name", "node_color")) {
    config[[nm]] <- make.names(config[[nm]])
  }

  default_config <- list(
    input_name = tools::file_path_sans_ext(basename(config$file_path)),
    output_path = "output",
    author_column_name = NULL,
    author_delimiter = NULL,
    year_column_name = NULL,
    edge_id = NULL,
    max_authors_per_paper = NULL,
    min_papers_per_author = NULL,
    directed = FALSE,
    from_year = NULL,
    to_year = NULL,
    split_per_year = NULL,
    node_properties_file_path = NULL,
    node_name = NULL,
    node_color = NULL,
    package_version = as.character(utils::packageVersion("NetworkAnalysis"))
  )

  # Merge the provided config with the default config
  utils::modifyList(default_config, config, keep.null = TRUE)
}

#' @export
read_metadata <- function(metadata) {
  default_metadata <- list(
    Author = "Not provided",
    Email = "Not provided",
    Data_description = "Not provided",
    Data_access_date = "Not provided",
    Data_source = "Not provided",
    Data_source_url = ""
  )

  metadata <- utils::modifyList(default_metadata, metadata, keep.null = TRUE)
  metadata$Data_source <- paste0("\\href{", metadata$Data_source_url, "}", " {", metadata$Data_source, "}")
  metadata$Data_source_url <- NULL
  names(metadata) <- gsub("_", " ", names(metadata))

  metadata
}
