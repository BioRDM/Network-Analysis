.default_config <- "metadata:
  Author: Name of the report author
  Email: Email address for the report author
  Data description: Main data file description
  Data access_date: 01-01-2020
  Data source:
  Data source_url:

data:
  file_path: /path/to/SBS_2019-2024_2025-03-24.csv
  output_path: /path/to/Outputs
  skip_report: FALSE
  skip_plots: FALSE
  node_id: Name
  node_delimiter:
  edge_id: Title
  year_column: Earliest published date
  filters:  # list
    - Type == 'Article'
  max_authors_per_paper:
  min_papers_per_author:
  from_year: 2019
  to_year: 2024
  split_per_year:

plot:
  layout: centrality  # centrality or auto

node_properties:
  file_path: /path/to/SBS_BPS_affiliations_tidy.csv
  filters:  # list
  remove_NA: FALSE
  node_id: Full.name
  color: Department
  order:  # list
  palette:  # list
"

#' Generate a config file for the Network Analysis pipeline
#'
#' @param path The path where the config file will be created
#' @returns None (writes a .yaml config file to the specified path)
#' @export
generate_config <- function(path = ".") {
  # Add check that provided path exists
  if (dir.exists(path)) {
    out_file <- file.path(path, "SNA_config.yaml")
  } else {
    parent_dir <- dirname(path)

    if (!dir.exists(parent_dir)) {
      cli::cli_abort(c("x" = "Input path does not exist: parent directory is missing."))
    }

    out_file <- path
  }

  writeLines(.default_config, out_file)
}
