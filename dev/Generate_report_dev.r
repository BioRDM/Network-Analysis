devtools::load_all()

# Report metadata
# Additional fields can be added to the list, and will be displayed in the Introduction section of the report.
metadata <- list(
  Author = "Daniel Thedie",
  Email = "daniel.thedie@ed.ac.uk",
  Data_description = "SBS publications from 2019 to 2024",
  Data_access_date = "24-09-2025",
  Data_source = "Pure",
  Data_source_url = ""
)

# Configuration list for the analysis
config <- list(
  file_path = "/media/Store/Daniel/Data_sharing_evaluation/Data/SBS_Pure/SBS_2019-2024_2025-09-24.csv",
  output_path = "/media/Store/Daniel/Data_sharing_evaluation/Outputs",
  author_delimiter = ";",
  author_column_name = "Internal_Author",
  edge_id = "Title",
  year_column_name = "Publication_date",
  max_authors_per_paper = 40,
  min_papers_per_author = NULL,
  from_year = 2019,
  to_year = 2024,
  split_per_year = NULL,
  node_properties_file_path = "/media/Store/Daniel/Data_sharing_evaluation/Data/SBS_affiliations/SBS_affiliations_tidy_2.csv",
  node_name = "Name",
  node_color = "Organisational.unit",
  node_order = c(
    "Institute of Quantitative Biology, Biochemistry and Biotechnology",
    "Institute of Ecology and Evolution",
    "Institute of Cell Biology",
    "Institute of Molecular Plant Sciences",
    "Institute for Immunology and Infection Research",
    "Institute for Stem Cell Research",
    "Institute of Evolutionary Biology",
    "SBS"
  ),
  node_palette = c(
    "blue",
    "green",
    "purple",
    "forestgreen",
    "red",
    "pink",
    "orange",
    "grey"
  )
)

# Run the analysis and generate the report using the provided configuration.
assemble_report(config, metadata)
