devtools::load_all()

# Report metadata
# Additional fields can be added to the list, and will be displayed in the Introduction section of the report.
metadata <- list(
  Author = "Daniel Thedie",
  Email = "daniel.thedie@ed.ac.uk",
  Data_description = "Grant applications submitted between 2019 and 2024 at the School of Biological Sciences, University of Edinburgh.",
  Data_access_date = "3rd April 2025",
  Data_source = "ERO BiSuite",
  Data_source_url = "https://research-office.ed.ac.uk/"
)

# Configuration list for the analysis
config <- list(
  file_path = "/media/Store/Daniel/Network_Analysis/Data/Grant_application_data/SBS_Research_Applications_RAWdata2019-24.csv",
  output_path = "/media/Store/Daniel/Network_Analysis/Outputs",
  author_delimiter = NULL,
  author_column_name = "Investigator",
  edge_id = "Project_ID",
  year_column_name = "Academic.Year",
  max_authors_per_paper = NULL,
  min_papers_per_author = NULL,
  from_year = NULL,
  to_year = NULL,
  split_per_year = 2
)

# Run the analysis and generate the report using the provided configuration.
assemble_report(config, metadata)
