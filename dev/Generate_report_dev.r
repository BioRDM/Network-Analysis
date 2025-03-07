library(devtools)
load_all()

# Report metadata
# Additional fields can be added to the list, and will be displayed in the Introduction section of the report.
metadata <- list(
  Author = "Daniel Thedie",
  Email = "daniel.thedie@ed.ac.uk",
  Data_description = "Articles published in the School of Biological Sciences (SBS) at the University of Edinburgh between 2023 and 2024, downloaded from the Pure Research Information System (University of Edinburgh).",
  Data_access_date = "26th February 2025",
  Data_source = "Pure (University of Edinburgh)",
  Data_source_url = "https://library.ed.ac.uk/research-support/research-information-management/pure"
)

# Configuration list for the analysis
config <- list(
  #Copy and Paste the file path
  # File path to the CSV file containing the data.
  # - Use double backslashes (\\) or forward slashes (/) in the file path.
      file_path = "E:/Daniel/NetworkAnalysisData/Pure Data/Pure_23_24.csv", 
      
  # Output directory where the results (PDFs, figures, CSV files, etc.) will be saved.
  # - A folder named "output" will be created if it doesn't already exist.
      output_path = "output",
  
  # Delimiter used to separate author names in the "Author" column.
  # - For example, if authors are separated by semicolons, use ";".
      author_delimiter = ";",
  
  # Name of the column in the CSV file that contains the author names.
  # - Ensure the column name matches exactly, including capitalisation and pluralisation.
      author_column_name = "Authors",
 
  # Name of the column in the CSV file that contains the publication year.
  # - Ensure the column name matches exactly, including capitalisation and pluralisation.
      year_column_name = "Year",
  
  # Filter 1: Maximum number of authors per paper to include in the analysis.
  # - Papers with more than this number of authors will be excluded.
      max_authors_per_paper = 50,
  
  # Filter 2: Minimum number of papers an author must have to be included in the analysis.
  # - Authors with fewer than this number of papers will be excluded.
      min_papers_per_author = 5,
  
  # Timeframe for the analysis: specify the start and end years.
  # - Use `NULL` to include all the years in your csv.
  # - Example: `from_year = 2019, to_year = 2022` for papers published between 2019 and 2022.
      from_year = NULL,
      to_year = NULL,
  
  # Split the analysis into smaller time intervals (e.g., every 2, 3, or 5 years).
  # - Use `NULL` to avoid splitting the analysis.
  # - Example: `split_per_year = 2` to generate a separate report every 2 years.
      split_per_year = NULL
)

# Run the analysis and generate the report using the provided configuration.
assemble_report(config, metadata)
