library(devtools)

load_all()

config <- list(
  file_path = "Trial Analysis\\Pure_23_24_Code.csv",
  output_path = "output",
  author_delimiter = ";",
  author_column_name = "Author",
  year_column_name = "Year",
  max_authors_per_paper = 50,
  min_papers_per_author = 5,
  directed = FALSE,
  from_year = NULL,
  to_year = NULL,
  split_per_year = NULL
)

assemble_report(config)
