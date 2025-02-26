library(devtools)

load_all()

config <- list(
  file_path = "data/SynthSysFinal_Direct_v2.csv",
  output_path = "output",
  author_delimiter = ";",
  author_column_name = "Author",
  year_column_name = "Publication Year",
  max_authors_per_paper = 50,
  min_papers_per_author = 5,
  directed = FALSE,
  from_year = 2008,
  to_year = 2011,
  split_per_year = NULL
)

assemble_report(config)
