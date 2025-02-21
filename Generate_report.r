library(devtools)

load_all()

# Load data
graph_path <- "E:\\Daniel\\NetworkAnalysis\\Pure Data\\Scopus 2023.csv"
institution <- Interactions(file_path = graph_path,
                            author_delimiter = ";",
                            csv_column_name = "Authors",
                            max_authors_per_paper = 50,
                            min_papers_per_author = 5,
                            directed = FALSE)

# Initiate report
report <- Report()

# Add graph Figure
report <- add_figure(
  report,
  plot = plot_graph(institution),
  fig_caption = "Visualisation of the Author network"
)

# Add network type paragraph
report <- add(report, network_type(institution))

# Add cohesion metrics
report <- add(report, cohesion_metrics(institution))

# Add density and transitivity info
report <- add(report, density_transitivity(institution))

# Add centrality metrics
report <- add(report, centrality_metrics(institution))

# Add top authors Figure
report <- add_figure(
  report,
  plot = plot_top_authors(institution, n = 15),
  fig_caption = "Direct connections between the 15 most central authors"
)

# Save report as markdown
save(report, "output/Report.md")

# Convert the report to pdf
export_pdf(report, "Report.md", output_file = "output/Report.pdf")
