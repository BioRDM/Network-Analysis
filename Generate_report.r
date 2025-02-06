library(devtools)

load_all()

# Load data
graph_path <- "E:\\Daniel\\NetworkAnalysis\\data\\SynthsysFinal_Direct_v2.csv"
institution <- Interactions(file_path = graph_path)

# Initiate report
report <- Report()

# Add network type paragraph
report <- add(report, network_type(institution))

# Add cohesion metrics
report <- add(report, cohesion_metrics(institution))

# Add density and transitivity info
report <- add(report, density_transitivity(institution))

# Add centrality metrics
report <- add(report, centrality_metrics(institution))

# Save report as markdown
save(report, "Report.md")

# Convert the report to pdf
export_pdf(report, "Report.md")
