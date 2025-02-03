library(igraph)
library(intergraph)
library(statnet)

source("src/Report_class.r")
source("src/Report_text.r")
source("src/Interactions_class.r")

# Load data
graph_path <- "E:\\Daniel\\Network-Analysis\\data\\SBS_Direct_Andrew_Data.net"
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

# Save report
save(report, "Report.md")
