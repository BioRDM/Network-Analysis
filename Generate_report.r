<<<<<<< HEAD

# library("rmarkdown")
=======
library(igraph)
library(intergraph)
library(statnet)
>>>>>>> 3bab69e29d63a8e1997e5b264d97a9d0d144eb9a

source("src/Report_class.r")
source("src/Report_text.r")
source("src/Graph_class.r")

# Load data
graph_path <- "E:\\Daniel\\Network-Analysis\\data\\SBS_Direct_Andrew_Data.net"
institution <- Graph(file_path = graph_path)

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
