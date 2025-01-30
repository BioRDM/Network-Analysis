library(igraph)
library(statnet)
library(intergraph)

source("src/Report_class.r")
source("src/Report_text.r")
source("src/Utilities.r")

# Initiate report
report <- Report()

institution.graph <- load_graph(file_path="E:\\Daniel\\Network-Analysis\\data\\SBS_Direct_Andrew_Data.net")

#Add network type paragraph
report <- add(report, network_type(directed = is.directed(institution.graph)))

# Save report
save(report, "test_report.md")




# print(is.directed(institution.graph))
# print(is.weighted(institution.graph))

