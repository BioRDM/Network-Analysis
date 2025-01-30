library(igraph)
library(statnet)

load_graph <- function(file_path) {
    institution.graph <- read.graph(file_path, format="pajek")
    detach(package:igraph)
    library(statnet)
    library(igraph)
    return(institution.graph)
}