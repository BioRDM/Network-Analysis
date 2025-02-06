#' @export
introduction <- function() {
  text <- "
# Introduction
This report presents an analysis of a network dataset, focusing on key structural characteristics, group collaboration dynamics, and interdisciplinary."
  return(text)
}

#' @export
network_type <- function(interactions) {
  interactions <- get_network_description(interactions)
  text <- "\n# Network Description
## Network Type"
  if (interactions$directed == TRUE) {
    text <- paste0(text, "\n- **Directed**: The network is directed, meaning ... TO BE FILLED.")
  } else if (interactions$directed == FALSE) {
    text <- paste0(text, "\n- **Undirected**: The network is undirected, meaning all connections are mutual. This is common in co-authorship or collaboration networks. In an undirected network, connections do not have a direction (e.g., if A is connected to B, then B might also be connected to A by definition).")
  } else  {
    text <- paste0(text, "\n- **Unknown directionnality**: This could be due to an error in the data or the data is not available.")
  }
  if (interactions$weighted == TRUE) {
    text <- paste0(text, "\n- **Weighted**: The network is weighted, meaning that connections between the nodes (authors) vary in strength or intensity. This result indicates that the network has weighted edges, meaning that the relationships between nodes are assigned numerical values. These weights could represent the strength or frequency of interactions, such as the number of co-authored papers.")
  } else if (interactions$weighted == FALSE) {
    text <- paste0(text, "\n- **Unweighted**: The network is unweighted, meaning ... TO BE FILLED.")
  } else {
    text <- paste0(text, "\n- **Unknown weighting**: This could be due to an error in the data or the data is not available.")
  }
  return(text)
}

#' @export
cohesion_metrics <- function(interactions) {
  interactions <- get_cohesion(interactions)
  text <- paste0("\n## Cohesion Metrics",
                 "\n - **Total Dyads**: Dyads, representing all possible pairs of nodes (authors connection), were calculated as (n*n-1) = ", interactions$dyadcount,
                 "\n - **Actual Edges**: The network contains ", interactions$edgecount, " actual connections (edges), showing the level of interconnectedness among nodes (authors).",
                 "\n - **Number of Nodes**: The network consists of ", interactions$size, " nodes (authors), representing the total entities analysed.")
  return(text)
}

#' @export
density_transitivity <- function(interactions) {
  density <- get_density(interactions)
  transitivity <- get_transitivity(interactions)
  text <- paste0("\n# Density and Transitivity",
                 "\n## Density",
                 "\nDensity measures the proportion of realised connections compared to all possible connections. It is calculated as:",
                 "\n\n`Density = Number of Edges/(Number of Possible Edges) = Actual Edges/[nx(n-1)/2]`",
                 "\n\n- **Value**: ", round(density * 100, digits = 2), "%",
                 "\n- **Interpretation**: TO BE DEFINED (what are the possible cases?)",
                 "\n\n## Transitivity",
                 "\nTransitivity, or the clustering coefficient, measures the tendency of nodes to form triangles (e.g., if A → B and B → C, then A → C). (In other words, if two nodes are connected to a common third node, they are also likely to be connected to each other). It is calculated as:",
                 "\n\n`Transitivity = Number of Triangles/Number of Connected Triplets`",
                 "\n\n- **Value**: ", round(transitivity * 100, digits = 0), "%",
                 "\n- **Interpretation**: TO BE DEFINED (what are the possible cases?)")
  return(text)
}

#' @export
centrality_metrics <- function(interactions) {
  centrality <- get_centrality(interactions)
  betweenness <- get_betweenness(interactions)
  closeness <- get_closeness(interactions)
  diameter <- get_diameter(interactions)
  text <- paste0("\n# Node Centrality and Centralization",
                 "\n## Degree Centrality",
                 "\nDegree centrality measures the number of direct connections each node has in the network. ",
                 "\n\n- **Min**: ", round(min(centrality)),
                 "\n- **Max**: ", round(max(centrality)),
                 "\n- **Mean**: ", round(mean(centrality)),
                 "\n- **Median**: ", round(median(centrality)),
                 "\n\n## Betweenness Centrality",
                 "\nBetweenness centrality quantifies how often a node acts as a bridge in the shortest paths between other nodes.",
                 "\n\n- **Min**: ", round(min(betweenness)),
                 "\n- **Max**: ", round(max(betweenness)),
                 "\n- **Mean**: ", round(mean(betweenness)),
                 "\n- **Median**: ", round(median(betweenness)),
                 "\n\n## Closeness Centrality",
                 "\nCloseness centrality measures how quickly a node can access other nodes in the network.",
                 "\n\n- **Min**: ", round(min(closeness), digits = 5),
                 "\n- **Max**: ", round(max(closeness), digits = 5),
                 "\n- **Mean**: ", round(mean(closeness), digits = 5),
                 "\n- **Median**: ", round(median(closeness), digits = 5),
                 "\n\n## Network diameter",
                 "\nThe network diameter is the maximum distance (in terms of edges or steps) required to connect any two nodes in the network through the shortest possible path. ",
                 "\n\n- In our network: **the diameter is ", diameter, "**, it indicates that the farthest two nodes in your co-authorship network can be linked by the shortest path of ", diameter, " steps. ",
                 "\n- In practical terms, if two authors in the network are the most distantly connected, they are separated by ", diameter, " intermediaries (co-authors).")
  return(text)
}

#' @export
community_detection <- function(interactions) {
  text <- paste0("\n# Community (cohesive subgroups) Detection",
                 "\n## Strong and Weak Connectivity",
                 "\n\n- **Strongly connected**: ")
  return(text)
}