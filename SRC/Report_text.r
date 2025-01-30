source("src/Graph_class.r")

introduction <- function() {
  text <- "# Network analysis report\n
## Introduction
This report presents an analysis of a network dataset, focusing on key structural characteristics, group collaboration dynamics, and interdisciplinary."
  return(text)
}

network_type <- function(graph) {
  graph <- get_network_description(graph)
  text <- "\n## Network Description
### Network Type"
  if (graph$directed == TRUE) {
    text <- paste(text, "\n- **Directed**: The network is directed, meaning ... TO BE FILLED.", sep = "")
  } else if (graph$directed == FALSE) {
    text <- paste(text, "\n- **Undirected**: The network is undirected, meaning all connections are mutual. This is common in co-authorship or collaboration networks. In an undirected network, connections do not have a direction (e.g., if A is connected to B, then B might also be connected to A by definition).", sep = "")
  } else  {
    text <- paste(text, "\n- **Unknown directionnality**: This could be due to an error in the data or the data is not available.", sep = "")
  }
  if (graph$weighted == TRUE) {
    text <- paste(text, "\n- **Weighted**: The network is weighted, meaning that connections between the nodes (authors) vary in strength or intensity. This result indicates that the network has weighted edges, meaning that the relationships between nodes are assigned numerical values. These weights could represent the strength or frequency of interactions, such as the number of co-authored papers.", sep = "")
  } else if (graph$weighted == FALSE) {
    text <- paste(text, "\n- **Unweighted**: The network is unweighted, meaning ... TO BE FILLED.", sep = "")
  } else {
    text <- paste(text, "\n- **Unknown weighting**: This could be due to an error in the data or the data is not available.", sep = "")
  }
  return(text)
}

cohesion_metrics <- function(graph) {
  graph <- get_cohesion(graph)
  text <- paste("\n### Cohesion Metrics",
                "\n - **Total Dyads**: Dyads, representing all possible pairs of nodes (authors connection), were calculated as (n*n-1) = ", graph$dyadcount,
                "\n - **Actual Edges**: The network contains ", graph$edgecount, " actual connections (edges), showing the level of interconnectedness among nodes (authors).",
                "\n - **Number of Nodes**: The network consists of ", graph$size, " nodes (authors), representing the total entities analysed.", sep = "")
  return(text)
}

density_transitivity <- function(graph) {
  density <- get_density(graph)
  transitivity <- get_transitivity(graph)
  text <- paste("\n## Density and Transitivity",
                "\n### Density",
                "\nDensity measures the proportion of realised connections compared to all possible connections. It is calculated as:",
                "\n\n`Density = Number of Edges/(Number of Possible Edges) = Actual Edges/[nx(n-1)/2]`",
                "\n- **Value**: ", round(density * 100, digits = 2), "%",
                "\n- **Interpretation**: TO BE DEFINED (what are the possible cases?)",
                "\n\n### Transitivity",
                "\nTransitivity, or the clustering coefficient, measures the tendency of nodes to form triangles (e.g., if A → B and B → C, then A → C). (In other words, if two nodes are connected to a common third node, they are also likely to be connected to each other). It is calculated as:",
                "\n\n`Transitivity = Number of Triangles/Number of Connected Triplets`",
                "\n- **Value**: ", round(transitivity * 100, digits = 0), "%",
                "\n- **Interpretation**: TO BE DEFINED (what are the possible cases?)",
                sep = "")
  return(text)
}

centrality <- function(graph) {
  centrality <- get_centrality(graph)
  betweenness <- get_betweenness(graph)
  closeness <- get_closeness(graph)
  diameter <- get_diameter(graph)
  text <- paste("\n## Node Centrality and Centralization",
                "\n### Degree Centrality",
                "\nDegree centrality measures the number of direct connections each node has in the network. ",
                "\n- **Min**: ", round(min(centrality)),
                "\n- **Max**: ", round(max(centrality)),
                "\n- **Mean**: ", round(mean(centrality)),
                "\n- **Median**: ", round(median(centrality)),
                "\n\n### Betweenness Centrality",
                "\nBetweenness centrality quantifies how often a node acts as a bridge in the shortest paths between other nodes.",
                "\n- **Min**: ", round(min(betweenness)),
                "\n- **Max**: ", round(max(betweenness)),
                "\n- **Mean**: ", round(mean(betweenness)),
                "\n- **Median**: ", round(median(betweenness)),
                "\n\n### Closeness Centrality",
                "\nCloseness centrality measures how quickly a node can access other nodes in the network.",
                "\n- **Min**: ", round(min(closeness), digits = 5),
                "\n- **Max**: ", round(max(closeness), digits = 5),
                "\n- **Mean**: ", round(mean(closeness), digits = 5),
                "\n- **Median**: ", round(median(closeness), digits = 5),
                "\n\n### Network diameter",
                "\nThe network diameter is the maximum distance (in terms of edges or steps) required to connect any two nodes in the network through the shortest possible path. ",
                "\n- In our network: **the diameter is ", diameter, "**, it indicates that the farthest two nodes in your co-authorship network can be linked by the shortest path of ", diameter, " steps. ",
                "\n- In practical terms, if two authors in the network are the most distantly connected, they are separated by ", diameter, " intermediaries (co-authors).",
                sep = "")
  return(text)
}

community_detection <- function(graph) {
  text <- paste("\n## Community (cohesive subgroups) Detection",
                "\n### Strong and Weak Connectivity",
                "\n- **Strongly connected**: ",
                sep = "")
  return(text)
}