#' @export
introduction <- function() {
  text <- "
# Introduction
This report presents an analysis of a network dataset, focusing on key structural characteristics, group collaboration dynamics, and interdisciplinarity."
  return(text)
}

#' @export
data_filtering <- function(interactions, prefilter_author_stats, postfilter_author_stats) {
  text <- paste0("\n# Data filtering
  \nThe initial dataset contained ", interactions$n_papers+interactions$papers_removed, " papers and ", prefilter_author_stats$sum, " authors from ", interactions$from_year, " to ", interactions$to_year, ".\n")
  text <- paste0(text,
                 "\n## Before filtering",
                 "\n - Average authors per paper: ", round(prefilter_author_stats$average),
                 "\n - Median authors per paper: ", prefilter_author_stats$median,
                 "\n - Minimum authors per paper: ", prefilter_author_stats$min,
                 "\n - Maximum authors per paper: ", prefilter_author_stats$max)

  text <- paste0(text, "\n\n## Filtering")
  if (interactions$papers_removed > 0) {
    text <- paste0(text,
                   "\n", interactions$papers_removed, " papers were removed from the dataset due to having more than ", interactions$max_authors, " authors.\n")
  } else {
    text <- paste0(text,
                   "\nAll papers in the dataset had less than ", interactions$max_authors, " authors and were included in the network analysis.\n")
  }
  if (interactions$authors_removed > 0) {
    text <- paste0(text,
                   "\n", interactions$authors_removed, " authors were removed from the dataset because they appeared in less than ", interactions$min_papers, " papers.\n")
  } else {
    text <- paste0(text,
                   "\nAll authors in the dataset appeared in at least ", interactions$min_papers, " papers and were included in the network analysis.\n")
  }
  text <- paste0(text, "\nIn total, ", interactions$n_papers, " papers and ", postfilter_author_stats$sum, " authors were included in the network analysis.\n")

  text <- paste0(text,
                 "\n## After filtering",
                 "\n - Average authors per paper: ", round(postfilter_author_stats$average),
                 "\n - Median authors per paper: ", postfilter_author_stats$median,
                 "\n - Minimum authors per paper: ", postfilter_author_stats$min,
                 "\n - Maximum authors per paper: ", postfilter_author_stats$max)
}

#' @export
network_type <- function(interactions) {
  interactions$weighted <- igraph::is_weighted(interactions$graph)
  interactions <- get_cohesion(interactions)
  text <- "\n# Network Description"
  text <- paste0(text, "\n## Network Type")
  if (interactions$directed == TRUE) {
    text <- paste0(text, "\n - **Directed**: The network is directed, meaning edges have a direction (e.g., A → B but not necessarily B → A). Examples include citation networks or hierarchical structures.")
  } else if (interactions$directed == FALSE) {
    text <- paste0(text, "\n - **Undirected**: The network is undirected, meaning all connections are mutual. This is common in co-authorship or collaboration networks. In an undirected network, connections do not have a direction (e.g., if A is connected to B, then B might also be connected to A by definition).")
  } else  {
    text <- paste0(text, "\n - **Unknown directionnality**: This could be due to an error in the data or the data is not available.")
  }
  if (interactions$weighted == TRUE) {
    text <- paste0(text, "\n - **Weighted**: The network is weighted, meaning that connections between the nodes (authors) vary in strength or intensity. This result indicates that the network has weighted edges, meaning that the relationships between nodes are assigned numerical values. These weights could represent the strength or frequency of interactions, such as the number of co-authored papers.")
  } else if (interactions$weighted == FALSE) {
    text <- paste0(text, "\n - **Unweighted**: Edges are unweighted and represent only the presence or absence of connections.")
  } else {
    text <- paste0(text, "\n - **Unknown weighting**: This could be due to an error in the data or the data is not available.")
  }
  return(text)
}

#' @export
cohesion_metrics <- function(interactions) {
  interactions <- get_cohesion(interactions)
  text <- paste0("\n## Cohesion Metrics",
                 "\n - **Total Dyads**: Dyads, representing all possible pairs of nodes (authors connection), were calculated as (n*n-1) = ", interactions$dyadcount,
                 "\n - **Actual Edges**: The network contains ", interactions$edgecount, " actual connections (edges), showing the level of interconnectedness among nodes (authors).
                 This is the count of actual connections (edges) present in the network, indicating how interconnected the nodes are",
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
                 "\n- **Interpretation**: The value of Density is in percantage.
                 High Density: Close to 100%, meaning most nodes are connected. This indicates a highly cohesive network.
                 Low Density: low percantage, meaning most nodes are not connected. Sparse networks are common in real-world systems.",
                 "\n\n## Transitivity",
                 "\nTransitivity, or the clustering coefficient, measures the tendency of nodes to form triangles (e.g., if A → B and B → C, then A → C). (In other words, if two nodes are connected to a common third node, they are also likely to be connected to each other). It is calculated as:",
                 "\n\n`Transitivity = Number of Triangles/Number of Connected Triplets`",
                 "\n\n- **Value**: ", round(transitivity * 100, digits = 0), "%",
                 "\n- **Interpretation**: The value of Transitivity is in percantage.
                 High Transitivity: Close to 100%, indicating a high level of clustering. Nodes tend to form tight-knit groups.
                 Low Transitivity: low percantage, indicating a lack of clustering.
                 A higher Transitivity value suggests that network tend to collaborate in groups, forming cohesive communities.")
  return(text)
}

#' @export
centrality_metrics <- function(interactions) {
  centrality <- get_centrality(interactions)
  diameter <- get_diameter(interactions)
  text <- paste0("\n# Node Centrality and Centralisation",
                 "\n## Degree Centrality",
                 "\nDegree centrality measures the number of direct connections each node has in the network.
                 Nodes with higher degree centrality are likely to be influential or well-connected individuals/entities.",
                 "\n\n- **The least connected node has (Min)**: ", round(min(centrality$degree)),
                 "\n- **The most connected node has (Max)**: ", round(max(centrality$degree)),
                 "\n- **On average, nodes have about (Mean)**: ", round(mean(centrality$degree)),
                 "\n- **Half of the nodes have more connection than (Median)**: ", round(median(centrality$degree)),

                 "\n\n## Betweenness Centrality",
                 "\nBetweenness centrality quantifies how often a node acts as a bridge in the shortest paths between other nodes.
                 Nodes with high betweenness are critical connectors in the network. Removing them could significantly disrupt the flow of information.",
                 "\n\n- **Min**: ", round(min(centrality$betweenness)),
                 "\n- **Max**: ", round(max(centrality$betweenness)), "(the node with the highest betweenness value is a critical bridge in the network, controlling the flow of information).",
                 "\n- **Mean**: ", round(mean(centrality$betweenness)),
                 "\n- **Median**: ", round(median(centrality$betweenness)),

                 "\n\n## Harmonic Centrality",
                 "\nHarmonic centrality measures how close a node is to all other nodes in a network.
                 Nodes with high harmonic centrality are in advantageous positions to disseminate information quickly.",
                 "\n\n- **Min**: ", round(min(centrality$harmonic), digits = 5), "(average path for the least central node to reach other nodes).",
                 "\n- **Max**: ", round(max(centrality$harmonic), digits = 5),
                 "\n- **Mean**: ", round(mean(centrality$harmonic), digits = 5),
                 "\n- **Median**: ", round(median(centrality$harmonic), digits = 5),
                 "\n\n## Network diameter",
                 "\nThe network diameter is the maximum distance (in terms of edges or steps) required to connect any two nodes in the network through the shortest possible path. ",
                 "\n\n- In our network: **the diameter is ", diameter$diameter, "**, it indicates that the farthest two nodes in the co-authorship network can be linked in ", diameter$diameter, " steps at shortest.",
                 "\n- In practical terms, the two most distantly connected authors in the network are separated by ", diameter$diameter, " intermediates (co-authors).",
                 "\n- The average shortest path between two authors in the network is ", round(diameter$average_shortest_path, digits = 1), " steps.")
  return(text)
}

#' @export
reachability_metrics <- function(interactions) {
  unreachable_percentage <- get_reachability(interactions) * 100
  text <- paste0("\n# Reachability",
                 "\nReachability measures the proportion of pairs of nodes that are reachable in the network.",
                 "\n\n- **Unreachable Pairs**: ", round(unreachable_percentage), "% of all possible pairs of nodes are unreachable in the network.",
                 "\n- **Interpretation**: A high percentage of unreachable pairs may indicate a fragmented network or isolated nodes that are not connected to the main network.")
  return(text)
}

#' @export
cutpoint_authors <- function(interactions) {
  cutpoints <- get_cutpoints(interactions)
  cutpoint_names <- network::network.vertex.names(interactions$network)[which(cutpoints == TRUE)]
  if (length(cutpoint_names) == 0) {
    text <- "\n# Cutpoint Authors"
    text <- paste0(text,
                   "\n## Definition",
                   "\nCutpoint authors are nodes in the network whose removal would increase the number of connected components in the network.",
                   "\n\n## List of Cutpoint Authors",
                   "\nThere are no cutpoint authors in the network.")
  } else {
    cutpoint_names <- format_names(cutpoint_names)
    cutpoint_names <- sort(cutpoint_names)
    text <- paste0("\n# Cutpoint Authors",
                   "\n## Definition",
                   "\nCutpoint authors, also known as 'articulation points,' are critical nodes in the co-authorship network. If a cutpoint author is removed, the network would break into disconnected components, meaning the collaboration structure would become fragmented. These authors act as bridges, connecting different clusters or groups of researchers, and their presence is essential for maintaining the overall connectivity of the network.",
                   "\n\n## Importance",
                   "\nIdentifying cutpoint authors helps us understand who plays a pivotal role in sustaining collaboration across the network. Their removal could disrupt communication and collaboration between research groups, highlighting their strategic importance in the network's structure.",
                   "\n\n## List of Cutpoint Authors",
                   "\nThe following authors have been identified as cutpoints in this network:",
                   "\n", paste(cutpoint_names, collapse = ", "))
  }
  return(text)
}

#' @export
community_detection <- function(interactions) {
  text <- paste0("\n# Community (cohesive subgroups) Detection",
                 "\n## Strong and Weak Connectivity",
                 "\n\n- **Strongly connected**: ")
  return(text)
}
