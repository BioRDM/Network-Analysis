---
title: 'Network Analysis Report'
date: \today
output:
  pdf_document:
    latex_engine: xelatex
    extra_dependencies: ['fontspec']
fontsize: 12pt
mainfont: 'DejaVu Sans'
toc: TRUE
---


# Introduction
This report presents an analysis of a network dataset, focusing on key structural characteristics, group collaboration dynamics, and interdisciplinarity.

![](output/Pure_23_24_Code/figures/graph_2023-2024.png)
**Figure 1:** Visualisation of the Author network


# Data filtering
## Before filtering
 - Average authors per paper: 83
 - Median authors per paper: 8
 - Minimum authors per paper: 0
 - Maximum authors per paper: 1217

## Filtering
163 papers were removed from the dataset due to having more than 50 authors.

8882 authors were removed from the dataset because they appeared in less than 5 papers.

In total, 1369 papers and 246 authors were included in the network analysis.

## After filtering
 - Average authors per paper: 2
 - Median authors per paper: 1
 - Minimum authors per paper: 1
 - Maximum authors per paper: 14

# Network Description
## Network Type
 - **Undirected**: The network is undirected, meaning all connections are mutual. This is common in co-authorship or collaboration networks. In an undirected network, connections do not have a direction (e.g., if A is connected to B, then B might also be connected to A by definition).
 - **Weighted**: The network is weighted, meaning that connections between the nodes (authors) vary in strength or intensity. This result indicates that the network has weighted edges, meaning that the relationships between nodes are assigned numerical values. These weights could represent the strength or frequency of interactions, such as the number of co-authored papers.

## Cohesion Metrics
 - **Total Dyads**: Dyads, representing all possible pairs of nodes (authors connection), were calculated as (n*n-1) = 27261
 - **Actual Edges**: The network contains 959 actual connections (edges), showing the level of interconnectedness among nodes (authors).
                 This is the count of actual connections (edges) present in the network, indicating how interconnected the nodes are
 - **Number of Nodes**: The network consists of 233 nodes (authors), representing the total entities analysed.

# Density and Transitivity
## Density
Density measures the proportion of realised connections compared to all possible connections. It is calculated as:

`Density = Number of Edges/(Number of Possible Edges) = Actual Edges/[nx(n-1)/2]`

- **Value**: 3.54%
- **Interpretation**: Values of Density range from 0 to 1.
                 High Density: Close to 1, meaning most nodes are connected. This indicates a highly cohesive network.
                 Low Density: Close to 0, meaning most nodes are not connected. Sparse networks are common in real-world systems.

## Transitivity
Transitivity, or the clustering coefficient, measures the tendency of nodes to form triangles (e.g., if A → B and B → C, then A → C). (In other words, if two nodes are connected to a common third node, they are also likely to be connected to each other). It is calculated as:

`Transitivity = Number of Triangles/Number of Connected Triplets`

- **Value**: 47%
- **Interpretation**: Values range from 0 to 1.
                 High Transitivity: Close to 1, indicating a high level of clustering. Nodes tend to form tight-knit groups.
                 Low Transitivity: Close to 0, indicating a lack of clustering.
                 A higher Transitivity value suggests that network tend to collaborate in groups, forming cohesive communities.

# Node Centrality and Centralization
## Degree Centrality
Degree centrality measures the number of direct connections each node has in the network.
                 Nodes with higher degree centrality are likely to be influential or well-connected individuals/entities.

- **The least connected node has (Min)**: 1
- **The most connected node has (Max)**: 41
- **On average, nodes have about (Mean)**: 8
- **Half of the nodes have more connection than (Median)**: 7

## Betweenness Centrality
Betweenness centrality quantifies how often a node acts as a bridge in the shortest paths between other nodes.
                 Nodes with high betweenness are critical connectors in the network. Removing them could significantly disrupt the flow of information.

- **Min**: 0
- **Max**: 3343(the node with the highest betweenness value is a critical bridge in the network, controlling the flow of information).
- **Mean**: 311
- **Median**: 53

## Closeness Centrality
Closeness centrality measures how quickly a node can access other nodes in the network.
                 Nodes with high closeness centrality are in advantageous positions to disseminate information quickly.

- **Min**: 0.00032(average path for the least central node to reach other nodes).
- **Max**: 1
- **Mean**: 0.02342
- **Median**: 0.00108

## Network diameter
The network diameter is the maximum distance (in terms of edges or steps) required to connect any two nodes in the network through the shortest possible path. 

- In our network: **the diameter is 20**, it indicates that the farthest two nodes in the co-authorship network can be linked in 20 steps at shortest.
- In practical terms, the two most distantly connected authors in the network are separated by 20 intermediates (co-authors).
- The average shortest path between two authors in the network is 4.9 steps.

# Reachability
Reachability measures the proportion of pairs of nodes that are reachable in the network.

- **Unreachable Pairs**: 14% of all possible pairs of nodes are unreachable in the network.
- **Interpretation**: A high percentage of unreachable pairs may indicate a fragmented network or isolated nodes that are not connected to the main network.

![](output/Pure_23_24_Code/figures/top_authors_2023-2024.png)
**Figure 2:** Direct connections between the 15 most central authors


# Cutpoint Authors
## Definition
Cutpoint authors are nodes in the network whose removal would increase the number of connected components in the network.

## List of Cutpoint Authors
Akiyoshi B., Albery G. F., Chen Z., Clarke D. J., Grima R., Halliday K. J., Hearn J., Hermann A., Keightley P. D., Li Y., Marston A. L., Martin S. H., McNeilly T. N., Moses T., Mutapi F., Nicholls J. A., Pennington R. T., Rappsilber J., Rosser S. J., Sato K., Spanos C., Thumbi S. M., Wang S., Wood C. W.

![](output/Pure_23_24_Code/figures/cutpoints_2023-2024.png)
**Figure 3:** Visualisation of the Author network with cutpoints highlighted

