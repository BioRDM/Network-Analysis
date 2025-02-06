
# Introduction
This report presents an analysis of a network dataset, focusing on key structural characteristics, group collaboration dynamics, and interdisciplinary.

# Network Description
## Network Type
- **Undirected**: The network is undirected, meaning all connections are mutual. This is common in co-authorship or collaboration networks. In an undirected network, connections do not have a direction (e.g., if A is connected to B, then B might also be connected to A by definition).
- **Weighted**: The network is weighted, meaning that connections between the nodes (authors) vary in strength or intensity. This result indicates that the network has weighted edges, meaning that the relationships between nodes are assigned numerical values. These weights could represent the strength or frequency of interactions, such as the number of co-authored papers.

## Cohesion Metrics
 - **Total Dyads**: Dyads, representing all possible pairs of nodes (authors connection), were calculated as (n*n-1) = 79003
 - **Actual Edges**: The network contains 3359 actual connections (edges), showing the level of interconnectedness among nodes (authors).
 - **Number of Nodes**: The network consists of 398 nodes (authors), representing the total entities analysed.

# Density and Transitivity
## Density
Density measures the proportion of realised connections compared to all possible connections. It is calculated as:

`Density = Number of Edges/(Number of Possible Edges) = Actual Edges/[nx(n-1)/2]`

- **Value**: 4.25%
- **Interpretation**: TO BE DEFINED (what are the possible cases?)

### Transitivity
Transitivity, or the clustering coefficient, measures the tendency of nodes to form triangles (e.g., if A → B and B → C, then A → C). (In other words, if two nodes are connected to a common third node, they are also likely to be connected to each other). It is calculated as:

`Transitivity = Number of Triangles/Number of Connected Triplets`

- **Value**: 64%
- **Interpretation**: TO BE DEFINED (what are the possible cases?)

# Node Centrality and Centralization
## Degree Centrality
Degree centrality measures the number of direct connections each node has in the network. 

- **Min**: 1
- **Max**: 147
- **Mean**: 17
- **Median**: 12

## Betweenness Centrality
Betweenness centrality quantifies how often a node acts as a bridge in the shortest paths between other nodes.

- **Min**: 0
- **Max**: 17874
- **Mean**: 352
- **Median**: 16

## Closeness Centrality
Closeness centrality measures how quickly a node can access other nodes in the network.

- **Min**: 0.00051
- **Max**: 1
- **Mean**: 0.01281
- **Median**: 0.00091

## Network diameter
The network diameter is the maximum distance (in terms of edges or steps) required to connect any two nodes in the network through the shortest possible path. 

- In our network: **the diameter is 5**, it indicates that the farthest two nodes in your co-authorship network can be linked by the shortest path of 5 steps. 
- In practical terms, if two authors in the network are the most distantly connected, they are separated by 5 intermediaries (co-authors).
