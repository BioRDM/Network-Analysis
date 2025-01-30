introduction <- function() {
    text <- "# Network analysis report\n
## Introduction
This report presents an analysis of a network dataset, focusing on key structural characteristics, group collaboration dynamics, and interdisciplinary."
    return(text)
}

network_type <- function(directed) {
    text <- "\n## Network Description
Network Type is:\n"
    if (directed) {
        text <- paste(text, "- **Directed**: The network is directed, meaning ... TO BE FILLED.", sep="")
    } else if (!directed) {
       text <- paste(text, "- **Undirected**: The network is undirected, meaning all connections are mutual. This is common in co-authorship or collaboration networks. In an undirected network, connections do not have a direction (e.g., if A is connected to B, then B might also be connected to A by definition).", sep="")
    } else  {
        text <- paste(text, "- **Unknown**: The network type is unknown. This could be due to an error in the data or the data is not available.", sep="")
    }
    return(text)
}