#' @importFrom rlang .data
get_edges <- function(data, vertices, edges, edge_attr = NULL) {
  group_cols <- c(edges, edge_attr)
  data |>
    dplyr::group_by(!!!rlang::syms(group_cols)) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::summarise(
      pairs = list(as.data.frame(t(utils::combn(!!rlang::sym(vertices), 2)))),
      .groups = "drop"
    ) |>
    tidyr::unnest(pairs) |>
    dplyr::mutate(
      from = pmin(.data$V1, .data$V2),
      to   = pmax(.data$V1, .data$V2)
    ) |>
    dplyr::group_by(.data$from, .data$to, !!!rlang::syms(edge_attr)) |>
    dplyr::summarise(weight = dplyr::n(), .groups = "drop") |>
    dplyr::select("from", "to", dplyr::everything())
}

get_graph_metrics <- function(graph) {
  list(
    weighted = igraph::is_weighted(graph),
    density = igraph::edge_density(graph),
    transitivity = igraph::transitivity(graph, type = "global"),
    centrality = get_centrality(graph),
    diameter = igraph::diameter(graph, directed = igraph::is_directed(graph), weights = NULL),
    average_shortest_path = igraph::mean_distance(graph, directed = igraph::is_directed(graph), unconnected = TRUE),
    unreachable_percentage = get_reachability(graph) * 100,
    cutpoints = igraph::articulation_points(graph),
    top_authors_degree = get_most_central_authors(graph, centrality = "degree", n = 15),
    top_authors_betweenness = get_most_central_authors(graph, centrality = "betweenness", n = 15),
    top_authors_harmonic = get_most_central_authors(graph, centrality = "harmonic", n = 15)
  )
}
