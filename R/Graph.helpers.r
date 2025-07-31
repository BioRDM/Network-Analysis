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
