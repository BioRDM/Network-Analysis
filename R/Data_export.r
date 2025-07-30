save_centrality_data <- function(graph, output_file = "output/centrality_data.csv") {
  output_data <- as.data.frame(get_centrality(graph))
  rownames(output_data) <- format_names(igraph::V(graph)$name)
  output_data <- output_data[statnet.common::order(-output_data$degree), ]
  utils::write.csv(output_data, output_file, row.names = TRUE)
}

save_papers_per_author <- function(prefilter_papers_per_author, postfilter_papers_per_author, output_file = "output/papers_per_author.csv") {
  merged_papers_per_author <- dplyr::full_join(
    prefilter_papers_per_author |> dplyr::rename(Papers_prefilter = "Papers"),
    postfilter_papers_per_author |> dplyr::rename(Papers_postfilter = "Papers"),
    by = "Author"
  ) |>
    dplyr::mutate(
      Papers_prefilter = tidyr::replace_na(.data$Papers_prefilter, 0),
      Papers_postfilter = tidyr::replace_na(.data$Papers_postfilter, 0)
    ) |>
    dplyr::mutate(Papers_removed = .data$Papers_prefilter - .data$Papers_postfilter) |>
    dplyr::arrange(dplyr::desc(.data$Papers_removed))

  utils::write.csv(merged_papers_per_author, output_file, row.names = FALSE)
}
