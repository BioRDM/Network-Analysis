assemble_report <- function(config) {
  config <- read_config(config)

  # Import the data
  data <- import_csv_data(config$file_path)
  # Replace spaces with dots in column names to match R column names
  config$author_column_name <- gsub(" ", ".", config$author_column_name)
  config$year_column_name <- gsub(" ", ".", config$year_column_name)
  # Check that the author column exists
  check_author_column(data, config$author_column_name)

  # Tidy the author column
  data <- tidy_authors(data, author_column = config$author_column_name)  # Tidy the author names

  # Create output folders
  paths <- create_output_paths(config)

  # Calculate the year intervals for the reports
  years <- get_years_from_to(data, config)
  years_from <- years$years_from
  years_to <- years$years_to

  # Create the report(s)
  for (i in seq_along(years_from)) {
    from_year <- years_from[i]
    to_year <- years_to[i]
    date_range <- paste0(from_year, "-", to_year)
    print(paste0("Creating report for the period ", from_year, " to ", to_year))

    # Retrieve authors statistics before filtering
    prefilter_author_stats <- get_author_stats(data, author_column_name = config$author_column_name, delimiter = config$author_delimiter)

    interactions <- Interactions(data = data,
                                 author_delimiter = config$author_delimiter,
                                 author_column_name = config$author_column_name,
                                 year_column_name = config$year_column_name,
                                 max_authors_per_paper = config$max_authors_per_paper,
                                 min_papers_per_author = config$min_papers_per_author,
                                 directed = config$directed,
                                 from_year = from_year,
                                 to_year = to_year)

    # Retrieve authors statistics after filtering
    postfilter_author_stats <- get_author_stats(interactions$data, author_column_name = config$author_column_name, delimiter = config$author_delimiter)

    # Initiate report
    report <- Report()

    # Add filtering information
    report <- add(report, data_filtering(interactions, prefilter_author_stats, postfilter_author_stats))

    # Add graph Figure
    report <- add_figure(
      report,
      plot = plot_graph(interactions, output_file = paste0(paths$figures, "/graph_", date_range, ".png")),
      fig_caption = "**Visualisation of the Co-Authorship Network Analysis**
      \nEach color represents a cluster of authors who are strongly connected within the network. The size of each node corresponds to the centrality of the author, with larger nodes indicating higher centrality and stronger connections within the network. Centrality, in this context, reflects an author's influence based on their position in the network — those with higher centrality are more interconnected and play a larger role in facilitating collaboration. The legend identifies the most central authors from each cluster, providing insight into key contributors within their respective groups."
    )

    # Add network type paragraph
    report <- add(report, network_type(interactions))

    # Add cohesion metrics
    report <- add(report, cohesion_metrics(interactions))

    # Add density and transitivity info
    report <- add(report, density_transitivity(interactions))

    # Add centrality metrics
    report <- add(report, centrality_metrics(interactions))

    # Add reachability metrics
    report <- add(report, reachability_metrics(interactions))

    # Add top authors Figure
    report <- add_figure(
      report,
      plot = plot_top_authors(interactions, n = 15, output_file = paste0(paths$figures, "/top_authors_", date_range, ".png")),
      fig_caption = "**Direct connections between the 15 most central authors**
      \nThis figure highlights the direct connections between the 15 most central authors in the network. Nodes positioned around the circle represent these top authors, and the thickness of the connecting lines indicates the strength of their collaboration (thicker lines represent stronger connections). 
      \nA known limitation in social network analysis is reflected in cases where authors share the same last name and first initial, leading to duplicate entries being treated as a single node (in this figures, it looks like the authors connected to themselves). This issue arises when the dataset lacks full names, resulting in potential misrepresentation of distinct authors as one."
    )

    # Save centrality data as csv
    save_centrality_data(interactions, paste0(paths$output, "/centrality_data_", date_range, ".csv"))

    # Add list of cutpoint authors
    report <- add(report, cutpoint_authors(interactions))

    # Add cutpoints Figure
    report <- add_figure(
      report,
      plot = plot_cutpoints(interactions, output_file = paste0(paths$figures, "/cutpoints_", date_range, ".png")),
      fig_caption = "**Visualisation of the Author network with cutpoints highlighted**
      \nThis figure builds on Figure 1, with the addition of highlighting cutpoint authors in red. Cutpoints are authors whose removal would fragment the network, indicating their critical role in maintaining connectivity. The node size, as in Figure 1, still reflects the centrality of each author — larger nodes indicate more central figures within the network. The names of these cutpoint authors are listed above."
    )

    # Appendix
    report <- add(report, "\n\n# Appendix\n")

    # Network plot with vertex size based on betweenness centrality
    report <- add_figure(
      report,
      plot = plot_graph(interactions,
                        centrality = "betweenness",
                        output_file = paste0(paths$figures, "/graph_betweenness_", date_range, ".png")),
      fig_caption = "**Visualisation of betweenness centrality in the Co-Authorship Network Analysis**
      \nEach color represents a cluster of authors who are strongly connected within the network. The size of each node corresponds to the betweenness centrality of the author, a measure of how often an author appears on the shortest path between two other authors. Authors with high betweenness centrality act as bridges, controlling the flow of information between different clusters or groups. "
    )

    report <- add_figure(
      report,
      plot = plot_graph(interactions,
                        centrality = "none",
                        output_file = paste0(paths$figures, "/graph_no_centrality_", date_range, ".png")),
      fig_caption = "**Visualisation of the Co-Authorship Network Analysis without centrality indicator**
      \nEach color represents a cluster of authors who are strongly connected within the network. The legend identifies the most central authors from each cluster, providing insight into key contributors within their respective groups."
    )

    # Save report as markdown
    save_md(report, paste0(paths$output, "/Report_", date_range, ".md"))

    # Convert the report to pdf
    export_pdf(report, "Report.md", output_file = paste0(paths$output, "/Report_", date_range, ".pdf"))
  }

}