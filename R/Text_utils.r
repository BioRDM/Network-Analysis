#' @export
format_names <- function(name_list) {
  name_list <- sapply(name_list, function(name) {
    if (grepl("[a-z][A-Z]", name)) {
      parts <- unlist(strsplit(name, "(?<=[a-z])(?=[A-Z])", perl = TRUE))
      last_name <- parts[1]

      if (length(parts) > 1) {
        initials <- ""
        for (i in 2:length(parts)) {
          if (grepl("[a-z]", parts[i])) {
            last_name <- paste0(last_name, parts[i])
          } else {
            initials <- paste0(initials, parts[i])
          }
        }

        if (nchar(initials) > 0) {
          formatted_name <- strsplit(initials, "")[[1]] |>
            (\(x) paste(x, collapse = ". "))() |>
            (\(x) paste0(x, "."))() |>
            (\(x) paste(last_name, x))()
        } else {
          formatted_name <- last_name
        }
      } else {
        formatted_name <- last_name
      }
    } else {
      formatted_name <- name
    }
    formatted_name
  })
  name_list
}

#' @export
format_summary_stats <- function(summary_stats) {
  summary_stats$Date_Range <- paste(summary_stats$Start_year, summary_stats$End_year, sep = "-")

  summary_stats$Density <- summary_stats$Density * 100
  summary_stats$Transitivity <- summary_stats$Transitivity * 100

  summary_stats |>
    dplyr::rename_with(dplyr::recode,
                       "Date_Range" = "Dates",
                       "Total_Papers" = "Papers",
                       "Total_Authors" = "Authors",
                       "Average_Authors_per_Paper" = "Authors per Paper",
                       "Density" = "Density (%)",
                       "Transitivity" = "Transitivity (%)",
                       "Mean_degree_centrality" = "Degree Centrality",
                       "Mean_betweenness_centrality" = "Betweenness Centrality",
                       "Mean_harmonic_centrality" = "Harmonic Centrality",
                       "Mean_shortest_path" = "Shortest Path",
                       "Number_of_cutpoints" = "Cutpoints")
}

escape_latex <- function(text) {
  text <- gsub("\\\\", "\\\\textbackslash{}", text)
  text <- gsub("\\$", "\\\\$", text)
  text <- gsub("#", "\\\\#", text)
  text <- gsub("%", "\\\\%", text)
  text <- gsub("&", "\\\\&", text)
  text <- gsub("_", "\\\\_", text)
  text <- gsub("\\{", "\\\\{", text)
  text <- gsub("\\}", "\\\\}", text)
  text <- gsub("\\^", "\\\\textasciicircum{}", text)
  text <- gsub("~", "\\\\textasciitilde{}", text)
  text
}