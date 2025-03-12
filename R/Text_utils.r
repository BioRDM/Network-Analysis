#' @export
format_names <- function(name_list) {
  name_list <- sapply(name_list, function(name) {
    # First check if there are any uppercase letters after lowercase letters
    if (grepl("[a-z][A-Z]", name)) {
      # Split by the transition from lowercase to uppercase
      parts <- unlist(strsplit(name, "(?<=[a-z])(?=[A-Z])", perl = TRUE))

      # The first part is the last name
      last_name <- parts[1]

      # The rest are potential initials - but check if they're all uppercase
      if (length(parts) > 1) {
        initials <- ""
        for (i in 2:length(parts)) {
          # If the part contains lowercase, it's still part of the last name
          if (grepl("[a-z]", parts[i])) {
            last_name <- paste0(last_name, parts[i])
          } else {
            # Otherwise it's initials
            initials <- paste0(initials, parts[i])
          }
        }

        # Format initials with spaces and dots
        if (nchar(initials) > 0) {
          initials_chars <- strsplit(initials, "")[[1]]
          formatted_initials <- paste(initials_chars, collapse = ". ")
          formatted_initials <- paste0(formatted_initials, ".")

          # Combine last name with formatted initials
          formatted_name <- paste(last_name, formatted_initials)
        } else {
          formatted_name <- last_name
        }
      } else {
        formatted_name <- last_name
      }
    } else {
      # If no initials pattern detected, return name as is
      formatted_name <- name
    }
    return(formatted_name)
  })
  return(name_list)
}

#' @export
format_summary_stats <- function(summary_stats) {
  # Merge Start_year and End_year into "Date Range"
  summary_stats$Date_Range <- paste(summary_stats$Start_year, summary_stats$End_year, sep = "-")

  # Give Density and Transitivity in percentage
  summary_stats$Density <- summary_stats$Density * 100
  summary_stats$Transitivity <- summary_stats$Transitivity * 100

  # Rename columns
  summary_stats <- summary_stats %>%
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

  return(summary_stats)
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
  return(text)
}