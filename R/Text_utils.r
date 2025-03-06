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

  # Define the columns to keep
  columns_to_keep <- c("Date_Range",
                       "Total_Papers",
                       "Total_Authors",
                       "Density",
                       "Transitivity",
                       "Mean_shortest_path",
                       "Number_of_cutpoints")

  # Filter the columns
  summary_stats <- summary_stats[, columns_to_keep, drop = FALSE]

  # Give Density and Transitivity in percentage
  summary_stats$Density <- summary_stats$Density * 100
  summary_stats$Transitivity <- summary_stats$Transitivity * 100

  # Rename columns
  summary_stats <- summary_stats %>%
    dplyr::rename(`Dates` = Date_Range,
                  `Papers` = Total_Papers,
                  `Authors` = Total_Authors,
                  `Density (%)` = Density,
                  `Transitivity (%)` = Transitivity,
                  `Mean Shortest Path` = Mean_shortest_path,
                  `Cutpoints` = Number_of_cutpoints)

  return(summary_stats)
}