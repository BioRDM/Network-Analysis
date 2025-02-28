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
