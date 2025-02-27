#' @export
add_space_after_last_name <- function(name_list) {
  name_list <- sapply(name_list, function(name) {
    sub("([A-Za-z]+)([A-Z]+)", "\\1 \\2", gsub("([A-Z])", " \\1", name))
  })
  return(name_list)
}
