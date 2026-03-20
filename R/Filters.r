#' Apply a set of user-defined filters to a dataframe
#'
#' @param df The dataframe to be filtered
#' @param filters A set of filters in a vector, like c("col1 == TRUE", "col2 > 20")
#' @returns The filtered dataframe
#' @export
apply_filters <- function(df, filters) {
  if (is.null(filters) || filters == c("")) {
    return(df)
  }

  for (f in filters) {
    df <- dplyr::filter(df, !!rlang::parse_expr(f))
  }
  df
}

#' Subset a dataframe to only keep rows that are also present in another dataframe
#'
#' @param df The dataframe to subset
#' @param col The df column to use for the subset
#' @param df2 The dataframe to use as reference
#' @param col2 The column to use as reference in df2
#' @returns The subsetted dataframe (df)
#' @export
subset_df <- function(df, col, df2, col2) {
  dplyr::filter(df, !!rlang::sym(col) %in% df2[[col2]])
}
