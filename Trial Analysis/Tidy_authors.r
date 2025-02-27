library(NetworkAnalysis)

data <- import_csv_data("Trial Analysis\\Pure_23_24_Code.csv")

data <- tidy_authors(data, author_column = "Author")

author_col <- rlang::sym("Author")

data <- data %>% mutate(!!author_col := stringr::str_replace_all(!!author_col, ";", "; "))

write.csv(data, "Trial Analysis\\Pure_23_24_Code_tidy_authors.csv", row.names = FALSE)