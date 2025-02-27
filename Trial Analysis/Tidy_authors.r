library(NetworkAnalysis)

data <- import_csv_data("Trial Analysis\\Pure_23_24_Code.csv")

data <- tidy_authors(data, author_column = "Author")

write.csv(data, "Trial Analysis\\Pure_23_24_Code_tidy_authors.csv", row.names = FALSE)