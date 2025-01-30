# Network-Analysis

This code computes metrics about a network of publications.

## Data format
For input format, see the file `data/SBS_Direct_Andrew_data.net`.

## Modifying the report
Run `Generate_report.r` to generate the report in markdown format (`Report.md`).

### Changing text
Report text can be edited in `src/Report_text.r`

### Adding new metrics
To compute new metrics, edit `src/Graph_class.r` to add a new method (both functions need to be added):
```r
get_diameter.Graph <- function(graph) {
  return(diameter(graph$data, directed = FALSE, weights = NA))
}

get_diameter <- function(graph) {
  UseMethod("get_diameter", graph)
}
```

The method can then be called in the relevant function in `Report_text.r` to obtain the numbers:
```r
diameter <- get_diameter(graph)
```

