# CoAuthorship - Social Network Analysis

Welcome to the **Co-Authorship Network Analysis repository!**
This project focuses on *co-authorship network analysis*, a powerful method for visualising and analysing collaboration patterns among researchers. By representing authors as nodes and their co-authorships as edges, we construct a network that reveals clusters of closely connected researchers, highlights central figures, and identifies critical "cutpoint" authors whose removal would fragment the network.

This repository provides tools and code to perform social network analysis, generate insightful reports, and visualise interactions within a network.

## Authors

- Haya Deeb (hdeeb@ed.ac.uk)
- Daniel Thedie (daniel.thedie@ed.ac.uk)
- Andrew Millar (andrew.millar@ed.ac.uk)

General Queries and Data Management: bio_rdm@ed.ac.uk
Principal Investigator and Corresponding Author: Andrew Millar (andrew.millar@ed.ac.uk) Orcid: 0000-0003-1756-3654

---

## Instructions for Use

### 1. Data format

The input data should be in `.csv`  format, with one column containing author names. This CSV file can be exported from databases such as PubMed, Scopus, Web of Science, or university research databases.

### 2. Install the package

Installing this package requires an R version >= 4.5.

Run the following commands in R to install the package:

```r
install.packages("devtools")
devtools::install_github("BioRDM/Network-Analysis")
```

### 3. Generating the report

Generate a config file in R:

```r
library(Network-Analysis)

generate_config("path/to/file/config.yaml")
```
If the path is left empty, a config file named "SNA_config.yaml" will be created in the current directory.

Open the config file (in RStudio or any text editor) and edit the parameters for your analysis. The tables below give short descriptions of the parameters for each config section.

Run the report script:

```r
library(Network-Analysis)

assemble_report(config_path)
```

#### metadata

This section contains metadata that will be displayed at the top of the report.

| Parameter        | Description                                                                                         |
|------------------|-----------------------------------------------------------------------------------------------------|
| Author           | The name of the report author                                                                       |
| Email            | The email of the report author                                                                      |
| Data description | A brief description of the input data                                                               |
| Data access date | The date at which the input data was accessed                                                       |
| Data source      | Where the input data was obtained                                                                   |
| Data source url  | If relevant, a URL pointing to the website from which the data was obtained (or to the data itself) |

#### data

This section contains the main parameters for data processing.

| Parameter             | Description                                                                                                                                                                                                                                                                           |
|-----------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| file_path             | The full path to the input data                                                                                                                                                                                                                                                       |
| output_path           | The path to the output folder                                                                                                                                                                                                                                                         |
| output_suffix         | A suffix to be added to the folder name for the report                                                                                                                                                                                                                                |
| node_id               | Column name for the node IDs in the input data (e.g. author names)                                                                                                                                                                                                                    |
| node_delimiter        | If there are several nodes per table row, how are they delimited (e.g. ; or //)                                                                                                                                                                                                       |
| edge_id               | Column name for the edge IDs in the input data (e.g. article title)                                                                                                                                                                                                                   |
| year_column           | Column name containing the year of publication (can be a full date)                                                                                                                                                                                                                   |
| filters               | A list of filters to be applied to the data prior to creating the network. Filters have to be in format: `colname == value`, where colname is a column in the input data, "==" can be replaced by "<" or ">=", and value can be either a number, or a quoted string (e.g. "Article"). |
| max_authors_per_paper | Articles with more authors than this will be removed from the analysis. Leave empty for no filter.                                                                                                                                                                                    |
| min_papers_per_author | Authors with fewer articles than this will be removed from the analysis. Leave empty for no filter.                                                                                                                                                                                   |
| from_year             | Only keep edges (articles) published after this year.                                                                                                                                                                                                                                 |
| to_year               | Only keep articles published before this year.                                                                                                                                                                                                                                        |
| split_per_year        | Produce multiple reports for smaller year ranges between from_year and to_year. e.g. set `split per year: 2` to produce reports in 2-year increments.                                                                                                                                 |

#### plot

This section contains options to customise the report plots.

| Parameter | Description                                                                    |
|-----------|--------------------------------------------------------------------------------|
| layout    | The layout of the graph. Can be `centrality` (default), `auto`, or `circular`. |

#### node_properties

This section allows the user to provide information about the nodes (authors) from a separate file, e.g. academic affiliations. The table given here as input must have one row per author (missing values are ok).

| Parameter | Description                                                                                                     |
|-----------|-----------------------------------------------------------------------------------------------------------------|
| file_path | The path to the node properties file. If it is empty, this whole section will be ignored.                       |
| filters   | Filters for the node properties table, defined as in the data section.                                          |
| remove_NA | TRUE/FALSE Whether to include nodes that are missing (or been filtered out) from this table in the final graph. |
| node_id   | Column name for the node IDs (name format must match with the node_id column provided under data)               |
| color     | Column name to color the nodes by.                                                                              |
| order     | A list that provides the order in which legend entries should be displayed                                      |
| palette   | A list with custom colors (e.g. color names or hex codes) for the different levels in "color"                   |

## Generated outcomes

1. `Report_YYYY_YYYY.pdf`
This is the main output of the automated code: a PDF report containing the analysis results. It includes a table of contents, detailed interpretations of the findings, and visualisations with captions. 
2. `data` folder
This folder contains .csv tables with raw data extracted from the analysis
- `Centrality_data_YYY-YYYY.csv`: This CSV file contains centrality metrics for all authors in the co-authorship network. It includes four columns:
  - Author: Each row represents an author in the network, along with their centrality metrics.
  - Degree: The number of direct co-authorship connections an author has in the network.
  - Harmonic: Indicates how quickly an author can reach others in the network, reflecting their proximity to all other authors.
  - Betweenness: Measures the extent to which an author acts as a bridge between other researchers, highlighting their role in connecting different parts of the network.
  - Use this file to identify influential authors (high degree), those central to information flow (high harmonicity), and key connectors (high betweenness).
- `Summary_statistics.csv`: a table listing the main report statistics, with one line per date range (as defined by the `split_per_year` option)
- `Cutpoints_YYYY-YYYY.csv`: a list of authors identified as cutpoints in the network
- `Filtered_data_YYYY-YYYY.csv`: the input data after filtering was applied
- `Papers_per_author_YYYY-YYYY.csv`: the number of papers by each author, before and after filtering (used for quality control)
- `Raw_data_YYYY-YYYY.csv`: the raw data used to create the report (prior to filtering)
- `Vertex_attributes_YYYY-YYYY.csv`: the vertex attributes that were used to color the nodes in the report. The "community" column shows an automated clustering of the nodes using the `igraph` "cluster_louvain" method.
3. `Summary.pdf`: a table listing the main report statistics, with one line per date range (as defined by the `split_per_year` option)
4. The yaml config file used to create the report.

