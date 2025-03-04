# CoAuthorship - Social Network Analysis

Welcome to the **Social Network Analysis (SNA) repository!**   
This project focuses on *co-authorship network analysis*, a powerful method for visualising and analysing collaboration patterns among researchers. By representing authors as nodes and their co-authorships as edges, we construct a network that reveals clusters of closely connected researchers, highlights central figures, and identifies critical "cutpoint" authors whose removal would fragment the network.   

This repository provides tools and code to perform social network analysis, generate insightful reports, and visualise interactions within a network.


---

## Instructions for Use

### 1. Data format
The input data should be in `.csv`  format, with one column containing author names. This CSV file can be exported from databases such as PubMed, Scopus, Web of Science, or university research databases. 

### 2. Clone the repository
Clone the entire repository to your computer using the `git clone` command or the `GitHub Desktop` application.

### 3. Ruuning the repository 

#### 3.1 intsall Packages: 
Ensure that the following libraries are installed on your computer:   
(the list of required packages here.)

#### 3.2 Report generating: 
The primary file you will need is `Generate_report.R` . You can run this R script in RStudio or Visual Studio Code. Each function and line of code is well-documented, with instructions and explanations on how to modify it to suit your analysis needs.
The code includes various filter functions that you may adjust depending on your research questions, such as:

- The maximum number of authors per paper in your dataset.
- The minimum number of papers per author to be included in the analysis.
- The timeframe (years) you wish to study.
- Whether to split the analysis into smaller time intervals. 

#### 3.3 output folder:
After running the code, the output (including figures, PDFs, and additional CSV files) will be saved in a folder named `output`. This folder will be created automatically if it does not already exist.
If you analyse multiple CSV files, a separate subfolder (under the same name of the csv file) will be created within the output folder for each dataset, containing the relevant results.

##### the generated outcomes: 
      1. `Report_YYYY_YYYY.pdf`  
      This is the main output of the automated code: a PDF report containing the analysis results. It includes a table of contents, detailed interpretations of the findings, and visualisations with captions. 
      The corresponding markdown file, `Report_YYYY_YYYY.md`, is also generated.

      2. `centrality_data_YYY-YYYY.csv`
      This CSV file contains centrality metrics for all authors in the co-authorship network. It includes four columns:
      - Author: Each row represents an author in the network, along with their centrality metrics.
      - Degree: The number of direct co-authorship connections an author has in the network.
      - Closeness: Indicates how quickly an author can reach others in the network, reflecting their proximity to all other authors.
      - Betweenness: Measures the extent to which an author acts as a bridge between other researchers, highlighting their role in connecting different parts of the network.
      
      Use this file to identify influential authors (high degree), those central to information flow (high closeness), and key connectors (high betweenness).
      
      3. `figures` Folder 
      This folder contains three PNG images:
      - graph.png (Figure 1 in the report): Visualisation of the co-authorship network.
      - top_authors.png (Figure 2 in the report): Direct connections between the top 15 most central authors.
      - cutpoints.png (Figure 3 in the report): Highlighted cutpoint authors in the network.

---
## Repository Contents

|---------------- .github             
            (Automatically generated folder used for GitHub workflows, such as building PDF files.)            
|---------------- Pure Data             
            (Contains datasets extracted from the University of Edinburgh's Research Explorer (Pure))             
|---------------- R            
            (Includes all R functions used to generate the analysis outcomes and build the PDF report.)                 
|---------------- Trial Analysis          
            (Contains datasets from the UOE Explorer (Pure) and RMarkdown files used to develop the code.)             
|---------------- assets         
            (Stores additional resources, such as the .tiff file for the font type used in the PDF report.)              
|---------------- data           
            (Contains datasets from the Centre for Engineering Biology at the University of Edinburgh.)            
|---------------- tests  
            (Includes trial datasets and code used during the development and testing of the repository.)  
|------- .Rbuildignore  
            (Specifies files and folders to ignore when building the R package.)       
|-------.gitignore     
            (Lists files and folders to exclude from Git version control.)        
|-------.lintr     
            (Configuration file for linting R code to ensure style and syntax consistency.)            
|-------DESCRIPTION     
            (Metadata file for the R package, including dependencies and project details.)             
|-------Generate_report.r     
            (The main R script for generating the PDF report and other analysis outputs.)           
|-------LICENSE     
            (The license file for the repository (plain text format).)      
|-------LICENSE.md       
            (The license file in markdown format.)       
|-------NAMESPACE         
            (Defines the namespace for the R package, including exported functions.)   
|-------NetworkAnalysis.Rproj       
            (RStudio project file for managing the repository.)   
|-------README.md      
            (This file, providing an overview of the repository and instructions for use.)      

---


### Adding new metrics
To compute new metrics, edit `src/Interactions_class.r` to add a new method (both functions need to be added):
```r
get_diameter.Interactions <- function(interactions) {
  return(diameter(interactions$graph, directed = FALSE, weights = NA))
}

get_diameter <- function(interactions) {
  UseMethod("get_diameter", interactions)
}
```

The method can then be called in the relevant function in `Report_text.r` to obtain the numbers:
```r
diameter <- get_diameter(interactions)
```
