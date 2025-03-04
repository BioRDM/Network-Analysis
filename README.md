# CoAuthorship - Social Network Analysis

Welcome to the **Social Network Analysis (SNA) repository!**   
This project focuses on *co-authorship network analysis*, a powerful method for visualising and analysing collaboration patterns among researchers. By representing authors as nodes and their co-authorships as edges, we construct a network that reveals clusters of closely connected researchers, highlights central figures, and identifies critical "cutpoint" authors whose removal would fragment the network.   

This repository provides tools and code to perform social network analysis, generate insightful reports, and visualise interactions within a network.


---

## Instructions for Use

### 1. Data format
The input data should be in `.csv`  format, with one column containing author names. This CSV file can be exported from databases such as PubMed, Scopus, Web of Science, or university research databases. 

### 2. Install the package
Installing this package requires an R version >= 4.4.

Run the following commands in R to install the package:
```R
install.packages("pak")
pak::pkg_install("BioRDM/Network-Analysis")
```

#### For Linux users
Additional dependencies might be needed. On Debian-based systems, they can be installed with this command:
`sudo apt-get install -y --no-install-recommends libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev libtiff5-dev texlive-xetex`

### 3. Generating the report

Download the [generate_report.R](https://github.com/BioRDM/Network-Analysis/blob/main/Generate_report.r) script.

You can run this R script in RStudio or Visual Studio Code. Each function and line of code is well-documented, with instructions and explanations on how to modify it to suit your analysis needs.
The code includes various filter functions that you may adjust depending on your research questions, such as:

- The maximum number of authors per paper in your dataset.
- The minimum number of papers per author to be included in the analysis.
- The timeframe (years) you wish to study.
- Whether to split the analysis into smaller time intervals. 

#### 3.3 Output folder:
After running the code, the output (including figures, PDFs, and additional CSV files) will be saved in a folder named `output`. This folder will be created automatically if it does not already exist.
If you analyse multiple CSV files, a separate subfolder (under the same name of the csv file) will be created within the output folder for each dataset, containing the relevant results.

##### Generated outcomes: 
      1. `Report_YYYY_YYYY.pdf`  
      This is the main output of the automated code: a PDF report containing the analysis results. It includes a table of contents, detailed interpretations of the findings, and visualisations with captions. 
      The corresponding markdown file, `Report_YYYY_YYYY.md`, is also generated.

      2. `centrality_data_YYY-YYYY.csv`
      This CSV file contains centrality metrics for all authors in the co-authorship network. It includes four columns:
      - Author: Each row represents an author in the network, along with their centrality metrics.
      - Degree: The number of direct co-authorship connections an author has in the network.
      - Harmonic: Indicates how quickly an author can reach others in the network, reflecting their proximity to all other authors.
      - Betweenness: Measures the extent to which an author acts as a bridge between other researchers, highlighting their role in connecting different parts of the network.
      
      Use this file to identify influential authors (high degree), those central to information flow (high harmonicity), and key connectors (high betweenness).
      
      3. `figures` Folder 
      This folder contains three PNG images:
      - graph.png (Figure 1 in the report): Visualisation of the co-authorship network.
      - top_authors.png (Figure 2 in the report): Direct connections between the top 15 most central authors.
      - cutpoints.png (Figure 3 in the report): Highlighted cutpoint authors in the network.

---
## Repository Contents

|---------------- .github             
            (Folder for GitHub workflows, for code testing purposes.)            
|---------------- Pure Data             
            (Contains datasets extracted from the University of Edinburgh's Research Explorer (Pure))             
|---------------- R            
            (Includes all R functions used to generate the analysis outcomes and build the PDF report.)                 
|---------------- Trial Analysis          
            (Contains datasets from the UOE Explorer (Pure) and RMarkdown files used to develop the code.)             
|---------------- assets         
            (Stores additional resources, such as the .ttf file for the font type used in the PDF report.)              
|---------------- data           
            (Contains datasets from the Centre for Engineering Biology at the University of Edinburgh.)            
|---------------- tests  
            (Contains unit-tests for the package.)  
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
