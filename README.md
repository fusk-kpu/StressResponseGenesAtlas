# AtSRGA: an interactive web application to retrieve stress-responsive genes in _Arabidopsis thaliana_

## Background
Abiotic and biotic stresses pose significant constraints to plant productivity. Elucidating the gene regulatory networks involved in stress responses and tolerance in plants is essential for advancing future breeding programs and developing innovative agricultural products.

## What is AtSRGA
We introduce a user-friendly application, AtSRGA, which enables users to retrieve stress-responsive genes in Arabidopsis thaliana (Arabidopsis). To develop the application, we collected a total of 1,133 microarray and 1,050 RNA sequencing datasets in Arabidopsis from publicly accessible databases. These data are corresponding 11 biotic and abiotic stresses, including abscisic acid, cold, drought, heat, high light, hypoxia, osmotic stress, oxidation, salt, wounding, and Pseudomonas syringae pv. Tomato DC3000. Using a modified meta-analysis technique known as the vote-counting method, we computed integrated scores to evaluate the stress responsiveness for each stress condition across multiple studies. AtSRGA visualizes gene behavior under 11 stress conditions and offers an interactive, user-friendly interface accessible to researchers without bioinformatics training.

## How To Use It
To run this App after downloading this App from Github to your local PC, please follow the steps as below:

1. If 'Download ZIP', upzip the downloaded file (StressResponseGenesAtlas-main.zip) to the Desktop.

2. Open R or RStudio (if installed) with R version >= 4.3.

3. Set your working directory to where this App are unzipped or downloaded. 

   For example, I downloaded this App via 'Download ZIP' and unzipped it to the Desktop, then I need to set the working directory in R by `setwd("~/Desktop/StressResponseGenesAtlas-main")`.
   
4. Install all depended CRAN R packages and R Bioconductor packages by sourcing the R installation program with `source("prep.R")`.

If all depended packages are successfully installed, logical value “TRUE” should be returned and printed on the R Console pane for each package after sourcing the installation program. Otherwise, please check whether you have the R version >=4.3 installed.
    
5. run this App by `shiny::runApp()`

6. A web page will be open in you Browser to show the stress responsive gene atlas in Arabidopsis. Please see our tutorial file.

## Feedback
If you have further questions or suggestions regarding this App, please contact Atsushi Fukushima at afukushima@kpu.ac.jp at the Kyoto Prefectural Universitye (KPU).
