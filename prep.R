packages <- c("shiny", "shinydashboard", "shinyWidgets", "shinyFiles",
              "DT", "ggplot2", "dplyr", "tibble", "htmlwidgets", "plotly",
              "heatmaply", "magrittr", "gprofiler2", "openxlsx")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

BCpackages <- "genefilter"
if (length(setdiff(BCpackages, rownames(installed.packages()))) > 0) {
  # source("http://bioconductor.org/biocLite.R")
  # biocLite(setdiff(BCpackages, rownames(installed.packages())))
  BiocManager::install(setdiff(BCpackages, rownames(installed.packages())))
}

sapply(c(packages, BCpackages), require, character.only=T)

print(sapply(c(packages, BCpackages), require, character.only=T))
