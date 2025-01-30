packages <- c("shiny", "shinydashboard", "shinyWidgets",
              "DT", "ggplot2", "dplyr", "tibble", "htmlwidgets", "plotly",
              "heatmaply", "magrittr", "openxlsx", "markdown")
if (length(setdiff(packages, installed.packages()[, "Package"])) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

BCpackages <- c("genefilter", "clusterProfiler", "org.At.tair.db")
if (length(setdiff(BCpackages, installed.packages()[, "Package"])) > 0) {
  BiocManager::install(setdiff(BCpackages, rownames(installed.packages())))
}

sapply(c(packages, BCpackages), require, character.only=T)

print(sapply(c(packages, BCpackages), require, character.only=T))
