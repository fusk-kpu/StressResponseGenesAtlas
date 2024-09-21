FROM bioconductor/bioconductor_docker:RELEASE_3_19

RUN R -e 'BiocManager::install(c("shiny", "shinydashboard", "shinyWidgets", "shinyFiles", "DT", "ggplot2", "dplyr", "tibble", "htmlwidgets", "plotly", "heatmaply", "magrittr", "gprofiler2", "openxlsx"))'
RUN R -e 'BiocManager::install("genefilter")'

COPY . .

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
