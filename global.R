# Source scripts
# source("proxy.R", local = TRUE)
source("setup.R", local = TRUE)
source("data_processing.R", local = TRUE)


# Sub Menu : Atlas ####
## Tab : Overview ####
overviewUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("reset"), "Reset Table"),
    actionButton(ns("remove"), "Remove",
                 style = "color: red"),
    br(),
    br(),
    dataTableOutput(ns("atlas")),
    br(),
    br(),
    sidebarLayout(
      sidebarPanel(h3(strong("Bulk Search → Heatmap")), width = 3,
                   textAreaInput(ns("text"), h4("1. Input list of identifiers :"),
                                 width = "400px", height = "300px"
                   ),
                   actionButton(ns("submit"), "Submit",
                                style = "color: white; background-color: #337ab7; border-color: #2e6da4"),
                   actionButton(ns("example1"), "Example1"),
                   actionButton(ns("example2"), "Example2"),
                   br(),
                   h4("You can remove inappropriate rows if any."),
                   "1. Select rows in the table.",
                   br(),
                   "2. Click 'Remove' button above the table.",
                   br(),
                   br(),
                   h4("2. Control paremeters of heatmap"),
                   selectInput(ns("identifier"),
                               label = "Choose y axis :",
                               choices = c("ensembl_gene_id",
                                           "SYMBOL"),
                               selected = "ensembl_gene_id"),
                   numericInput(ns("height"),
                                label = "height :",
                                value = 400),
                   actionButton(ns("heatmap"), "Plot",
                                style = "color: white; background-color: #337ab7; border-color: #2e6da4")
      ),
      mainPanel(
        dataTableOutput(ns("bulk"))
      )
    )
  )
}

overview <- function(input, output, session, srga, cl, Breaks, Color) {
  ##　Table Display : SRGA (Stress Response Gene Atlas) ####
  ### Display the atlas as a heatmap ####
  rv  <- reactiveValues(df = NULL)
  rv$df <- srga
  observeEvent(input$reset, {
    rv$df <- srga
  })
  output$atlas <- renderDataTable({
    datatable(
      rv$df,
      filter = "top",
      selection = "single",
      extensions = c("Buttons", "FixedColumns"),
      escape = FALSE,
      rownames = FALSE,
      options = list(columnDefs = list(list(className = "dt-nowrap", targets = "_all"),
                                       list(visible = FALSE, targets = NULL)),
                     scrollX = TRUE, 
                     fixedColumns = TRUE,
                     dom = "lrtBip",
                     buttons = list(
                       I("colvis"),
                       list(extend = "collection",
                            buttons = list(list(extend = "csv", filename = "SRGA"),
                                           list(extend = "excel", filename = "SRGA")
                            ),
                            text = "Download"))
      )
    ) %>%
      formatStyle(names(srga[cl]), backgroundColor = styleInterval(Breaks, Color))
  }, server = TRUE)
  
  ## Side Bar : Bulk Search → Heatmap ####
  ### Input string ####
  #### Lowercase conversion and delimiter splitting（space or break or tab or commma）####
  lower_input <- reactive({
    strsplit(tolower(input$text), " |\n|\t|,")[[1]]
  })
  
  #### Get index of AGI code ####
  agi_index <- reactive({
    grepl("at.g.+", lower_input(), ignore.case = T)
  })
  
  #### Delete Version Part of AGI Code（e.g., ATXGXXXX.1, ATXGXXXXX.3） ####
  key <- reactive({
    c(gsub("\\..+", "", lower_input()[agi_index()]), lower_input()[!agi_index()])
  })
  
  ### SRGA ####
  #### Extract AGI code and SYMBOL for each line
  AGIandSYMBOL <- reactive(apply(srga[c("ensembl_gene_id", "SYMBOL")], 1, paste, collapse = "/"))
  
  #### Divide with a slash ####
  splited_AGIandSYMBOL <- reactive(strsplit(AGIandSYMBOL(), "/"))
  
  #### Convert to lowcase ####
  lowered_AGIandSYMBOL <- reactive(lapply(splited_AGIandSYMBOL(), tolower))
  
  #### Get the index of the element that exactly matches the input string ####
  matched_AGIandSYMBOL <- reactive(lapply(lowered_AGIandSYMBOL(), match, key()[which(key() != "")]))
  
  ### Retrieve the row corresponding to the number from SRGA ####
  observeEvent(input$submit, {
    rv$df <- srga[which(lapply(matched_AGIandSYMBOL(), any) == TRUE), ]
  })
  
  ### Remove line ####
  observeEvent(input$remove, {
    rv$df <- rv$df[-as.numeric(input$atlas_rows_selected), ]
  })
  
  ### Example of input string ####
  #### First ####
  observeEvent(input$example1, {
    name <- paste("ADH1", "DREB1A", "ELIP1", "HSP17.8", "JAZ1", "NCED3", "PP2CA")
    updateTextAreaInput(session, "text", value = name)
  })
  #### Second ####
  observeEvent(input$example2, {
    name <- paste("DREB1A", "DREB1B", "DREB1C", "DREB2A", "DREB2B", "GolS1", "GolS2", "GolS3", sep = "\n")
    updateTextAreaInput(session, "text", value = name)
  })
  
  output$tesss <- renderDataTable({
    Filter(is.numeric,
           set_rownames(rv$df, value = rv$df[, input$identifier]))
  })
  
  ### Create a heatmap ####
  heatmap_tbl <- reactive(
    heatmaply(Filter(is.numeric,
                     set_rownames(rv$df, value = rv$df[, input$identifier])),
              height = input$height,
              grid_gap = 0.2, grid_color = "gray90",
              scale_fill_gradient_fun = scale_fill_gradient2(
                low = "deepskyblue",
                high = "hotpink",
                midpoint = 0
              ),
              Rowv = FALSE,
              Colv = FALSE,
              cellnote = Filter(is.numeric,
                                set_rownames(rv$df, value = rv$df[, input$identifier])),
              cellnote_size = 18,
              cellnote_textposition = "middle center")
  )
  
  ### Display the heatmap inside the modal dialog box ####
  output$plot <- renderPlotly({
    heatmap_tbl()
  })
  ns <- session$ns
  observeEvent(input$heatmap, {
    showModal(modalDialog({
      plotlyOutput(ns("plot"))},
      easyClose = TRUE,
      size = "l",
      title = "Heatmap"))
  })
  
  return(reactive(rv$df$ensembl_gene_id[input$atlas_rows_selected]))
}

## Tab : Abiotic and biotic stress ####
stressUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    actionButton(ns("button_ratio"), "Show SRratio", icon("table"),
                 style = "color: white; background-color: #337ab7; border-color: #2e6da4"),
    br(),
    br(),
    dataTableOutput(ns("ratio")),
    br(),
    br(),
    br(),
    actionButton(ns("button_metadata"), "Show metadata", icon("table"),
                 style = "color: white; background-color: #337ab7; border-color: #2e6da4"),
    actionButton(ns("button_metadata_more"), "", icon("filter"),
                 style = "color: pink; background-color: white; border-color: #2e6da4"),
    actionButton(ns("button_metadata_middle"), "", icon("filter"),
                 style = "color: black; background-color: white; border-color: #2e6da4"),
    actionButton(ns("button_metadata_less"), "", icon("filter"),
                 style = "color: skyblue; background-color: white; border-color: #2e6da4"),
    br(),
    dataTableOutput(ns("metadata"))
  )
}

stress <- function(input, output, session, ratio, srga, selectedRow, metadata) {
  ## Table Display : SRratio ####
  ### Retrieve the SRratio of the specified gene (改善点あり) ####
  selectedRatio <- reactive({
    ratio[which(ratio$ensembl_gene_id %in% selectedRow()), ]
  })
  
  rv <- reactiveValues(ratio = NULL, metadata = NULL)
  
  observeEvent(input$button_ratio, {
    rv$ratio <- ratio[which(ratio$ensembl_gene_id %in% selectedRow()), ]
  })
  
  ### Display SRratio ####
  output$ratio <- renderDataTable({
    datatable(rv$ratio,
              extensions = c("Buttons", "FixedColumns"),
              rownames = FALSE,
              selection = "single",
              options = list(scrollX =TRUE, fixedColumns = TRUE)) %>%
      formatStyle(colnames(selectedRatio())[-1], backgroundColor = styleInterval(c(-2, 2), c("skyblue", "white", "pink")))
  })
  
  ### Identify the treated samples that meet the following criteria ####
  #### SRratio ≧ 2 (1) ####
  more <- reactive({
    which(metadata$treated_sample %in% colnames(rv$ratio)[rv$ratio >= 2])
  })
  
  #### -2 < SRratio < 2 ####
  middle <- reactive({
    which(metadata$treated_sample %in% colnames(rv$ratio)[-2 <= rv$ratio & rv$ratio <= 2])
  })
  
  #### SRratio ≦ -2 (2) ####
  less <- reactive({
    which(metadata$treated_sample %in% colnames(rv$ratio)[rv$ratio <= -2])
  })
  
  ## Table Display : Metadata ####
  ### Color the metadata (1 → pink、2 → skyblue) ####
  observeEvent(input$button_metadata, {
    rv$metadata <- datatable(
      metadata,
      selection = "single",
      rownames = FALSE,
      options = list(paging = FALSE,
                     scrollY = "1000px",
                     columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     dom = 'flrtBip', 
                     buttons = list(list(extend = 'collection',
                                         buttons = list(list(extend = 'csv', filename = 'Metadata'),
                                                        list(extend = 'excel', filename = 'Metadata')),
                                         text = 'Download')))) %>% 
      formatStyle(
        "treated_sample", 
        target = "row",
        backgroundColor = styleRow(c(more(), less()), c(rep("pink", length(more())), rep("skyblue", length(less())))
        )
      )
  })
  
  ### Retrieve the metadata under the any following conditions  ####
  #### SRratio ≧ 2 ####
  observeEvent(input$button_metadata_more, {
    rv$metadata <- datatable(
      metadata[more(), ],
      selection = "single",
      rownames = FALSE,
      options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     dom = 'flrtBip', 
                     buttons = list(list(extend = 'collection',
                                         buttons = list(list(extend = 'csv', filename = 'Metadata_detected_up'),
                                                        list(extend = 'excel', filename = 'Metadata_detected_up')),
                                         text = 'Download')))
    )
  })
  
  #### -2 < SRratio < 2 ####
  observeEvent(input$button_metadata_middle, {
    rv$metadata <- datatable(
      metadata[middle(), ],
      selection = "single",
      rownames = FALSE,
      options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     dom = 'flrtBip', 
                     buttons = list(list(extend = 'collection',
                                         buttons = list(list(extend = 'csv', filename = 'Metadata_not_detected'),
                                                        list(extend = 'excel', filename = 'Metadata_not_detected')),
                                         text = 'Download')))
    )
  })
  
  #### SRratio ≦ -2 ####
  observeEvent(input$button_metadata_less, {
    rv$metadata <- datatable(
      metadata[less(), ],
      selection = "single",
      rownames = FALSE,
      options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     dom = 'flrtBip', 
                     buttons = list(list(extend = 'collection',
                                         buttons = list(list(extend = 'csv', filename = 'Metadata_detected_down'),
                                                        list(extend = 'excel', filename = 'Metadata_detected_down')),
                                         text = 'Download')))
    )
  })
  
  ### Display metadata ####
  output$metadata <- renderDataTable({
    rv$metadata
  }, server = FALSE)
}

# Sub Menu : Template Matching ####
TemplateMatchUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(h3(strong("Teplate Matching → Heatmap")), width = 3,
                   h4("1. Select a gene in overview"),
                   selectInput(ns("method"), 
                               label = "Choose a method :",
                               choices = c("euclidean","maximum", "manhattan", 
                                           "canberra", "correlation", "binary"),
                               selected = "euclidean"),
                   numericInput(ns("display"),
                                label = "Number of results to display :",
                                value = 5),
                   h4("2. Control paremeters of heatmap"),
                   selectInput(ns("identifier"),
                               label = "Choose y axis :",
                               choices = c("ensembl_gene_id",
                                           "SYMBOL"),
                               selected = "ensembl_gene_id"),
                   numericInput(ns("height"),
                                label = "height :",
                                value = 400),
                   actionButton(ns("heatmap"), "Plot",
                                style = "color: white; background-color: #337ab7; border-color: #2e6da4")
      ),
      mainPanel(width = 9,
                dataTableOutput(ns("close_genes")),
                dataTableOutput(ns("selected"))
      )
    )
  )
}

TemplateMatch <- function(input, output, session, query, selectRow, srga, cl, Breaks, Color) {
  ## Table Display : Template Matching results ####
  ### Template matching ####
  close_genes <- reactive({
    genefinder(query,
               selectRow(),
               input$display,
               method = input$method)})
  
  ### Display matching result ####
  output$close_genes <- renderDataTable({
    datatable(
      add_column(srga[close_genes()[[1]]$indices, ], 
                 "dists" = round(close_genes()[[1]]$dists, digits = 3),
                 .after = max(which(sapply(srga, is.numeric)))
      ),
      filter = "top",
      selection = "single",
      extensions = c("Buttons", "FixedColumns"),
      options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     scrollX = TRUE, fixedColumns = TRUE,
                     dom = 'lrtBip', buttons = list(list(extend = 'collection',
                                                         buttons = list(list(extend = 'csv', filename = 'closegenes'),
                                                                        list(extend = 'excel', filename = 'closegenes')),
                                                         text = 'Download'))
      ),
      escape = FALSE, rownames = FALSE
    ) %>%
      formatStyle(names(srga[cl]), backgroundColor = styleInterval(Breaks, Color)) %>%
      formatStyle("dists", backgroundColor = "yellow")
  },
  server = FALSE)
  
  
  ### Display template ####
  output$selected <- renderDataTable({
    datatable(
      srga[selectRow(), ],
      filter = "top",
      extensions = c("Buttons", "FixedColumns"),
      selection = "single",
      options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     scrollX = TRUE, 
                     fixedColumns = TRUE,
                     dom = 'lrtBip', buttons = list(list(extend = 'collection',
                                                         buttons = list(list(extend = 'csv', filename = 'template'),
                                                                        list(extend = 'excel', filename = 'template')),
                                                         text = 'Download'))
      ),
      escape = FALSE, rownames = FALSE
    ) %>%
      formatStyle(names(srga[cl]), backgroundColor = styleInterval(Breaks, Color))
  })
  
  output$test <- renderDataTable({})
  
  ## Side Bar : Template Matching → Heatmap ####
  ### Create heatmap ####
  heatmap_tbl <- reactive(
    heatmaply(Filter(is.numeric,
                     set_rownames(srga[close_genes()[[1]]$indices, ], 
                                  value = srga[close_genes()[[1]]$indices, input$identifier])),
              height = input$height,
              grid_gap = 0.2, grid_color = "gray90",
              scale_fill_gradient_fun = scale_fill_gradient2(
                low = "deepskyblue",
                high = "hotpink",
                midpoint = 0
              ),
              Rowv = FALSE,
              Colv = FALSE)
  )
  
  ### Display a heatmap inside the modal dialog box ####
  output$plot <- renderPlotly({
    heatmap_tbl()
  })
  ns <- session$ns
  observeEvent(input$heatmap, {
    showModal(modalDialog({
      plotlyOutput(ns("plot"))},
      easyClose = TRUE,
      size = "l",
      title = "Heatmap")
    )
  })
}
