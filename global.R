# Source scripts
# source("proxy.R", local = TRUE)
source("setup.R", local = TRUE)
source("data_processing.R", local = TRUE)


# Sub Menu : Atlas ####
## Tab : Overview ####
overviewUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      actionButton(ns("clear"), "Clear Selected Rows"),
      actionButton(ns("reset"), "Reset Table"),
      actionButton(ns("remove"), "Remove",
                   style = "color: red"),
      br(),
      br(),
      dataTableOutput(ns("atlas"))
      ),
      br(),
      br(),
    fluidRow(
      column(width = 4,
             box(title = "Bulk Search",
                 width = 12,
                 status = "primary",
                 solidHeader = TRUE,
                 textAreaInput(ns("text"), h4("Input list of identifiers :"),
                               width = "400px", height = "300px"
                 ),
                 actionButton(ns("submit"), "Submit",
                              style = "color: white; background-color: #337ab7; border-color: #2e6da4"),
                 actionButton(ns("example1"), "Example1"),
                 actionButton(ns("example2"), "Example2"),
                 br(),
                 actionButton(ns("selected"), "Entries shown above"))),
      column(width = 4,
             box(title = "Bulk Search → Heatmap",
                 width = 12,
                 status = "success",
                 solidHeader = TRUE,
                 selectInput(ns("identifier"),
                             label = "Choose y axis :",
                             choices = c("ensembl_gene_id",
                                         "SYMBOL"),
                             selected = "ensembl_gene_id"),
                 numericInput(ns("height"),
                              label = "height :",
                              value = 400),
                 actionButton(ns("heatmap"), "Plot",
                              style = "color: white; background-color: #337ab7; border-color: #2e6da4"))),
      column(width = 4,
             box(title = "Bulk Search → Enrichment analysis",
                 width = 12, 
                 status = "warning",
                 solidHeader = TRUE,
                 radioButtons(ns("source"),
                              "Include gene sets from AtSRGA :",
                              c("positiveSRscore (SRscore ≧ 1) .",
                                "negativeSRscore (SRscore ≦ -1) .",
                                "both of the above."), selected = ""),
                 actionButton(ns("reset2"), "Reset"),
                 br(),
                 br(),
                 br(),
                 actionButton(ns("analysis"), "Analysis",
                              style = "color: white; background-color: #337ab7; border-color: #2e6da4"),
                 br(),
                 selectInput(ns("xaxis"), 
                             label = "Choose x-axis :",
                             choices = c("GeneRatio","Count", "pvalue", 
                                         "p.adjust", "qvalue"),
                             selected = "GeneRatio"),
                 numericInput(ns("showCategory"),
                              label = "Set number of categories to display :",
                              value = 10),
                 numericInput(ns("labelFormat"),
                              label = "Set wrap length :",
                              value = 100),
                 numericInput(ns("dotplot_height"),
                              label = "Adjust height of plot :",
                              value = 500)
                 )
             )
      ),
    plotOutput(ns("result"))
    )
}

overview <- function(input, output, session, srga, cl, Breaks, Color, prop, positive, negative, nonzero) {
  ##　Table Display : SRGA (Stress Response Gene Atlas) ####
  ### Display the atlas as a heatmap ####
  rv  <- reactiveValues(df = srga)
  
  observeEvent(input$reset, {
    rv$df <- srga
  })
  
  output$atlas <- renderDataTable({
    datatable(
      rv$df,
      filter = "top",
      selection = "multiple",
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
                            buttons = list(list(extend = "excel", filename = "SRGA")
                                           ),
                            text = "Download"))
      )
    ) %>%
      formatStyle(names(srga[cl]), backgroundColor = styleInterval(Breaks, Color))
  }, server = TRUE)
  
  ## Side Bar : Bulk Search → Heatmap ####
  ### Input string ####
  #### Lowercase conversion and delimiter splitting（Space or New Line or Tab or commma）####
  lower_input <- reactive({
    gsub("\"", "", strsplit(tolower(input$text), " |\n|\t|,")[[1]])
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
  
  ## clear selected rows by pressing the button ####
  observeEvent(input$clear, {
    selectRows(dataTableProxy("atlas"), NULL)
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
  observeEvent(input$selected, {
    updateTextAreaInput(session, "text", value = rv$df$ensembl_gene_id[input$atlas_rows_all])
  })
  
  ### Create heatmap ####
  heatmap_tbl <- reactive(
    heatmaply(set_rownames(rv$df[cl], value = rv$df[, input$identifier]),
              height = input$height,
              grid_gap = 0.2, grid_color = "gray90",
              scale_fill_gradient_fun = scale_fill_gradient2(
                low = "deepskyblue",
                high = "hotpink",
                midpoint = 0
              ),
              Rowv = FALSE,
              Colv = FALSE,
              cellnote = set_rownames(rv$df[cl], value = rv$df[, input$identifier]),
              cellnote_size = 18,
              cellnote_textposition = "middle center")
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
      title = "Heatmap"))
  })
  
  ## 指定した範囲内のSRscoreを持つ遺伝子のID抽出
  query <- reactive({
    srga$ensembl_gene_id[which(lapply(matched_AGIandSYMBOL(), any) == TRUE)]
  })
  
  ## カスタムデータのoff
  observeEvent(input$reset2, {
    updateRadioButtons(session, 
                       inputId = "source",
                       choices = c("positiveSRscore (SRscore ≧ 1)",
                                   "negativeSRscore (SRscore ≦ -1)"),
                       selected = "")
  })
  
  observeEvent(input$analysis, {
    withProgress(message = "", {
      if (is.null(input$source)) {
        rv$earesult <- enrichGO(gene = query(),
                                TERM2GENE = gokegg,
                                pvalueCutoff = 0.05,
                                maxGSSize = 2000)
      } else if (input$source == "positiveSRscore (SRscore ≧ 1)") {
        rv$earesult <- enricher(gene = query(),
                                TERM2GENE = positive,
                                pvalueCutoff = 0.05,
                                maxGSSize = 2000)
      } else if (input$source == "negativeSRscore (SRscore ≦ -1)") {
        rv$earesult <- enricher(gene = query(),
                                TERM2GENE = negative,
                                pvalueCutoff = 0.05,
                                maxGSSize = 2000)
      } else if (input$source == "both of the above.") {
        rv$earesult <- enricher(gene = query(),
                                TERM2GENE = nonzero,
                                pvalueCutoff = 0.05,
                                maxGSSize = 2000)
      }
      for (i in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.1)
      }
    })
  })
  
  output$result <- renderPlot({
    dotplot(rv$earesult,
            showCategory = input$showCategory,
            label_format = input$labelFormat, 
            x = input$xaxis) +
      theme( 
        legend.text=element_text(size = 12))
  }, height = reactive(input$dotplot_height))
  
  return(
    list(geneid = reactive(rv$df$ensembl_gene_id[input$atlas_rows_selected]))
  )
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
  rv <- reactiveValues(ratio = NULL, metadata = NULL)
  
  ### Retrieve the SR ratio of the specified gene ####
  selectedRatio <- reactive({
    ratio[which(ratio$ensembl_gene_id %in% selectedRow()), ]
  })
  observeEvent(input$button_ratio, {
    rv$ratio <- selectedRatio()
  })
  
  ### Display SRratio ####
  output$ratio <- renderDataTable({
    num_cl <- which(sapply(rv$ratio, is.numeric))
    rv$ratio[, num_cl] <- round(rv$ratio[, num_cl], digits = 2)
    datatable(rv$ratio,
              extensions = c("Buttons", "FixedColumns"),
              rownames = FALSE,
              selection = "single",
              options = list(scrollX =TRUE,
                             fixedColumns = TRUE,
                             dom = 'lrtBip',
                             buttons = list(list(extend = 'collection',
                                                 buttons = list(list(extend = 'excel', filename = 'Metadata')),
                                                 text = 'Download')))) %>%
      formatStyle(colnames(selectedRatio())[-1], backgroundColor = styleInterval(c(-2, 2), c("skyblue", "white", "pink")))
  })
  
  ### Identify the treated samples that meet the following criteria ####
  
  #### Specify a gene in the SRratio table
  ratio_single <- reactive({
    rv$ratio[input$ratio_rows_selected, ]
  })
  
  #### SRratio ≧ 2 (1) ####
  more <- reactive({
    which(metadata$treated_sample %in% colnames(ratio_single())[ratio_single() >= 2])
  })
  
  #### -2 < SRratio < 2 ####
  middle <- reactive({
    which(metadata$treated_sample %in% colnames(ratio_single())[-2 <= ratio_single() & ratio_single() <= 2])
  })
  
  #### SRratio ≦ -2 (2) ####
  less <- reactive({
    which(metadata$treated_sample %in% colnames(ratio_single())[ratio_single() <= -2])
  })
  
  ## Table Display : Metadata ####
  ### Color metadata (1 → pink、2 → skyblue) ####
  observeEvent(input$button_metadata, {
    if (length(more()) == 0 & length(less()) == 0) {
      rv$metadata <- datatable(
        metadata,
        selection = "single",
        escape = FALSE,
        rownames = FALSE,
        options = list(paging = FALSE,
                       scrollY = "1000px",
                       columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                       dom = 'flrtBip', 
                       buttons = list(list(extend = 'collection',
                                           buttons = list(list(extend = 'excel', filename = 'Metadata')),
                                           text = 'Download'))))
    } else {
    rv$metadata <- datatable(
      metadata,
      selection = "single",
      escape = FALSE,
      rownames = FALSE,
      options = list(paging = FALSE,
                     scrollY = "1000px",
                     columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     dom = 'flrtBip', 
                     buttons = list(list(extend = 'collection',
                                         buttons = list(list(extend = 'excel', filename = 'Metadata')),
                                         text = 'Download')))) %>% 
      formatStyle(
        "treated_sample", 
        target = "row",
        backgroundColor = styleRow(c(more(), less()), c(rep("pink", length(more())), rep("skyblue", length(less())))
        )
      )
  }
  })
  
  ### SRratio ≧ 2のストレス処理サンプルのみを含むメタデータの取得 ####
  observeEvent(input$button_metadata_more, {
    rv$metadata <- datatable(
      metadata[more(), ],
      selection = "single",
      escape = FALSE,
      rownames = FALSE,
      options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     dom = 'flrtBip', 
                     buttons = list(list(extend = 'collection',
                                         buttons = list(list(extend = 'excel', filename = 'Metadata_detected_up')),
                                         text = 'Download')))
    )
  })
  
  ### -2 < SRratio < 2のストレス処理サンプルのみを含むメタデータの取得 ####
  observeEvent(input$button_metadata_middle, {
    rv$metadata <- datatable(
      metadata[middle(), ],
      selection = "single",
      escape = FALSE,
      rownames = FALSE,
      options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     dom = 'flrtBip', 
                     buttons = list(list(extend = 'collection',
                                         buttons = list(list(extend = 'excel', filename = 'Metadata_not_detected')),
                                         text = 'Download')))
    )
  })
  
  ### SRratio ≦ -2のストレス処理サンプルのみを含むメタデータの取得 ####
  observeEvent(input$button_metadata_less, {
    rv$metadata <- datatable(
      metadata[less(), ],
      selection = "single",
      escape = FALSE,
      rownames = FALSE,
      options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     dom = 'flrtBip', 
                     buttons = list(list(extend = 'collection',
                                         buttons = list(list(extend = 'excel', filename = 'Metadata_detected_down')),
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
    box(title = "Template Matching → Heatmap",
        width = 4,
        status = "danger",
        solidHeader = TRUE,
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
                    choices = c("ensembl_gene_id", "SYMBOL"),
                    selected = "ensembl_gene_id"),
        numericInput(ns("height"),
                     label = "height :",
                     value = 400),
        actionButton(ns("heatmap"), "Plot",
                     style = "color: white; background-color: #337ab7; border-color: #2e6da4")
        ),
    dataTableOutput(ns("close_genes")),
    br(),
    br(),
    dataTableOutput(ns("template"))
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
                                                          buttons = list(list(extend = 'excel', filename = 'closegenes')),
                                                          text = 'Download'))
                     ),
      escape = FALSE, rownames = FALSE
      ) %>%
      formatStyle(names(srga[cl]), backgroundColor = styleInterval(Breaks, Color)) %>%
      formatStyle("dists", backgroundColor = "yellow")
  },
  server = FALSE)
  
  ### Display template ####
  output$template <- renderDataTable({
    datatable(
      srga[srga$ensembl_gene_id == selectRow(), ],
      extensions = "FixedColumns",
      selection = "single",
      options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     scrollX = TRUE, 
                     fixedColumns = TRUE,
                     dom = 'rti'
                     ),
      escape = FALSE, rownames = FALSE
      ) %>%
      formatStyle(names(srga[cl]), backgroundColor = styleInterval(Breaks, Color))
  })
  
  ## Side Bar : Template Matching → Heatmap ####
  ### Create heatmap ####
  heatmap_tbl <- reactive(
    heatmaply(set_rownames(srga[close_genes()[[1]]$indices, cl], 
                           value = srga[close_genes()[[1]]$indices, input$identifier]),
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