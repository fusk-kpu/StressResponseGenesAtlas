# Installation
RPackages <- c("shiny", "dplyr", "tibble", "ggplot2", "DT", "withr", "targets", "htmlwidgets", "plotly", "heatmaply", "magrittr") 
newPackages <- RPackages[!(RPackages %in% installed.packages()[, "Package"])]
if (length(newPackages)) {
  install.packages(newPackages)
}
for (package in RPackages) {
  library(package, character.only = TRUE)
}

BCPackages <- c("genefilter")
newPackages <- BCPackages[!(BCPackages %in% installed.packages()[, "Package"])]
if (length(newPackages)) {
  
  if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  
  BiocManager::install(newPackages)
}
for (package in BCPackages) {
  library(package, character.only = TRUE)
}

# Source scripts
source("data_processing.R", local = TRUE)




# Sub Menu : Atlas ####
## Tab : Overview ####
overviewUI <- function(id) {
  ns <- NS(id)
  tagList(
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
                   "1. Select rows in the table to the right.",
                   br(),
                   "2. Click this button :",
                   actionButton(ns("remove"), "Remove",
                                style = "color: red"),
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
                   h5("Uncheck if dendrogram is not needed :"),
                   checkboxInput(ns("dendro_row"),
                                 label = "Row",
                                 value = TRUE),
                   checkboxInput(ns("dendro_col"),
                                 label = "Column",
                                 value = TRUE),
                   actionButton(ns("heatmap"), "Plot",
                                style = "color: white; background-color: #337ab7; border-color: #2e6da4")
      ),
      mainPanel(
        dataTableOutput(ns("atlas_pick"))
      )
    )
  )
}

overview <- function(input, output, session, srga, cl, Breaks, Color) {
  ##　Table Display : SRGA (Stress Response Gene Atlas) ####
  ### SRGAをヒートマップにして表示 ####
  output$atlas <- renderDataTable({
    datatable(
      srga,
      filter = "top",
      selection = "single",
      extensions = "Buttons",
      rownames = FALSE,
      escape = FALSE,
      options = list(columnDefs = list(list(className = "dt-nowrap", targets = "_all")),
                     dom = "lfrtBip", buttons = list(list(extend = "collection",
                                                          buttons = list(list(extend = "csv", filename = "SRGA"),
                                                                         list(extend = "excel", filename = "SRGA")),
                                                          text = "Download"))
      )
    ) %>%
      formatStyle(names(srga[cl]), backgroundColor = styleInterval(Breaks, Color))
  }, server = TRUE)
  
  ## Side Bar : Bulk Search → Heatmap ####
  rv  <- reactiveValues(df = NULL)
  
  ### 入力文字列の小文字変換および区切り文字分割（スペース or 改行 or タブ or コンマ）####
  lower_input <- reactive({
    strsplit(tolower(input$text), " |\n|\t|,")[[1]]
  })
  
  ### AGIコードの要素番号を取得 ####
  agi_index <- reactive({
    grepl("at.g.+", lower_input(), ignore.case = T)
  })
  
  ### AGIコードのバージョン部分（例：ATXGXXXX.1）を消去 ####
  key <- reactive({
    c(gsub("\\..+", "", lower_input()[agi_index()]), lower_input()[!agi_index()])
  })
  
  ### SRGAから入力文字列と完全一致する要素を含む行を取得 ####
  lap <- reactive(lapply(lapply(strsplit(apply(srga, 1, paste, collapse = "/"), "/"), tolower), match, key()[which(key() != "")]))
  observeEvent(input$submit, {
    rv$df <- srga[which(lapply(lap(), any) == TRUE), ]
  })
  
  ### 取得行の表示 ####
  output$atlas_pick <- renderDataTable({
    datatable(
      rv$df,
      filter = "top",
      extensions = "Buttons",
      rownames = FALSE,
      escape = FALSE,
      options = list(columnDefs = list(list(className = "dt-nowrap", targets = "_all")),
                     dom = "lfrtBip", buttons = list(list(extend = "collection",
                                                          buttons = list(list(extend = "csv", filename = "SRGA"),
                                                                         list(extend = "excel", filename = "SRGA")),
                                                          text = "Download"))
      )
    ) %>%
      formatStyle(names(srga[cl]), backgroundColor = styleInterval(Breaks, Color))
  }, server = TRUE)
  
  ### 選択した行の消去 ####
  observeEvent(input$remove, {
    rv$df <- rv$df[-as.numeric(input$atlas_pick_rows_selected), ]
  })
  
  ### 入力文字列の例 ####
  #### その１ ####
  observeEvent(input$example1, {
    name <- paste("ADH1", "DREB1A", "ELIP1", "HSP17.8", "JAZ1", "NCED3", "PP2CA")
    updateTextAreaInput(session, "text", value = name)
  })
  #### その２ ####
  observeEvent(input$example2, {
    name <- paste("DREB1A", "DREB1B", "DREB1C", "DREB2A", "DREB2B", "GolS1", "GolS2", "GolS3", sep = "\n")
    updateTextAreaInput(session, "text", value = name)
  })
  
  ### ヒートマップ図の描画 ####
  heatmap_tbl <- reactive(
    heatmaply(Filter(is.numeric,
                     set_rownames(rv$df, value = rv$df[, input$name])),
              height = input$height,
              grid_gap = 0.2, grid_color = "gray90",
              scale_fill_gradient_fun = scale_fill_gradient2(
                low = "deepskyblue",
                high = "hotpink",
                midpoint = 0
              ),
              Rowv = input$dendro_row,
              Colv = input$dendro_col,
              cellnote = Filter(is.numeric,
                                set_rownames(rv$df, value = rv$df[, input$name])),
              cellnote_size = 18,
              cellnote_textposition = "middle center")
  )
  
  ### ヒートマップ図をモーダルダイアログ内に表示 ####
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
  
  
  return(reactive(input$atlas_rows_selected))
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
  ### 指定した遺伝子のSRratioを取得 (改善点あり) ####
  selectedRatio <- reactive({
    ratio[which(ratio$ensembl_gene_id %in% srga$ensembl_gene_id[selectedRow()]), ]
  })
  
  rv <- reactiveValues(ratio = NULL, metadata = NULL)

  observeEvent(input$button_ratio, {
    rv$ratio <- ratio[which(ratio$ensembl_gene_id %in% srga$ensembl_gene_id[selectedRow()]), ]
  })
  
  ### SRratioの表示 ####
  output$ratio <- renderDataTable({
    datatable(rv$ratio,
              extensions = "FixedColumns",
              rownames = FALSE,
              selection = "single",
              options = list(scrollX =TRUE, fixedColumns = TRUE)) %>%
      formatStyle(colnames(selectedRatio())[-1], backgroundColor = styleInterval(c(-2, 2), c("skyblue", "white", "pink")))
  })
  
  ### SRratio ≧ 2のストレス処理サンプルを特定 (1) ####
  more <- reactive({
    which(metadata$treated_sample %in% colnames(rv$ratio)[rv$ratio >= 2])
  })
  
  ### -2 < SRratio < 2のストレス処理サンプルを特定 ####
  middle <- reactive({
    which(metadata$treated_sample %in% colnames(rv$ratio)[-2 <= rv$ratio & rv$ratio <= 2])
  })
  
  ### SRratio ≦ -2のストレス処理サンプルを特定 (2) ####
  less <- reactive({
    which(metadata$treated_sample %in% colnames(rv$ratio)[rv$ratio <= -2])
  })
  
  ## Table Display : Metadata ####
  ### メタデータの色分け(1 → pink、2 → skyblue) ####
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
                                         buttons = list(list(extend = 'csv', filename = 'SRGA'),
                                                        list(extend = 'excel', filename = 'SRGA')),
                                         text = 'Download')))) %>% 
      formatStyle(
        "treated_sample", 
        target = "row",
        backgroundColor = styleRow(c(more(), less()), c(rep("pink", length(more())), rep("skyblue", length(less())))
        )
      )
  })
  
  ### SRratio ≧ 2のストレス処理サンプルのみを含むメタデータの取得 ####
  observeEvent(input$button_metadata_more, {
    rv$metadata <- datatable(
      metadata[more(), ],
      selection = "single",
      rownames = FALSE,
      options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     dom = 'flrtBip', 
                     buttons = list(list(extend = 'collection',
                                         buttons = list(list(extend = 'csv', filename = 'SRGA'),
                                                        list(extend = 'excel', filename = 'SRGA')),
                                         text = 'Download')))
    )
  })
  
  ### -2 < SRratio < 2のストレス処理サンプルのみを含むメタデータの取得 ####
  observeEvent(input$button_metadata_middle, {
    rv$metadata <- datatable(
      metadata[middle(), ],
      selection = "single",
      rownames = FALSE,
      options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     dom = 'flrtBip', 
                     buttons = list(list(extend = 'collection',
                                         buttons = list(list(extend = 'csv', filename = 'SRGA'),
                                                        list(extend = 'excel', filename = 'SRGA')),
                                         text = 'Download')))
    )
  })
  
  ### SRratio ≦ -2のストレス処理サンプルのみを含むメタデータの取得 ####
  observeEvent(input$button_metadata_less, {
    rv$metadata <- datatable(
      metadata[less(), ],
      selection = "single",
      rownames = FALSE,
      options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     dom = 'flrtBip', 
                     buttons = list(list(extend = 'collection',
                                         buttons = list(list(extend = 'csv', filename = 'SRGA'),
                                                        list(extend = 'excel', filename = 'SRGA')),
                                         text = 'Download')))
    )
  })
  
  ### メタデータの表示 ####
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
  ### SRGAからテンプレートマッチングで検出した行を取得 ####
  close_genes <- reactive({
    genefinder(query,
               selectRow(),
               input$display,
               method = input$method)})
  
  ### 取得行の表示 ####
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
                     dom = 'lfrtBip', buttons = list(list(extend = 'collection',
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
  
  ### SRGAでユーザが選択した行を表示 ####
  output$selected <- renderDataTable({
    datatable(
      srga[selectRow(), ],
      filter = "top",
      extensions = "FixedColumns",
      selection = "single",
      options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     scrollX = TRUE, fixedColumns = TRUE),
      escape = FALSE, rownames = FALSE
    ) %>%
      formatStyle(names(srga[cl]), backgroundColor = styleInterval(Breaks, Color))
  })
  
  ## Side Bar : Template Matching → Heatmap ####
  ### ヒートマップ図の描画 ####
  heatmap_tbl <- reactive(
    heatmaply(Filter(is.numeric,
                     set_rownames(srga[close_genes()[[1]]$indices, ], 
                                  value = srga[close_genes()[[1]]$indices, input$name])),
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
  
  ### ヒートマップ図をモーダルダイアログ内に表示 ####
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




# Sub Menu : Unknown ####
UnknownUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("clear"), "Clear Selected Rows"),
    actionButton(ns("select_all"), "Select All"),
    dataTableOutput(ns("unknown_genes")),
    actionButton(ns("select_button"), "Select"),
    actionButton(ns("delete"), "Delete Rows"),
    dataTableOutput(ns("selected_genes"))
  )
}
Unknown <- function(input, output, session, srga, cl, Breaks, Color) {
  ## identify gene ids that start with "AT\\dG" and end with "0" ####
  unknown_genes <- reactive({
    srga[grep("AT\\dG.*0$", srga$SYMBOL), ]
  })
  
  ## clear selected rows when you press the button ####
  observeEvent(input$clear, {
    selectRows(dataTableProxy("unknown_genes"), NULL)
  })
  
  ## select all rows when you press the button ####
  observeEvent(input$select_all, {
    selectRows(dataTableProxy("unknown_genes"), input$unknown_genes_rows_all)
  })
  
  ## show datatable for unknown genes ####
  output$unknown_genes <- renderDataTable({
    datatable(unknown_genes(),
              filter = "top",
              extensions = "FixedColumns",
              options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                             scrollX = TRUE, fixedColumns = TRUE),
              escape = FALSE, rownames = FALSE
    ) %>%
      formatStyle(names(srga[cl]), backgroundColor = styleInterval(Breaks, Color))
  })
  
  ## for removing the selected rows ####
  values <- reactiveValues(
    df = NULL
  )
  
  observeEvent(input$select_button, {
    values$df <- unknown_genes()[input$unknown_genes_rows_selected, ]
  })
  
  observeEvent(input$delete, {
    values$df <- values$df[-as.numeric(input$selected_genes_rows_selected), ]
  })
  
  output$selected_genes <- renderDataTable({
    datatable(
      values$df,
      extensions = "FixedColumns",
      options = list(columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
                     scrollX = TRUE, fixedColumns = TRUE,
                     dom = 'lfrtBip', buttons = list(list(extend = 'collection',
                                                          buttons = list(list(extend = 'csv', filename = 'pick_up'),
                                                                         list(extend = 'excel', filename = 'pick_up')),
                                                          text = 'Download'))
      ),
      escape = FALSE, rownames = FALSE
    ) %>%
      formatStyle(names(srga[cl]), backgroundColor = styleInterval(Breaks, Color))
  }, server = FALSE)
  
  return(reactive(input$selected_genes_rows_selected))
}