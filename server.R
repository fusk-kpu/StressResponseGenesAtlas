shinyServer(function(input, output, session) {
  # Atlas ###########################################################################################################
  ## Microarray #####
  ### overview
  select_microarray <- callModule(overview, "SRscore_microarray", SRscore_microarray, colnames_microarray, microarrayBreaks, microarrayColor)
  
  ### each stress
  callModule(stress, "ABAm", ABA_ratiom, SRscore_microarray, select_microarray, ABA_metadatam)
  callModule(stress, "coldm", cold_ratiom, SRscore_microarray, select_microarray, cold_metadatam)
  callModule(stress, "DC3000m", DC3000_ratiom, SRscore_microarray, select_microarray, DC3000_metadatam)
  callModule(stress, "droughtm", drought_ratiom, SRscore_microarray, select_microarray, drought_metadatam)
  callModule(stress, "heatm", heat_ratiom, SRscore_microarray, select_microarray, heat_metadatam)
  callModule(stress, "highlightm", highlight_ratiom, SRscore_microarray, select_microarray, highlight_metadatam)
  callModule(stress, "hypoxiam", hypoxia_ratiom, SRscore_microarray, select_microarray, hypoxia_metadatam)
  callModule(stress, "osmoticm", osmotic_ratiom, SRscore_microarray, select_microarray, osmotic_metadatam)
  callModule(stress, "oxidationm", oxidation_ratiom, SRscore_microarray, select_microarray, oxidation_metadatam)
  callModule(stress, "saltm", salt_ratiom, SRscore_microarray, select_microarray, salt_metadatam)
  callModule(stress, "woundm", wound_ratiom, SRscore_microarray, select_microarray, wound_metadatam)

  ## RNA-Seq #####
  ### overview
  select_rnaseq <- callModule(overview, "SRscore_rnaseq", SRscore_rnaseq, colnames_rnaseq, rnaseqBreaks, rnaseqColor)
  
  ### each stress
  callModule(stress, "ABAr", ABA_ratior, SRscore_rnaseq, select_rnaseq, ABA_metadatar)
  callModule(stress, "coldr", cold_ratior, SRscore_rnaseq, select_rnaseq, cold_metadatar)
  callModule(stress, "DC3000r", DC3000_ratior, SRscore_rnaseq, select_rnaseq, DC3000_metadatar)
  callModule(stress, "droughtr", drought_ratior, SRscore_rnaseq, select_rnaseq, drought_metadatar)
  callModule(stress, "heatr", heat_ratior, SRscore_rnaseq, select_rnaseq, heat_metadatar)
  callModule(stress, "highlightr", highlight_ratior, SRscore_rnaseq, select_rnaseq, highlight_metadatar)
  callModule(stress, "hypoxiar", hypoxia_ratior, SRscore_rnaseq, select_rnaseq, hypoxia_metadatar)
  callModule(stress, "osmoticr", osmotic_ratior, SRscore_rnaseq, select_rnaseq, osmotic_metadatar)
  callModule(stress, "oxidationr", oxidation_ratior, SRscore_rnaseq, select_rnaseq, oxidation_metadatar)
  callModule(stress, "salt", salt_ratior, SRscore_rnaseq, select_rnaseq, salt_metadatar)
  callModule(stress, "wound", wound_ratior, SRscore_rnaseq, select_rnaseq, wound_metadatar)
  
  ## help popup ####
  observeEvent(input$select, {
    showModal(modalDialog(
      title = strong("ヘルプ"),
      HTML(
        paste0(
          "①ストレス応答遺伝子群アトラスから任意の遺伝子をクリック選択した後、
      いずれかのストレス名タブを開いてください。<br><br>
      ②'Show SRratio'ボタンをクリックすると、
      非ストレス処理サンプル群およびストレス処理サンプル群の発現データから算出された遺伝子発現比が表示されます。
      SRratioが2以上の場合はピンク、-2以下の場合は水色で表示されます。<br><br>
      ③'Show metadata'ボタンをクリックすると、
      研究およびサンプルのアクセッションIDと実験条件に関連する情報をまとめたテーブルを表示します。
      treated_sample列は②で表示したテーブルのカラム名に相当し、色も対応づけられています。", 
          icon("filter"), "ボタンをクリックすると、行の色でフィルタリングすることができます。")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # TemplateMatch ##################################################################################################
  callModule(TemplateMatch, "microarray", genefinder_microarray, select_microarray, SRscore_microarray, colnames_microarray, microarrayBreaks, microarrayColor)
  callModule(TemplateMatch, "rnaseq", genefinder_rnaseq, select_rnaseq, SRscore_rnaseq, colnames_rnaseq, rnaseqBreaks, rnaseqColor)
  
  # Unknown ########################################################################################################
  callModule(Unknown, "microarray", SRscore_microarray, colnames_microarray, microarrayBreaks, microarrayColor)
  callModule(Unknown, "rnaseq", SRscore_rnaseq, colnames_rnaseq, rnaseqBreaks, rnaseqColor)
  })