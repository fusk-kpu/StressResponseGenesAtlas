shinyServer(function(input, output, session) {
  ## ======================================================== ##
  ## Menu : Microarray
  ## ======================================================== ##
  ## ======================================== ##
  ## Sub Menu : Atlas
  ## ======================================== ##
  select_microarray <- callModule(overview, "SRscore_microarray", SRscore_microarray, colnames_microarray, microarrayBreaks, microarrayColor)
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
  ## ======================================== ##
  ## Sub Menu : Template Matching
  ## ======================================== ##
  callModule(TemplateMatch, "microarray", genefinder_microarray, select_microarray, SRscore_microarray, colnames_microarray, microarrayBreaks, microarrayColor)
  ## ======================================== ##
  ## Sub Menu : Enrichment Analysis
  ## ======================================== ##
  callModule(Enrich, "microarray", SRscore_microarray, colnames_microarray, microarrayBreaks, microarrayColor)
  
  ## ========================================================= ##
  ## Menu : RNA-Seq
  ## ========================================================= ##
  ## ======================================== ##
  ## Sub Menu : Atlas
  ## ======================================== ##
  select_rnaseq <- callModule(overview, "SRscore_rnaseq", SRscore_rnaseq, colnames_rnaseq, rnaseqBreaks, rnaseqColor)
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
  ## ======================================== ##
  ## Sub Menu : Template Matching
  ## ======================================== ##
  callModule(TemplateMatch, "rnaseq", genefinder_rnaseq, select_rnaseq, SRscore_rnaseq, colnames_rnaseq, rnaseqBreaks, rnaseqColor)
  ## ======================================== ##
  ## Sub Menu : Enrichment Analysis
  ## ======================================== ##
  callModule(Enrich, "rnaseq", SRscore_rnaseq, colnames_rnaseq, rnaseqBreaks, rnaseqColor)
}) # shinyServer