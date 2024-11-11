shinyServer(function(input, output, session) {
  ## ======================================================== ##
  ## Menu : Microarray
  ## ======================================================== ##
  ## ======================================== ##
  ## Sub Menu : Atlas
  ## ======================================== ##
  atlas_ma <- callModule(overview, "SRscore_microarray", SRscore_microarray, colnames_microarray, microarrayBreaks, microarrayColor, 
                         gokegg, positiveSRscore_ma, negativeSRscore_ma, nonzeroSRscore_ma)
  callModule(stress, "ABAm", ABA_ratiom, SRscore_microarray, atlas_ma$geneid, ABA_metadatam)
  callModule(stress, "coldm", Cold_ratiom, SRscore_microarray, atlas_ma$geneid, Cold_metadatam)
  callModule(stress, "DC3000m", DC3000_ratiom, SRscore_microarray, atlas_ma$geneid, DC3000_metadatam)
  callModule(stress, "droughtm", Drought_ratiom, SRscore_microarray, atlas_ma$geneid, Drought_metadatam)
  callModule(stress, "heatm", Heat_ratiom, SRscore_microarray, atlas_ma$geneid, Heat_metadatam)
  callModule(stress, "highlightm", `High-light_ratiom`, SRscore_microarray, atlas_ma$geneid, `High-light_metadatam`)
  callModule(stress, "hypoxiam", Hypoxia_ratiom, SRscore_microarray, atlas_ma$geneid, Hypoxia_metadatam)
  callModule(stress, "osmoticm", Osmotic_ratiom, SRscore_microarray, atlas_ma$geneid, Osmotic_metadatam)
  callModule(stress, "oxidationm", Oxidation_ratiom, SRscore_microarray, atlas_ma$geneid, Oxidation_metadatam)
  callModule(stress, "saltm", Salt_ratiom, SRscore_microarray, atlas_ma$geneid, Salt_metadatam)
  callModule(stress, "woundm", Wound_ratiom, SRscore_microarray, atlas_ma$geneid, Wound_metadatam)
  ## ======================================== ##
  ## Sub Menu : Template Matching
  ## ======================================== ##
  callModule(TemplateMatch, "microarray", genefinder_microarray, atlas_ma$geneid, SRscore_microarray, 
             colnames_microarray, microarrayBreaks, microarrayColor)

  ## ========================================================= ##
  ## Menu : RNA-Seq
  ## ========================================================= ##
  ## ======================================== ##
  ## Sub Menu : Atlas
  ## ======================================== ##
  atlas_rs <- callModule(overview, "SRscore_rnaseq", SRscore_rnaseq, colnames_rnaseq, rnaseqBreaks, rnaseqColor, 
                         gokegg, positiveSRscore_rs, negativeSRscore_rs, nonzeroSRscore_rs)
  callModule(stress, "ABAr", ABA_ratior, SRscore_rnaseq, atlas_rs$geneid, ABA_metadatar)
  callModule(stress, "coldr", Cold_ratior, SRscore_rnaseq, atlas_rs$geneid, Cold_metadatar)
  callModule(stress, "DC3000r", DC3000_ratior, SRscore_rnaseq, atlas_rs$geneid, DC3000_metadatar)
  callModule(stress, "droughtr", Drought_ratior, SRscore_rnaseq, atlas_rs$geneid, Drought_metadatar)
  callModule(stress, "heatr", Heat_ratior, SRscore_rnaseq, atlas_rs$geneid, Heat_metadatar)
  callModule(stress, "highlightr", `High-light_ratior`, SRscore_rnaseq, atlas_rs$geneid, `High-light_metadatar`)
  callModule(stress, "hypoxiar", Hypoxia_ratior, SRscore_rnaseq, atlas_rs$geneid, Hypoxia_metadatar)
  callModule(stress, "osmoticr", Osmotic_ratior, SRscore_rnaseq, atlas_rs$geneid, Osmotic_metadatar)
  callModule(stress, "oxidationr", Oxidation_ratior, SRscore_rnaseq, atlas_rs$geneid, Oxidation_metadatar)
  callModule(stress, "saltr", Salt_ratior, SRscore_rnaseq, atlas_rs$geneid, Salt_metadatar)
  callModule(stress, "woundr", Wound_ratior, SRscore_rnaseq, atlas_rs$geneid, Wound_metadatar)
  ## ======================================== ##
  ## Sub Menu : Template Matching
  ## ======================================== ##
  callModule(TemplateMatch, "rnaseq", genefinder_rnaseq, atlas_rs$geneid, SRscore_rnaseq,
             colnames_rnaseq, rnaseqBreaks, rnaseqColor)
}) # shinyServer