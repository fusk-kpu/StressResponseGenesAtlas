shinyUI(
  navbarPage("SRGA",
             navbarMenu("Microarray",
                        tabPanel("Atlas",
                                 actionButton("select", "", icon("circle-info"),
                                              style = "color: white; background-color: #337ab7; border-color: #2e6da4; position: absolute; right: 10px"
                                              ),
                                 tabsetPanel(type = "tabs",
                                             tags$style(type = "text/css",
                                                        ".shiny-output-error { visibility: hidden; }",
                                                        ".shiny-output-error:before { visibility: hidden; }"
                                                        ),
                                             tabPanel("Overview", overviewUI("SRscore_microarray")),
                                             tabPanel("ABA", stressUI("ABAm")),
                                             tabPanel("Cold", stressUI("coldm")),
                                             tabPanel("DC3000", stressUI("DC3000m")),
                                             tabPanel("Drought", stressUI("droughtm")),
                                             tabPanel("Heat", stressUI("heatm")),
                                             tabPanel("Highlight", stressUI("highlightm")),
                                             tabPanel("Hypoxia", stressUI("hypoxiam")),
                                             tabPanel("Osmotic", stressUI("osmoticm")),
                                             tabPanel("Oxidation", stressUI("oxidationm")),
                                             tabPanel("Salt", stressUI("saltm")),
                                             tabPanel("Wound", stressUI("woundm"))
                                             )
                                 ),
                        tabPanel("Template matching", TemplateMatchUI("microarray")),
                        tabPanel("Unknown", UnknownUI("microarray"))
                        ),
             navbarMenu("RNA-Seq",
                        tabPanel("Atlas",
                                 actionButton("select", "", icon("circle-info"),
                                              style = "color: white; background-color: #337ab7; border-color: #2e6da4; position: absolute; right: 10px"
                                 ),
                                 tabsetPanel(type = "tabs",
                                             tags$style(type = "text/css",
                                                        ".shiny-output-error { visibility: hidden; }",
                                                        ".shiny-output-error:before { visibility: hidden; }"
                                             ),
                                             tabPanel("Overview", overviewUI("SRscore_rnaseq")),
                                             tabPanel("ABA", stressUI("ABAr")),
                                             tabPanel("Cold", stressUI("coldr")),
                                             tabPanel("DC3000", stressUI("DC3000r")),
                                             tabPanel("Drought", stressUI("droughtr")),
                                             tabPanel("Heat", stressUI("heatr")),
                                             tabPanel("Highlight", stressUI("highlightr")),
                                             tabPanel("Hypoxia", stressUI("hypoxiar")),
                                             tabPanel("Osmotic", stressUI("osmoticr")),
                                             tabPanel("Oxidation", stressUI("oxidationr")),
                                             tabPanel("Salt", stressUI("saltr")),
                                             tabPanel("Wound", stressUI("woundr"))
                                             )
                        ),
                        tabPanel("Template matching", TemplateMatchUI("rnaseq")),
                        tabPanel("Unknown", UnknownUI("rnaseq"))
                        )
             )
  )