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
                                             tabPanel("overview", overviewUI("SRscore_microarray")),
                                             tabPanel("ABA", stressUI("ABAm")),
                                             tabPanel("cold", stressUI("coldm")),
                                             tabPanel("DC3000", stressUI("DC3000m")),
                                             tabPanel("drought", stressUI("droughtm")),
                                             tabPanel("heat", stressUI("heatm")),
                                             tabPanel("highlight", stressUI("highlightm")),
                                             tabPanel("hypoxia", stressUI("hypoxiam")),
                                             tabPanel("osmotic", stressUI("osmoticm")),
                                             tabPanel("oxidation", stressUI("oxidationm")),
                                             tabPanel("salt", stressUI("saltm")),
                                             tabPanel("wound", stressUI("woundm"))
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
                                             tabPanel("overview", overviewUI("SRscore_rnaseq")),
                                             tabPanel("ABA", stressUI("ABAr")),
                                             tabPanel("cold", stressUI("coldr")),
                                             tabPanel("DC3000", stressUI("DC3000r")),
                                             tabPanel("drought", stressUI("droughtr")),
                                             tabPanel("heat", stressUI("heatr")),
                                             tabPanel("highlight", stressUI("highlightr")),
                                             tabPanel("hypoxia", stressUI("hypoxiar")),
                                             tabPanel("osmotic", stressUI("osmoticr")),
                                             tabPanel("oxidation", stressUI("oxidationr")),
                                             tabPanel("salt", stressUI("saltr")),
                                             tabPanel("wound", stressUI("woundr"))
                                             )
                        ),
                        tabPanel("Template matching", TemplateMatchUI("rnaseq")),
                        tabPanel("Unknown", UnknownUI("rnaseq"))
                        )
             )
  )