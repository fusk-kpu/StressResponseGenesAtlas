shinyUI(
  navbarPage(
    "SRGA",
    
  ## ======================================================== ##
  ## Menu : Microarray
  ## ======================================================== ##
    navbarMenu(
      "Microarray",
      ## ======================================== ##
      ## Sub Menu : Atlas
      ## ======================================== ##
      tabPanel(
        "Atlas",
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
                    ) # tabsetPanel
        ), # tabPanel(Atlas)
      ## ======================================== ##
      ## Sub Menu : Template Matching
      ## ======================================== ##
      tabPanel("Template matching",
               TemplateMatchUI("microarray")),
      ## ======================================== ##
      ## Sub Menu : Unknown
      ## ======================================== ##
      tabPanel("Unknown", 
               UnknownUI("microarray"))
      ),
  ## ======================================================== ##
  ## Menu : RNA-Seq 
  ## ======================================================== ##
    navbarMenu(
      "RNA-Seq",
      ## ======================================== ##
      ## Sub Menu : Atlas
      ## ======================================== ##
      tabPanel(
        "Atlas",
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
                    ) # tabsetPanel
        ), # tabPanel (Atlas)
      ## ======================================== ##
      ## Sub Menu :Template Matching
      ## ======================================== ##
      tabPanel("Template matching", TemplateMatchUI("rnaseq")),
      ## ======================================== ##
      ## Sub Menu : Unknown
      ## ======================================== ##
      tabPanel("Unknown", UnknownUI("rnaseq"))
      ) # navbarMenu

    ) # navbarPage
  ) # ShinyUI