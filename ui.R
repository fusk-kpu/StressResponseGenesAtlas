ui <- dashboardPage(
  
  dashboardHeader(title = "AtSRGA"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Microarray", tabName = "Microarray",
               menuSubItem("Atlas", tabName = "Atlasm"),
               menuSubItem("Template Matching", tabName = "TemplateMatchingm"),
               menuSubItem("Enrichment Analysis", tabName = "EnrichmentAnalysism")
      ),
      menuItem("RNA-Seq", tabName = "RNA-Seq",
               menuSubItem("Atlas", tabName = "Atlasr"),
               menuSubItem("Template Matching", tabName = "TemplateMatchingr"),
               menuSubItem("Enrichment Analysis", tabName = "EnrichmentAnalysisr")
      )
    )
  ),
  
  dashboardBody(
    tags$style(type = "text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    tabItems(
      ## ======================================================== ##
      ## Menu : Home
      ## ======================================================== ##
      tabItem(tabName = "Home",
              titlePanel("Wellcome to AtSRGA")
      ),    
      ## ======================================================== ##
      ## Menu : Microarray
      ## ======================================================== ##
      ## ======================================== ##
      ## Sub Menu : Atlas
      ## ======================================== ##
      tabItem(tabName = "Atlasm",
              tabBox(
                title = "",
                width = "100%",
                tabPanel("Overview", overviewUI("SRscore_microarray")),
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
      ## ======================================== ##
      ## Sub Menu : Template Matching
      ## ======================================== ##
      tabItem(tabName = "TemplateMatchingm",
              box(
                title = "",
                width = "100%",
                TemplateMatchUI("microarray")
              )
      ),
      ## ======================================== ##
      ## Sub Menu : Enrichment Analysis
      ## ======================================== ##
      tabItem(tabName = "EnrichmentAnalysism",
              EnrichUI("microarray", colnames_microarray)
      ),
      ## ========================================================= ##
      ## Menu : RNA-Seq
      ## ========================================================= ##
      ## ======================================== ##
      ## Sub Menu : Atlas
      ## ======================================== ##
      tabItem(tabName = "Atlasr",
              tabBox(
                title = "",
                width = "100%",
                tabPanel("Overview", overviewUI("SRscore_rnaseq")),
                tabPanel("ABA", stressUI("ABAr")),
                tabPanel("cold", stressUI("coldr")),
                tabPanel("DC3000", stressUI("DC3000r")),
                tabPanel("heat", stressUI("heatr")),
                tabPanel("drought", stressUI("droughtr")),
                tabPanel("highlight", stressUI("highlightr")),
                tabPanel("hypoxia", stressUI("hypoxiar")),
                tabPanel("osmotic", stressUI("osmoticr")),
                tabPanel("oxidation", stressUI("oxidationr")),
                tabPanel("salt", stressUI("saltr")),
                tabPanel("wound", stressUI("woundr"))
              )
      ),
      ## ======================================== ##
      ## Sub Menu : Template Matching
      ## ======================================== ##
      tabItem(tabName = "TemplateMatchingr",
              box(
                title = "",
                width = "100%",
                TemplateMatchUI("rnaseq")
              )
      ),
      ## ======================================== ##
      ## Sub Menu : Enrichment Analysis
      ## ======================================== ##
      tabItem(tabName = "EnrichmentAnalysisr",
              EnrichUI("rnaseq", colnames_rnaseq))
    )
  )
)