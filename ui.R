ui <- dashboardPage(
  
  dashboardHeader(title = "AtSRGA"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(h4("Getting Started"), tabName = "Getting_Started"),
      menuItem(h4("Microarray"), tabName = "Microarray",
               menuSubItem("Atlas", tabName = "Atlasm"),
               menuSubItem("Template Matching", tabName = "TemplateMatchingm")
               ),
      menuItem(h4("RNA-Seq"), tabName = "RNA-Seq",
               menuSubItem("Atlas", tabName = "Atlasr"),
               menuSubItem("Template Matching", tabName = "TemplateMatchingr")
               ),
      menuItem(h4("Tutorial"), tabName = "Tutorial"),
      menuItem(h4("Contact us"), tabName = "Contact_us"),
      menuItem(h4("Report bugs"), tabName = "Report_bugs"),
      menuItem(h4("Citation"), tabName = "Citation")
      ),
    br(),
    br(),
    div(style = "padding-left: 10px;",
        p("Creative Commons LicenseExcept where otherwise noted, this work is subject to a Creative Commons Attribution 4.0 International License. (c) 2025 Kyoto Prefectural University")
       ),
    img(src = "LicenseMark.png", height = "70%", width = "70%", style = "padding-left: 10px;")
  ),
  
  dashboardBody(
    tags$style(type = "text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
              ),
    tags$head(tags$style(HTML(".content-wrapper { overflow: auto; }"))
              ),
    tabItems(
      ## ======================================================== ##
      ## Menu : Home
      ## ======================================================== ##
      tabItem(tabName = "Getting_Started",
              box(
                title = "",
                width = "100%",
                includeMarkdown("md/introduction.Rmd")
                ),
              box(
                title = "",
                width = "100%",
                includeMarkdown("md/quick_start.Rmd")
                ),
              box(
                title = "",
                width = "100%",
                includeMarkdown("md/overview.Rmd")
                )
              
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
                tabPanel("ABA",
                         CollectionsInfo(collections_ma$ABA),
                         stressUI("ABAm")),
                tabPanel("Cold",
                         CollectionsInfo(collections_ma$Cold),
                         stressUI("coldm")),
                tabPanel("DC3000",
                         CollectionsInfo(collections_ma$DC3000),
                         stressUI("DC3000m")),
                tabPanel("Drought",
                         CollectionsInfo(collections_ma$Drought),
                         stressUI("droughtm")),
                tabPanel("Heat",
                         CollectionsInfo(collections_ma$Heat),
                         stressUI("heatm")),
                tabPanel("High-light",
                         CollectionsInfo(collections_ma$`High-light`),
                         stressUI("highlightm")),
                tabPanel("Hypoxia",
                         CollectionsInfo(collections_ma$Hypoxia),
                         stressUI("hypoxiam")),
                tabPanel("Osmotic",
                         CollectionsInfo(collections_ma$Osmotic),
                         stressUI("osmoticm")),
                tabPanel("Oxidation",
                         CollectionsInfo(collections_ma$Oxidation),
                         stressUI("oxidationm")),
                tabPanel("Salt",
                         CollectionsInfo(collections_ma$Salt),
                         stressUI("saltm")),
                tabPanel("Wound",
                         CollectionsInfo(collections_ma$Wound),
                         stressUI("woundm"))
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
                tabPanel("ABA",
                         CollectionsInfo(collections_rs$ABA),
                         stressUI("ABAr")),
                tabPanel("Cold",
                         CollectionsInfo(collections_rs$Cold),
                         stressUI("coldr")),
                tabPanel("DC3000",
                         CollectionsInfo(collections_rs$DC3000),
                         stressUI("DC3000r")),
                tabPanel("Drought",
                         CollectionsInfo(collections_rs$Drought),
                         stressUI("droughtr")),
                tabPanel("Heat",
                         CollectionsInfo(collections_rs$Heat),
                         stressUI("heatr")),
                tabPanel("High-light",
                         CollectionsInfo(collections_rs$`High-light`),
                         stressUI("highlightr")),
                tabPanel("Hypoxia",
                         CollectionsInfo(collections_rs$Hypoxia),
                         stressUI("hypoxiar")),
                tabPanel("Osmotic",
                         CollectionsInfo(collections_rs$Osmotic),
                         stressUI("osmoticr")),
                tabPanel("Oxidation",
                         CollectionsInfo(collections_rs$Oxidation),
                         stressUI("oxidationr")),
                tabPanel("Salt",
                         CollectionsInfo(collections_rs$Salt),
                         stressUI("saltr")),
                tabPanel("Wound",
                         CollectionsInfo(collections_rs$Wound),
                         stressUI("woundr"))
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
      ## ======================================================== ##
      ## Menu : Help
      ## ======================================================== ##
      tabItem(tabName = "Tutorial",
              box(
                title = h1(strong("Tutorial")),
                width = "100%",
                HTML(
                  "<h3>For more detail infomation and operating instruction, please see ",
                  "<a href =  https://raw.githubusercontent.com/fusk-kpu/StressResponseGenesAtlas/main/AtSRGA_tutorial_en.pdf > our tutorial file</a>.")
                )
              ),
      tabItem(tabName = "Contact_us",
              box(
                title = h1(strong("Contact us")),
                width = "100%",
                HTML(
                  "<h3>To get in contact with our group, please visit ",
                  "<a href = https://www.kpu-g.com/english target = _blank rel = noopener noreferrer > our laboratory's website</a>.")
              )
              ),
      tabItem(tabName = "Report_bugs",
              box(
                title = h1(strong("Report bugs")),
                width = "100%",
                HTML(
                  "<h3>The R code for this application is available on ",
                  "<a href = https://github.com/fusk-kpu/StressResponseGenesAtlas target = _blank rel = noopener noreferrer > Github</a>."),
                br(),
                HTML(
                  "<h3>To report bugs and/or request features, please post ",
                  "<a href = https://github.com/fusk-kpu/StressResponseGenesAtlas/issues target = _blank rel = noopener noreferrer > issue on github</a>.")
                )
              ),
      tabItem(tabName = "Citation",
              box(
                title = h1(strong("Citation")),
                width = "100%",
                HTML("<h3>If you find AtSRGA useful, please consider citing our publication : "),
                br(),
                HTML("<h3>Coming soon :)")
              )
             )
      ) # tabItems
  ) #dashboardBody
) #dashboardPage