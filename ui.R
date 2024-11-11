ui <- dashboardPage(
  
  dashboardHeader(title = "AtSRGA"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Microarray", tabName = "Microarray",
               menuSubItem("Atlas", tabName = "Atlasm"),
               menuSubItem("Template Matching", tabName = "TemplateMatchingm")
               ),
      menuItem("RNA-Seq", tabName = "RNA-Seq",
               menuSubItem("Atlas", tabName = "Atlasr"),
               menuSubItem("Template Matching", tabName = "TemplateMatchingr")
               ),
      menuItem("Help", tabName = "Help")
    )
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
      tabItem(tabName = "Help",
              box(
                title = "",
                width = "100%",
                h1("Tutorial"),
                br(),
                HTML("<h4>Please see ",
                     "<a href = https://github.com/fusk-kpu/StressResponseGenesAtlas/raw/main/AtSRGA_tutorial_en.pdf > our tutorial file. </a>"),
                br(),
                br(),
                br(),
                br(),
                h1("Contact us"),
                br(),
                HTML("<h4>To get in contact with our group, please visit : ",
                     "<a href = https://www.kpu-g.com/english", blank, 
                     " > https://www.kpu-g.com/english </a>"),
                br(),
                br(),
                br(),
                br(),
                h1("Report bugs"),
                br(),
                HTML("<h4>The R code for this application is available on ",
                     "<a href = https://github.com/fusk-kpu/StressResponseGenesAtlas ",
                     blank, "> Github </a>",
                     ". <br> To report bugs and/or request features, please use the following GitHub issue channel : <br><br>
                     <a href = https://github.com/fusk-kpu/StressResponseGenesAtlas/issues ",
                     blank, ">
                     https://github.com/fusk-kpu/StressResponseGenesAtlas/issues </a>"),
                br(),
                br(),
                br(),
                br(),
                h1("Citation"),
                br(),
                HTML("<h4>If you find <strong>AtSRGA</strong>, useful, please consider citing our publication : <br><br>
                Coming soon :)"),
                img(src = "hexsticker_AtSRGA.png", width = "25%", height = "auto",
                    style="position:absolute; top:50px; right:50px;")
              )
             )
      ) # tabItems
  ) #dashboardBody
) #dashboardPage