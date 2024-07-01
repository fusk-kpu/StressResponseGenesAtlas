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
                tabPanel("Overview", overviewUI("microarray")),
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
                tabPanel("Overview", overviewUI("rnaseq")),
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
                     "<a href = https://www.kpu-g.com/english > https://www.kpu-g.com/english </a>"),
                br(),
                br(),
                br(),
                br(),
                h1("Report bugs"),
                br(),
                HTML("<h4>The R code for this application is available on ",
                     "<a href = https://github.com/fusk-kpu/StressResponseGenesAtlas >  Github </a>",
                     ". <br> To report bugs and/or request features, please use the following GitHub issue channel : <br><br>
                <a href = https://github.com/fusk-kpu/StressResponseGenesAtlas/issues > https://github.com/fusk-kpu/StressResponseGenesAtlas/issues </a>"),
                br(),
                br(),
                br(),
                br(),
                h1("Citation"),
                br(),
                HTML("<h4>If you find <strong>AtSRGA</strong>, useful, please consider citing our publication : <br><br>
                Coming soon :)"),
                img(src = "hexsticker_AtSRGA.png", width = "250px", height = "300px",
                    style="position:absolute; top:50px; right:50px;")
              ))
      )
  )
)