ui <- dashboardPage(
  
  
  dashboardHeader(title = "US FAERS",
                  titleWidth = 300),
  
  dashboardSidebar(
    
    customCSS(),
    width = 280,
    sidebarMenu(
      menuItem("Reports", tabName = "reportdata", icon = icon("hospital-o")),
      menuItem("Patients", tabName = "patientdata", icon = icon("user-md")),
      menuItem("Drugs", tabName = "drugdata", icon = icon("flask")),
      menuItem("Reactions", tabName = "rxndata", icon = icon("heart-o")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"), selected = TRUE)
    ),
    conditionalPanel(
      condition = "input.name_type == 'generic'",
      selectizeInput("search_generic", 
                     "Generic Name", 
                     topdrugs,
                     multiple = TRUE)),
    conditionalPanel(
      condition = "input.name_type == 'brand'",
      selectizeInput("search_brand", 
                     "Brand Name (US Trade Name)",
                     topbrands,
                     multiple = TRUE)),
    radioButtons("name_type", "Drug name type:",
                 c("Generic" = "generic",
                   "Brand Name" = "brand")),
    selectizeInput("search_rxn",
                   "Preferred Term (PT)",
                   c("Loading..." = ""),
                   options = list(create = TRUE),
                   multiple = TRUE),
    dateRangeInput("searchDateRange", 
                   "Date Range", 
                   start = "2000-01-01",
                   end = substr(Sys.time(),1,10),
                   startview = "decade"),
    # hacky way to get borders correct
    #tags$div(class="form-group shiny-input-container",
    actionButton("searchButton","Search"),
                          #width = '80%'),
    tags$br(),
    tags$h3(strong("Current Query:")),
    tableOutput("current_search")
    # downloadButton(outputId = "hlt_data_dl",
    #                label = "Export data")
  ),
  
  dashboardBody(
    
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    
    
    fluidRow(
      box(htmlOutput(outputId = "timeplot_title"),
          plotlyOutput(outputId = "timeplot")%>%withSpinner(type=7),
          htmlOutput(outputId = "search_url"),
          width = 12
      )
    ),
    tabItems(
      tabItem(tabName = "reportdata",
            reportUI('reporttab')
      ),
      
      tabItem(tabName = "patientdata",
            patientUI('patienttab')
      ),
      
      tabItem(tabName = "drugdata",
            drugUI('drugtab')
      ),
      tabItem(tabName = "rxndata",
            rxnUI('reactiontab')
      ),
      tabItem(tabName = "aboutinfo",
          
          fluidRow(
              box(
              width = 12,
              h2("About"),
              # using tags$p() and tags$a() inserts spaces between text and hyperlink...thanks R
              HTML(paste0(
                "<p>",
                "This is a beta product developed using Open FDA data. DO NOT use as sole evidence to support ",
                "regulatory decisions or making decisions regarding medical care.  We do not hold any responsibility ",
                "for the authenticity or legitimacy of data.",
                "</p>",
                "<p>",
                "This app is a prototype experiment that utilizes publically available data (openFDA) ",
                "and presents it in an interactive way for enhanced visualizations. This app allows users to ",
                "effortlessly interact with the reports database, conduct searches and view results in highly ",
                "interactive dashboards.",
                "</p>",
                "<br>",
                "<p>",
                "Data provided by the U.S. Food and Drug Administration (FDA), retrieved through the openFDA API (",
                "<a href = \"https://open.fda.gov\">",
                "https://open.fda.gov",
                "</a>",
                "). The recency of the data is therefore dependent on when the API data source is updated, ",
                "This app uses US Trade Name ",
                "and Generic Name definitions. The descriptions used in this ",
                "app are those defined in the openFDA reference, and are subject to change. For more information, please refer to ",
                "<a href = \"https://open.fda.gov/drug/event/reference/\">",
                "https://open.fda.gov/drug/event/reference/",
                "</a>. Due to ongoing issues with the openFDA API (",
                "<a href = \"https://github.com/FDA/openfda/issues/29\">https://github.com/FDA/openfda/issues/29</a>",
                "), some search terms with symbols may not be available for querying.",
                "</p>"))))
              
      )
    )
  )
)
