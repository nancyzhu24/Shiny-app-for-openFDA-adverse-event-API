ui <- dashboardPage(
  
  
  dashboardHeader(title = titleWarning("Shiny FAERS"),
                  titleWidth = 600),
  
  dashboardSidebar(
    width = 240,
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
    tags$div(class="form-group shiny-input-container",
             actionButton("searchButton",
                          "Search",
                          width = '80%')
    ),
    tags$br(),
    tags$h3(strong("Current Query:")),
    tableOutput("current_search")
    # downloadButton(outputId = "hlt_data_dl",
    #                label = "Export data")
  ),
  
  dashboardBody(
    
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    customCSS(),
    fluidRow(
      box(htmlOutput(outputId = "timeplot_title"),
          plotlyOutput(outputId = "timeplot")%>%withSpinner(type=7),
          htmlOutput(outputId = "search_url"),
          width = 12
      )
    ),
    tabItems(
      tabItem(tabName = "reportdata",
              fluidRow(
                box(h3("Reporter Type",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = "Category of individual who submitted the report.")),
                    htmlOutput("reporterplot"),
                    width = 3),
                box(h3("Seriousness",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste0("Seriousness of the adverse event. An adverse event is marked serious if it ",
                                        "resulted in death, a life threatening condition, hospitalization, disability, ",
                                        "congenital anomaly, or other serious condition."))),
                    htmlOutput("seriousplot"),
                    width = 3),
                box(h3("Reason(s) for Seriousness",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste0("The serious condition which the adverse event resulted in. Total may sum to",
                                        " more than the total number of reports because reports can be marked serious for multiple reasons"))),
                    htmlOutput("seriousreasonsplot"),
                    width = 5)
              ),
              fluidRow(
                box(h3("Country of Occurrence",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = "The name of the country where the event occurred. This is not necessarily the same country the report was received from.")),
                    htmlOutput("countryplot"),
                    width = 6)
              )
      ),
      tabItem(tabName = "patientdata",
              fluidRow(
                box(h3("Sex",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = "The sex of the patient.")),
                    htmlOutput("sexplot"),
                    width = 3),
                box(h3("Age Group",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = HTML(paste0(
                           "Age group of the patient when the adverse effect occurred. ",
                           "Using the definitions from the Canada Vigilance Adverse Reaction Online Database.<br>",
                           "<br>Neonate: <= 25 days",
                           "<br>Infant: > 25 days to < 1 yr",
                           "<br>Child: >= 1 yr to < 13 yrs",
                           "<br>Adolescent: >= 13 yrs to < 18 yrs",
                           "<br>Adult: >= 18 yrs to <= 65 yrs",
                           "<br>Elderly: > 65 yrs")))),
                    htmlOutput("agegroupplot"),
                    width = 3),
                box(htmlOutput("agehisttitle"),
                    plotlyOutput("agehist"),
                    width = 6)
              )
      ),
      tabItem(tabName = "drugdata",
              fluidRow(
                box(h3("Reports per Indication (all reported drugs)",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste(
                           "This plot includes all indications for all drugs present in the matching reports.",
                           "It is not currently possible to search for only those indications associated with a specific drug",
                           "since the openFDA search API does not allow filtering of drug data.",
                           "The search query filters unique reports, which may have one or more drugs associated with them."))),
                    htmlOutput("indication_plot"),
                    width = 6),
                box(h3("Most Frequently Reported Drugs (Generic Name)",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste(
                           "This plot includes all drugs present in the matching reports.",
                           "The openFDA search API does not allow filtering of drug data.",
                           "The search query filters unique reports, which may have one or more drugs associated with them."))),
                    htmlOutput("all_drugs"),
                    width = 6)
              ),
              fluidRow(
                box(h3("Most Frequent Established Pharmaceutical Classes",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste(
                           "This plot includes established pharmacologic classes for all drugs present in the matching reports",
                           "The total number of instances for each class will be greater ",
                           "than the number of reports when reports include more than one drug of the same class.",
                           "The openFDA search API does not allow filtering of drug data."))),
                    htmlOutput("drugclassplot"),
                    width = 6)
              )
      ),
      tabItem(tabName = "rxndata",
              fluidRow(
                box(h3("Most Frequent Adverse Events (Preferred Terms)",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste0(
                           "Patient reactions, as MedDRA preferred terms, for all reactions present in ",
                           "the reports. For more rigorous analysis, use disproportionality statistics."))),
                    htmlOutput("top_pt"),
                    width = 6)
              ),
              fluidRow(
                box(h3("Report Outcome",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = "Outcome of the reaction at the time of last observation.")),
                    htmlOutput("outcomeplot"),
                    width = 4)
              )
      ),
      tabItem(tabName = "aboutinfo",
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
                "and Health Canada claims no responsibility for out-of-date information. This app uses US Trade Name ",
                "and Generic Name definitions, and therefore may not be relevant in a Canadian Context. The descriptions used in this ",
                "app are those defined in the openFDA reference, and are subject to change. For more information, please refer to ",
                "<a href = \"https://open.fda.gov/drug/event/reference/\">",
                "https://open.fda.gov/drug/event/reference/",
                "</a>. Due to ongoing issues with the openFDA API (",
                "<a href = \"https://github.com/FDA/openfda/issues/29\">https://github.com/FDA/openfda/issues/29</a>",
                "), some search terms with symbols may not be available for querying.",
                "</p>"))
              
      )
    )
  )
)
