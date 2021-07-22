server <- function(input, output, session) {
  # Relabel PT dropdown menu based on selected name
  observe({
    input$search_generic
    input$search_brand
    input$name_type
    
    isolate({
      pt_selected <- input$search_rxn
      pt_choices <- fda_query("/drug/event.json")
      
      if (input$name_type == "generic" & !is.null(input$search_generic)) {
        query_str <- paste0('"', gsub(" ", "+", input$search_generic), '"')
        query_str <- sprintf('(%s)', paste0(query_str, collapse = '+'))
        pt_choices %<>% fda_filter("patient.drug.openfda.generic_name.exact", query_str)
      } else if (input$name_type == "brand" & !is.null(input$search_brand)) {
        query_str <- paste0('"', gsub(" ", "+", input$search_brand), '"')
        query_str <- sprintf('(%s)', paste0(query_str, collapse = '+'))
        pt_choices %<>% fda_filter("patient.drug.openfda.brand_name.exact", query_str)
      }
      
      pt_choices %<>%
        fda_count("patient.reaction.reactionmeddrapt.exact") %>%
        fda_limit(1000) %>%
        fda_exec() %>%
        .$term %>%
        sort() %>%
        grep("[%,']", ., value = TRUE, invert = TRUE) # https://github.com/FDA/openfda/issues/29
      
      if (is.null(pt_selected)) pt_selected = ""
      updateSelectizeInput(session, "search_rxn",
                           choices = c("Start typing to search..." = "", pt_choices),
                           selected = pt_selected)
    })
  })
  
  
  
  
  
  ########## Reactive data processing
  # Data structure to store current query info
  current_search <- reactiveValues()
  faers_query <- reactiveValues()
  
  # We need to have a reactive structure here so that it activates upon loading
  reactiveSearchButton <- reactive(as.vector(input$searchButton))
  observeEvent(reactiveSearchButton(), {
      
    withProgress(message = 'Loading search criterias', value = 0, {
      
      if (input$name_type == "generic") {
        name <- input$search_generic
      } else {
        name <- input$search_brand
      }
     
      
      openfda_query <- fda_query("/drug/event.json")
      query_str <- paste0("[", input$searchDateRange[1], "+TO+", input$searchDateRange[2], "]")
      openfda_query %<>% fda_filter("receivedate", query_str)
      if(!is.null(name)) {
        query_str <- paste0('"', gsub(" ", "+", name), '"')
        query_str_combine <- sprintf('(%s)', paste0(query_str, collapse = '+'))
        
        if (input$name_type == "generic") {
          openfda_query %<>% fda_filter("patient.drug.openfda.generic_name.exact", query_str_combine)
        } else {
          openfda_query %<>% fda_filter("patient.drug.openfda.brand_name.exact", query_str_combine)
        }
      }
      
      incProgress(0.3)
      
      if(!is.null(input$search_rxn)) {
        query_str <- paste0('"', gsub(" ", "+", input$search_rxn), '"')
        query_str_combine <- sprintf('(%s)', paste0(query_str, collapse = '+'))
        openfda_query %<>% fda_filter("patient.reaction.reactionmeddrapt.exact", query_str_combine)
      }
      
      incProgress(0.6)
      
      result <- openfda_query %>% fda_search() %>% fda_limit(1) %>% fda_exec()
      if (is.null(result)) {
        setProgress(1)
        showModal(modalDialog(
          title = list(icon("exclamation-triangle"), "No results found!"),
          "There were no reports matching your query.",
          size = "s",
          easyClose = TRUE))
        return()
      }
      
      incProgress(0.95)
      
      current_search$name_type<- input$name_type
      current_search$name<- name
      current_search$rxn <- input$search_rxn
      current_search$date_range <- input$searchDateRange
      faers_query$query <- openfda_query
      
    })
    
  })
  ages <- reactive({
    query <- faers_query$query
    
    age_years <- query %>% 
      fda_filter("patient.patientonsetageunit", "801") %>%
      fda_count("patient.patientonsetage") %>%
      fda_limit(1000) %>%
      fda_exec()
    if(is.null(age_years)) age_years <- data.frame(term = numeric(), count = numeric())

    
    unknown <- query %>%
      fda_filter("_missing_", "patient.patientonsetage") %>%
      fda_url() %>%
      fda_fetch() %>%
      .$meta %>%
      .$results %>%
      .$total
    age_unknown <- data.frame(term = NA, count = unknown)
    
    ages <- bind_rows(age_years,
                      age_unknown) %>%
      mutate(age_group = NA,
             age_group = ifelse(term <1, "Infant", age_group),
             age_group = ifelse(term >= 1 & term < 13, "Child", age_group),
             age_group = ifelse(term >= 13 & term < 18, "Adolescent", age_group),
             age_group = ifelse(term >= 18 & term <= 65, "Adult", age_group),
             age_group = ifelse(term > 65, "Elderly", age_group),
             age_group = ifelse(is.na(term), "Not reported", age_group))
  })
  
  ########## Output
  output$current_search <- renderTable({
    data <- current_search
    result <- data.frame(names = c("Name Type:", 
                                   "Name:", 
                                   "Adverse Reaction Term:",
                                   "Date Range:"),
                         values = c(toupper(data$name_type),
                                    ifelse(is.null(data$name), 'Not Specified', paste(data$name, collapse = ', ')),
                                    ifelse(is.null(data$rxn), 'Not Specified', paste(data$rxn, collapse = ', ')),
                                    paste(data$date_range, collapse = " to ")),
                         stringsAsFactors = FALSE)
    
    result
  }, include.colnames = FALSE)
  
  ### Create time plot
  output$timeplot_title <- renderUI({
    query <- faers_query$query
    nreports <- query %>%
      fda_search() %>%
      fda_url() %>%
      fda_fetch() %>%
      .$meta %>%
      .$results %>%
      .$total
    drug_name <- current_search$name
    rxn_name <- current_search$rxn
    
    if (is.null(drug_name)) drug_name <- "All Drugs"
    if (is.null(rxn_name)) rxn_name <- "All Reactions"
    plottitle <- paste0("Drug Adverse Event Reports for ", paste0(drug_name, collapse = ', '), " and ", paste0(rxn_name, collapse = ', '), " (", nreports, " reports)")
    h3(strong(plottitle))
  })
  
  output$timeplot <- renderPlotly({
    
    withProgress(message = 'Querying data from FDA API', value = 0, {
    
    query <- faers_query$query
    
    incProgress(0.2)
    
    total_results <- query %>%
      fda_count("receivedate") %>%
      fda_limit(1000) %>%
      fda_exec() %>%
      dplyr::mutate(month = floor_date(ymd(time), "year")) %>%
      dplyr::count(month, wt = count) %>%
      rename(total = n)
    
    
    incProgress(0.5)
    
    serious_results <- query %>%
      fda_filter("serious", "1") %>%
      fda_count("receivedate") %>%
      fda_limit(1000) %>%
      fda_exec() %>%
      dplyr::mutate(month = floor_date(ymd(time), "year")) %>%
      dplyr::count(month, wt = count) %>%
      rename(serious = n)
    
    death_results <- query %>%
      fda_filter("seriousnessdeath", "1") %>%
      fda_count("receivedate") %>%
      fda_limit(1000) %>%
      fda_exec() %>%
      dplyr::mutate(month = floor_date(ymd(time), "year")) %>%
      dplyr::count(month, wt = count) %>%
      rename(death = n)
    
    incProgress(0.7)
    
    
    nmonths <- interval(min(total_results$month), max(total_results$month)) %/% years(1)
    time_list <- min(total_results$month) + years(0:nmonths)
    
    results <- data.frame(month = time_list) %>%
      left_join(total_results, by = "month") %>%
      left_join(serious_results, by = "month") %>%
      left_join(death_results, by = "month")
    results[is.na(results)] <- 0
    
    results$non_serious<-results$total-results$serious
    results$serious<-results$serious-results$death
    
    incProgress(0.95,message= 'Plotting')
    
    annotations <- list()
    for (i in 1:length(results$total)) {
      annotations[[i]] <- list(x = results$month[[i]],
                               y = results$total[[i]],
                               text = results$total[[i]],
                               yanchor='bottom',
                               showarrow = FALSE,
                               textangle=-90
      )
      
    }
    
    plot_ly(results,x=~month,y=~serious,type='bar',name='Serious',marker=list(color='#e6ab02'))%>%
      add_trace(y=~non_serious,name='Non-Serious',marker=list(color='#67a61e'))%>%
      add_trace(y=~death,name='Death',marker=list(color='#d62728'))%>%
      layout(yaxis = list(title = 'Count'), 
             xaxis=list(title='',tickangle=-45,color='grey'),
             barmode = 'stack',
             annotations=annotations)
    
    })
  
  })
  
  output$search_url <- renderUI({
    url <- faers_query$query %>%
      fda_search() %>%
      fda_limit(100) %>%
      fda_url()
    HTML(paste0("Reports by year from US FDA FAERS (open.fda.gov). Search URL: <a href = ", url, ">", url, "</a>"))
  })
  
  
  ### report tab module
  reportServer('reporttab',reactive(faers_query))
  
  
  ### patient tab module
  patientServer('patienttab',reactive(faers_query),ages)
  
  
  
  ### drugs tab module
 drugServer('drugtab',reactive(faers_query))
  
  ### Reactions tab module
 rxnServer('reactiontab',reactive(faers_query))
 
  
}