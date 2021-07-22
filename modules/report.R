#module for tab Reports

reportUI<-function(id){
  
  ns <- NS(id)
  
  tagList(
  
  fluidRow(
    box(h3("Reporter Type",
           tipify(
             el = icon("info-circle"), trigger = "hover click",
             title = "Category of individual who submitted the report.")),
        plotlyOutput(ns("reporterplot"))%>%withSpinner(),
        width = 4),
    
    box(h3("Seriousness",
           tipify(
             el = icon("info-circle"), trigger = "hover click",
             title = paste0("Seriousness of the adverse event. An adverse event is marked serious if it ",
                            "resulted in death, a life threatening condition, hospitalization, disability, ",
                            "congenital anomaly, or other serious condition."))),
        plotlyOutput(ns("seriousplot"))%>%withSpinner(),
        width = 4),
    
    box(h3("Reason(s) for Seriousness",
           tipify(
             el = icon("info-circle"), trigger = "hover click",
             title = paste0("The serious condition which the adverse event resulted in. Total may sum to",
                            " more than the total number of reports because reports can be marked serious for multiple reasons"))),
        plotlyOutput(ns("seriousreasonsplot"))%>%withSpinner(),
        width = 4)
  ),
  
  fluidRow(
    box(h3("Country of Occurrence",
           tipify(
             el = icon("info-circle"), trigger = "hover click",
             title = "The name of the country where the event occurred. This is not necessarily the same country the report was received from.")),
        plotlyOutput(ns("countryplot"))%>%withSpinner(),
        width = 6)
  )
  
  )
}


reportServer <- function(id, faers_query){
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      output$reporterplot <- renderPlotly({
        
        query <- faers_query()$query
        
        reporter_results <- query %>%
          fda_count("primarysource.qualification") %>%
          fda_exec()
        
        if (is.null(reporter_results)) reporter_results <- data.frame(term = numeric(), count = numeric())
        reporter_code <- data.frame(term = 1:5,
                                    label = c("Physician",
                                              "Pharmacist",
                                              "Other health professional",
                                              "Lawyer",
                                              "Consumer or\nnon-health professional"),
                                    stringsAsFactors = FALSE)
        reporter_results <- reporter_results %>%
          left_join(reporter_code, by = "term") %>%
          select(label, count)
        
        unknown <- query %>%
          fda_filter("_missing_", "primarysource.qualification") %>%
          fda_url() %>%
          fda_fetch() %>%
          .$meta %>%
          .$results %>%
          .$total
        if (!is.null(unknown)) reporter_results <- rbind(reporter_results, c("Not reported", unknown))
        reporter_results %<>% mutate(count = as.numeric(count))
        
        reporter_results %>%
            plot_ly(labels = ~ label, values = ~ count)%>%
            add_pie(hole = 0.5)
        
      })
      
      
      output$seriousplot <- renderPlotly({
        
        query <- faers_query()$query
        
        serious_results <- query %>%
          fda_count("serious") %>%
          fda_exec()
        if (is.null(serious_results)) serious_results <- data.frame(term = numeric(), count = numeric())
        
        serious_results <- serious_results %>%
          mutate(label = ifelse(term == 1, "Serious", "Non-serious")) %>%
          select(label, count) %>%
          slice(match(c("Serious", "Non-serious", "Not reported"), label))
        
        unknown <- query %>%
          fda_filter("_missing_", "serious") %>%
          fda_url() %>%
          fda_fetch() %>%
          .$meta %>%
          .$results %>%
          .$total
        if (!is.null(unknown)) serious_results <- rbind(serious_results, c("Not Reported", unknown))
        serious_results %<>% mutate(count = as.numeric(count))
        
        serious_results %>%
          plot_ly(labels = ~ label, values = ~ count)%>%
          add_pie(hole = 0.5)
      })
      
      
      output$seriousreasonsplot <- renderPlotly({
        query <- faers_query()$query
        
        congenital_results <- query %>%
          fda_count("seriousnesscongenitalanomali") %>%
          fda_exec()
        if (is.null(congenital_results)) congenital_results <- data.frame(term = 1, count = 0)
        
        death_results <-  query %>% 
          fda_count("seriousnessdeath") %>% 
          fda_exec()
        if (is.null(death_results)) death_results <- data.frame(term = 1, count = 0)
        
        disabling_results <-  query %>%
          fda_count("seriousnessdisabling") %>%
          fda_exec()
        if (is.null(disabling_results)) disabling_results <- data.frame(term = 1, count = 0)
        
        hospital_results <-  query %>%
          fda_count("seriousnesshospitalization") %>%
          fda_exec()
        if (is.null(hospital_results)) hospital_results <- data.frame(term = 1, count = 0)
        
        lifethreaten_results <-  query %>%
          fda_count("seriousnesslifethreatening") %>%
          fda_exec()
        if (is.null(lifethreaten_results)) lifethreaten_results <- data.frame(term = 1, count = 0)
        
        serother_results <-  query %>%
          fda_count("seriousnessother") %>%
          fda_exec()
        if (is.null(serother_results)) serother_results <- data.frame(term = 1, count = 0)
        
        serious_reasons <- bind_rows("Death" = death_results,
                                     "Life-threatening condition" = lifethreaten_results,
                                     "Hospitalization" = hospital_results,
                                     "Disabling" = disabling_results,
                                     "Congenital anomaly" = congenital_results,
                                     "Other serious condition" = serother_results,
                                     .id = "label")
        
        serious_reasons%>%
        plot_ly(x= ~ count, y= ~ reorder(label, count),
                type = 'bar',
                orientation = 'h',
                marker = list(
                  color = 'rgba(50, 171, 96, 0.6)',
                  line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1)
                ) )%>%
          layout(yaxis = list(title='', showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                 xaxis = list(title='', zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
        
      })
      
      
      output$countryplot <- renderPlotly({
        query <- faers_query()$query
        
        country_results <- query %>%
          fda_count("occurcountry.exact") %>%
          fda_limit(10) %>%
          fda_exec()
        if (is.null(country_results)) country_results <- data.frame(term = character(), count = numeric())
        
        unknown <- query %>%
          fda_filter("_missing_", "occurcountry") %>%
          fda_url() %>%
          fda_fetch() %>%
          .$meta %>%
          .$results %>%
          .$total
        
        if (!is.null(unknown)) country_results <- rbind(country_results, c("not reported", unknown))
        country_results %<>% mutate(count = as.numeric(count))
        
        country_results%>%
          plot_ly(x= ~ count, y= ~ reorder(term, count),
                  type = 'bar',
                  orientation = 'h',
                  marker = list(
                    color = 'rgba(31, 119, 180, 0.6)',
                    line = list(color = 'rgba(31, 119, 180, 1.0)', width = 1)
                  ) )%>%
          layout(yaxis = list(title='', showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                 xaxis = list(title='', zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
        
        
      })
      
      
    }
  )
}