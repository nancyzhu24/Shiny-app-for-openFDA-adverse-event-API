#module for tab Reports

rxnUI<-function(id){
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(h3("Most Frequent Adverse Events (Preferred Terms)",
             tipify(
               el = icon("info-circle"), trigger = "hover click",
               title = paste0(
                 "Patient reactions, as MedDRA preferred terms, for all reactions present in ",
                 "the reports. For more rigorous analysis, use disproportionality statistics."))),
          plotlyOutput(ns("top_pt")) %>% withSpinner(),
          width = 6),
      
      box(h3("Report Outcome",
             tipify(
               el = icon("info-circle"), trigger = "hover click",
               title = "Outcome of the reaction at the time of last observation.")),
          plotlyOutput(ns("outcomeplot")) %>% withSpinner(),
          width = 6)
    )
  )
}


rxnServer <- function(id, faers_query){
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      output$top_pt <- renderPlotly({
        query <- faers_query()$query
        
        data <- query %>%
          fda_count("patient.reaction.reactionmeddrapt.exact") %>%
          fda_limit(25) %>%
          fda_exec()
        
       data%>%
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
      
      
      output$outcomeplot <- renderPlotly({
        query <- faers_query()$query
        
        outcome_results <- query %>% 
          fda_count("patient.reaction.reactionoutcome") %>% 
          fda_exec()
        if(is.null(outcome_results)) outcome_results <- data.frame(term = numeric(), count = numeric())
        
        outcome_code <- data.frame(term = 1:6,
                                   label = c("Recovered/resolved",
                                             "Recovering/resolving",
                                             "Not recovered/not resolved",
                                             "Recovered/resolved with \nsequelae (consequent health issues)",
                                             "Fatal",
                                             "Unknown"),
                                   stringsAsFactors = FALSE)
        
        outcome_results <- outcome_results %>%  
          left_join(outcome_code, by = "term") %>%
          select(label, count)
        
        
        outcome_results %>%
          plot_ly(labels = ~ label, values = ~ count)%>%
          add_pie(hole = 0.5)
        
      })
        
       
    }
  )
}