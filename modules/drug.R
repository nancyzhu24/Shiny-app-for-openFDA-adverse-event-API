#module for drug

drugUI<-function(id){
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(h3("Reports per Indication (all reported drugs)",
             tipify(
               el = icon("info-circle"), trigger = "hover click",
               title = paste(
                 "This plot includes all indications for all drugs present in the matching reports.",
                 "It is not currently possible to search for only those indications associated with a specific drug",
                 "since the openFDA search API does not allow filtering of drug data.",
                 "The search query filters unique reports, which may have one or more drugs associated with them."))),
          plotlyOutput(ns("indication_plot"))%>%withSpinner(),
          width = 6),
      
      box(h3("Most Frequently Reported Drugs (Generic Name)",
             tipify(
               el = icon("info-circle"), trigger = "hover click",
               title = paste(
                 "This plot includes all drugs present in the matching reports.",
                 "The openFDA search API does not allow filtering of drug data.",
                 "The search query filters unique reports, which may have one or more drugs associated with them."))),
          plotlyOutput(ns("all_drugs"))%>%withSpinner(),
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
          plotlyOutput(ns("drugclassplot"))%>%withSpinner(),
          width = 8)
    )
  )
}



drugServer <- function(id, faers_query){
  
  moduleServer(id,
    function(input, output, session){
      
      output$indication_plot <- renderPlotly({
        query <- faers_query()$query
        
        indications <- query %>%
          fda_count("patient.drug.drugindication.exact") %>%
          fda_limit(25) %>%
          fda_exec()
        
        if(is.null(indications)) indications <- data.frame(term = character(), count = numeric())
        
        indications%>%
          plot_ly(x= ~ count, y= ~ reorder(term, count),
                  type = 'bar',
                  orientation = 'h',
                  marker = list(
                    color = 'rgba(50, 171, 96, 0.6)',
                    line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1)
                  ) )%>%
          layout(yaxis = list(title='', showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                 xaxis = list(title='', zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
        
        
      })
      
      
      output$all_drugs <- renderPlotly({
        query <- faers_query()$query
        
        drugs <- query %>%
          fda_count("patient.drug.openfda.generic_name.exact") %>%
          fda_limit(25) %>%
          fda_exec()
        
        if (is.null(drugs)) drugs <- data.frame(term = character(), count = numeric())
        
        drugs %>%
          plot_ly(x= ~ count, y= ~ reorder(term, count),
                  type = 'bar',
                  orientation = 'h',
                  marker = list(
                    color = 'rgba(50, 171, 96, 0.6)',
                    line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1)
                  ) )%>%
          layout(yaxis = list(title='', showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                 xaxis = list(title='', zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
        
      })
      
      
      output$drugclassplot <- renderPlotly({
        query <- faers_query()$query
        
        drugclass <- query %>%
          fda_count("patient.drug.openfda.pharm_class_epc.exact") %>%
          fda_limit(25) %>%
          fda_exec()
        
        if(is.null(drugclass)) drugclass <- data.frame(term = character(), count = numeric())
        
        drugclass%>%
          plot_ly(x= ~ count, y= ~ reorder(term, count),
                  type = 'bar',
                  orientation = 'h',
                  marker = list(
                    color = 'rgba(50, 171, 96, 0.6)',
                    line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1)
                  ) )%>%
          layout(yaxis = list(title='', showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                 xaxis = list(title='', zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
        
      })
      
      
    })}