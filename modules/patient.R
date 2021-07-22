#module for patient data

patientUI<-function(id){
  
  ns <- NS(id)
  
  tagList(

fluidRow(
  
  box(h3("Sex",
         tipify(
           el = icon("info-circle"), trigger = "hover click",
           title = "The sex of the patient.")),
      plotlyOutput(ns("sexplot"))%>%withSpinner(),
      width = 4),
  
  box(htmlOutput(ns("agehisttitle")),
      plotlyOutput(ns("agehist"))%>%withSpinner(),
      width = 8)
)
  )}




patientServer <- function(id, faers_query, ages){
  
  moduleServer(id,
    
    function(input, output, session){


output$sexplot <- renderPlotly({
  query <- faers_query()$query
  
  sex_code <- data.frame(term = 0:2,
                         label = c("Unknown",
                                   "Male",
                                   "Female"),
                         stringsAsFactors = FALSE)
  
  sex_results <- query %>% 
    fda_count("patient.patientsex") %>% 
    fda_exec() %>%
    left_join(sex_code, by = "term")
  if(is.null(sex_results)) sex_results <- data.frame(term = numeric(), count = numeric())
  
  unknown <- query %>%
    fda_filter("_missing_", "patient.patientsex") %>%
    fda_url() %>%
    fda_fetch() %>%
    .$meta %>%
    .$results %>%
    .$total
  if (!is.null(unknown)) sex_results %<>% rbind(c(3, unknown, "Not reported"))
  sex_results %<>% select(label, count) %>% mutate(count = as.numeric(count))
  
  sex_results %>%
    plot_ly(labels = ~ label, values = ~ count)%>%
    add_pie(hole = 0.5)
})


output$agehisttitle <- renderUI({
  excluded_count <- ages() %>%
    filter(age_group != "Unknown", term > 100) %>%
    `$`('count') %>% sum()
  HTML(paste0("<h3>Histogram of Patient Ages ",
              tipify(
                el = icon("info-circle"), trigger = "hover click",
                title = "Distribution of number of reports per age, colour-coded by age group. Each bin groups 2 years."),
              "<br>(", excluded_count, " reports with age greater than 100 excluded)", "</h3>"))
})

output$agehist <- renderPlotly({
  age_data <- ages() %>% filter(age_group != "Unknown", term <= 100) %>% rename(age = term)
  # age_groups$age_group %<>% factor(levels = c("Infant", "Child", "Adolescent", "Adult", "Elderly"))
  # 
  # # joining by remaining terms so you can assign the right colours to the legend
  # colours_df <- data.frame(
  #   age_group = c("Neonate", "Infant", "Child", "Adolescent", "Adult", "Elderly"),
  #   colours = google_colors[1:6],
  #   stringsAsFactors = FALSE) %>%
  #   semi_join(distinct(age_groups, age_group), by = "age_group")
  
  hist <- ggplot(age_data, aes(x = age, weight = count, fill = age_group)) +
    geom_histogram(breaks = seq(0, 100, by = 2)) +
    #scale_fill_manual(values = colours_df$colours) +
    xlab("Age at onset (years)") + 
    ylab("Number of Reports") +
    theme_bw()
  
  ggplotly(hist)
})


}
)}
