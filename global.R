# data manip + utils
library(magrittr)
library(lubridate)
library(dplyr)


# data visualizations
library(plotly)
library(ggplot2)


# Shiny libraries
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
library(DT)

library(openfda)
source("utils.R")

####### for an overview of this code, try clicking the arrow at the side of the editor
#< so you understand what are all the high-level functions and outputs in the server fxn
# https://github.com/FDA/openfda/issues/29

# not possible to do searching for only suspect or concomitant drugs in openFDA
# http://opendata.stackexchange.com/questions/6157

topdrugs <- fda_query("/drug/event.json") %>%
  fda_count("patient.drug.openfda.generic_name.exact") %>% 
  fda_limit(1000) %>% 
  fda_exec() %>%
  .$term %>%
  sort() %>%
  grep("[%,]", ., value = TRUE, invert = TRUE) # https://github.com/FDA/openfda/issues/29

topdrugs <- c("Start typing to search..." = "", topdrugs)

topbrands <- fda_query("/drug/event.json") %>%
  fda_count("patient.drug.openfda.brand_name.exact") %>% 
  fda_limit(1000) %>% 
  fda_exec() %>%
  .$term %>%
  sort() %>%
  grep("[%,]", ., value = TRUE, invert = TRUE) # https://github.com/FDA/openfda/issues/29

topbrands <- c("Start typing to search..." = "", topbrands)


age_code <- data.frame(term = 800:805,
                       label = c("Decade",
                                 "Year",
                                 "Month",
                                 "Week",
                                 "Day",
                                 "Hour"),
                       stringsAsFactors = FALSE)

