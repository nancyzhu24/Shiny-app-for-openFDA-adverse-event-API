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
source('modules/report.R')
source('modules/patient.R')
source('modules/drug.R')
source('modules/reaction.R')

#set spinner color globally
options(spinner.color="#18bc9c")

####### for an overview of this code, try clicking the arrow at the side of the editor
#< so you understand what are all the high-level functions and outputs in the server fxn


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


