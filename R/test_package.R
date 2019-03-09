library(data.table)
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyWidgets)
library(tidyr)
library(DT)

  
longley2 <- longley %>% mutate(Year = as.Date(as.character(Year),format = "%Y"))
dash_spark(data =longley2,x = c("Unemployed" ,"Armed_Forces","Employed"),
             y = "GNP",date_column = "Year",share_app = TRUE,port = 3951)
