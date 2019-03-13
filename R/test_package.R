library(data.table)
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyWidgets)
library(tidyr)
library(DT)
library(h2o)
library(dygraphs)
library(plotly)
library(sparklyr)

  

Sys.setenv(http_proxy="")
Sys.setenv(http_proxy_user="")
Sys.setenv(https_proxy_user="")
longley2 <- longley %>% mutate(Year = as.Date(as.character(Year),format = "%Y"))
dash_h20(data =longley2,x = c("Unemployed" ,"Armed_Forces","Employed"),
             y = "GNP",date_column = "Year",share_app = TRUE,port = 3953)

h2o.()