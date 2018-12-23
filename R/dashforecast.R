library(h2o)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(data.table)
library(dplyr)

dashforecast <- function(data = data, share_app = FALSE,port = NULL ){
  
  app <- shinyApp(
    ui = dashboardPage(
      dashboardHeader(title = "Compare forecast models"),
      dashboardSidebar(    
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
        )),
      
      dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
          box(plotOutput("plot1", height = 250)),
          
          box(
            title = "Controls",
            sliderInput("slider", "Number of observations:", 1, 100, 50)
          )
        )
      )
    ),
    
    server = function(input, output) {
      set.seed(122)
      histdata <- rnorm(500)
      
      output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
      })
      
      
      output$input_curve <- renderDygraph({
        
        data <- as.data.table(data)
        
        if (lead(sequence_dates$Date,1)[1] - sequence_dates$Date[1] == 1){type_curve <- ""}
        
      })
    }
  )
  
  if (share_app == TRUE){
    
    if(is.null(port)){stop("Please choose a port to share dashboard")}
    else if (nchar(port) != 4) {stop("Incorrect format of port")}
    else if (nchar(port) == 4){
      ip_adress <- gsub(".*? ([[:digit:]])", "\\1", system("ipconfig", intern=TRUE)[grep("IPv4", system("ipconfig", intern=TRUE))]) 
      message("Forecast dashboard shared on LAN at ",ip_adress,":",port)
      runApp(app,host = "0.0.0.0",port = port,quiet = TRUE)
    }
  }
  
  else {runApp(app)}
  #shinyApp(ui, server)
  
  
}


dashforecast(share_app = TRUE ,port = 7895)

sequence_dates <- seq.Date(from = as.Date("2017-01-01"),to = as.Date("2018-01-01"),by = "months") %>% 
  as.data.table() %>% 
  mutate(valeur = runif( row_number()) *100) %>% 
  as.data.table()
colnames(sequence_dates) <- c("Date","Valeur")

plot(sequence_dates$Valeur)
dygraph(sequence_dates[,.(Date,Valeur)]) %>% 
  dyBarChart()
?seq.Date

sequence_dates$Date
lead(sequence_dates$Date,1)[1] - sequence_dates$Date[1] > 1



