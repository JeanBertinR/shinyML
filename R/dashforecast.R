library(h2o)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(data.table)
library(dplyr)

dashforecast <- function(data = data,y,date_column, share_app = FALSE,port = NULL ){
  
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
          #box(plotOutput("plot1", height = 250)),
          box(dygraphOutput("input_curve", height = 250,width = 1100),width = 30),
          
          
          
          
          box(
            title = "Controls",
            sliderInput("range_selector", "Choose train and test period:",
                        eval(parse(text = paste0("min(data$",date_column,")"))),
                        eval(parse(text = paste0("max(data$",date_column,")"))),  
                        eval(parse(text = paste0("c(min(data$",date_column,"),max(data$",date_column,"-10),max(data$",date_column,"))"))))
            # as.Date("2015-01-01"), as.Date("2015-12-31"),
            # c(as.Date("2015-01-01"),as.Date("2015-06-01")))
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
        curve_entries <- dygraph(data = data) %>%
          dyShading(from = input$range_selector[1],to = input$range_selector[2],color = "lightyellow" ) %>% 
          dyEvent(x = input$range_selector[1]) %>% 
          dyEvent(x = input$range_selector[2]) %>% 
          dySeries(y,fillGraph = TRUE) 
        
        
        # dyShading(from = data_index[cat_facturation == "sous facturation",]$Date[1], 
        #           to = tail(data_index[cat_facturation == "sous facturation",]$Date,1), color = "lightyellow") %>%
        #   #dyShading(from = "2017-1-1", to = "2018-1-1", color = "aliceblue") %>%
        #   dyEvent("2016-01-01", "année 2016", labelLoc = "bottom") %>% 
        #   dyEvent("2017-01-01", "année 2017", labelLoc = "bottom") %>% 
        #eval(parse(text = paste0("dygraph(sequence_dates[,.(",date_column,",",y,")])")))
        
        
        
        # if (lead(data$date_column,1)[1] - data$date_column[1] == 1){
        #   curve_entries <- curve_entries %>% dyBarChart()
        # }
        curve_entries
        
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


dashforecast(share_app = TRUE ,port = 7895,data =sequence_dates ,y = "Valeur",date_column = "Date")

# Package h2o
h2o.init(nthreads = -1)
h2o_data <- as.h2o(iris)
splits <- h2o.splitFrame(data = h2o_data,ratios = 0.75,seed = 1234)
model <- h2o.gbm(x = 1:3,y = "Petal.Width",training_frame = splits[[1]])
h2o.predict(model, splits[[2]])


# Package sparklyr
library(sparklyr)
sc <- spark_connect(master = "local")
data_conso_sparklyr <- copy_to(sc, iris, "iris", overwrite = TRUE)
partitions <- sdf_partition(x =data_conso_sparklyr,training = 0.75,test = 0.25 )


fit <- train %>%
  ml_linear_regression(normal  ~ jour + mois ,   type = "regression")


pred <- sdf_predict(fit, test) %>% collect %>% as.data.table()
erreur_prev <- pred %>% summarise(mape = 100 * mean(abs((normal - prediction) / prediction)),
                                  rmse = sqrt(mean((normal - prediction)**2)))

iris

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
sapply(sequence_dates, function(x) !all(is.na(as.Date(as.character(x),format="%d/%m/%Y"))))


nom_variable <- "Valeur"
dygraph(sequence_dates[,.(Date,Valeur)])
eval(parse(text = paste0("dygraph(sequence_dates[,.(Date,Valeur)])")))




class(sequence_dates$Date) =="Date"