library(h2o)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(data.table)
library(dplyr)
library(timeDate)
library(lubridate)
library(plotly)


## x Optional) A vector containing the names or indices of the predictor variables to use in building the model. 
#If x is missing, then all columns except y are used.

# y The name or column index of the response variable in the data.
# The response must be either a numeric or a categorical/factor variable. 
#If the response is numeric, then a regression model will be trained, otherwise it will train a classification model.




sequence_dates <- seq.Date(from = as.Date("2017-01-01"),to = as.Date("2018-01-01"),by = "days") %>% 
  as.data.table() %>% 
  mutate(valeur = runif( row_number()) *100) %>% 
  as.data.table()
colnames(sequence_dates) <- c("Date","Valeur")
sequence_dates <- sequence_dates %>% 
  mutate(jour = day(Date),mois = month(Date)) %>% 
  as.data.table()


dashforecast <- function(data = data,x,y,date_column, share_app = FALSE,port = NULL ){

  app <- shinyApp(
    ui = dashboardPage(
      dashboardHeader(title = "Compare forecast models"),
      dashboardSidebar(    
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
        )),
      
      dashboardBody(
        fluidRow(
          #box(plotOutput("plot1", height = 250)),
          box(dygraphOutput("input_curve", height = 250,width = 1100),
              dygraphOutput("output_curve",height = 250,width = 700),
              
              width = 30),
          
          
          
          
          
          box(
            title = "Controls",
            sliderInput("train_selector", "Choose train period:",
                        min = eval(parse(text = paste0("min(data$",date_column,")"))),
                        max = eval(parse(text = paste0("max(data$",date_column,")"))),
                        value =  eval(parse(text = paste0("c(min(data$",date_column,"),mean(data$",date_column,"))")))),
            sliderInput("test_selector", "Choose test period:",
                        min = eval(parse(text = paste0("min(data$",date_column,")"))),
                        max = eval(parse(text = paste0("max(data$",date_column,")"))),
                        value = eval(parse(text = paste0("c(mean(data$",date_column,"),max(data$",date_column,"))")))),
            
            actionButton("run","Run forecast models!",style = 'color:white; background-color:darkgreen; padding:4px; font-size:150%',
                         icon = icon("users",lib = "font-awesome"))
            # as.Date("2015-01-01"), as.Date("2015-12-31"),
            # c(as.Date("2015-01-01"),as.Date("2015-06-01")))
          )
          #tags$style(HTML('#run{background-color:orange}')),
          
        

        )
      )
    ),
    
    server = function(session, input, output) {
      set.seed(122)
      histdata <- rnorm(500)
      
      output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
      })
      
      
      output$plot2 <- renderPlotly({
        plot_ly(data = data,x = iris$Sepal.Width,y = iris$Petal.Length)
        })
      
      
      output$input_curve <- renderDygraph({
        
        data <- as.data.table(data)
        curve_entries <- dygraph(data = eval(parse(text = paste0("data[,.(",date_column,",",y,")]"))))  %>% 
          dyShading(from = input$train_selector[1],to = input$train_selector[2],color = "snow" ) %>%
          dyShading(from = input$test_selector[1],to = input$test_selector[2],color = "azure" ) %>%
          dyAnnotation(mean(input$train_selector[1],input$train_selector[2],na.rm = TRUE,trim = 5),text= "Train period", attachAtBottom = TRUE,height = 20,width = 200) %>% 
          dyAnnotation(input$test_selector[2],text= "Test period", attachAtBottom = TRUE,height = 20,width = 200) %>% 
          dyEvent(x = input$train_selector[1]) %>%
          dyEvent(x = input$train_selector[2]) %>%
          dyEvent(x = input$test_selector[2]) %>%
          dySeries(y,fillGraph = TRUE)
        
        
        curve_entries
        
      })
      
      output$output_curve <- renderDygraph({
        data_output_curve <- data %>% filter(date_column >= input$range_selector[1]) %>% 
          as.data.table()
        data_output_curve <- eval(parse(text = paste0(
                                          "data %>% filter(",date_column,">= as.date(input$range_selector[1])) %>% as.data.table()"
        )
       ))
        
        dygraph(data_output_curve)
        
      })
      
      
      observeEvent(input$train_selector,{
        updateSliderInput(session,'test_selector',
                          value= c(input$train_selector[2],input$test_selector[2]) ) 
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





plot_ly(data = iris,x = eval(parse(text = paste0(data,"$",x))), y = eval(parse(text = paste0(data,"$",y))))


y <- "Petal.Length"

x <- "Sepal.Width"
data <- "iris"

# Package h2o
t <- Sys.time()
h2o.init(nthreads = -1)
h2o_data <- as.h2o(sequence_dates)
splits <- h2o.splitFrame(data = h2o_data,ratios = 0.75,seed = 1234)
model <- h2o.xgboost(x = 1:3,y = "Valeur",training_frame = splits[[1]])
model <- h2o.gbm(x = 3:4,y = "Valeur",training_frame = splits[[1]])

h2o.predict(model, splits[[2]]) %>% 
  as.data.table() %>% 
  cbind(.,as.data.table(splits[[2]]["Valeur"])) %>% 
  summarise(mape = 100 * mean(abs((Valeur - predict) / predict)),
            rmse = sqrt(mean((Valeur - predict)**2)))

t2 <- Sys.time()
?h2o.xgboost
t2 - t
# Package sparklyr
library(sparklyr)
t <- Sys.time()
sc <- spark_connect(master = "local")




iris_tbl <- sdf_copy_to(sc, iris, name = "iris_tbl", overwrite = TRUE)
partitions <- iris_tbl %>%
  sdf_partition(training = 0.7, test = 0.3, seed = 1111)

iris_training <- partitions$training
iris_test <- partitions$test

mlp_model <- iris_training %>%
  ml_multilayer_perceptron_classifier(Species ~ ., layers = c(4,3,3))

pred <- sdf_predict(iris_test, mlp_model)

ml_multiclass_classification_evaluator(pred)














data_conso_sparklyr <- copy_to(sc, iris, "iris", overwrite = TRUE)
partitions <- sdf_partition(x =data_conso_sparklyr,training = 0.75,test = 0.25 )


fit <- ml_gradient_boosted_trees(x = partitions$training,
                                 formula = "Petal_Width ~Sepal_Length+Sepal_Width+Petal_Length",type = "regression")

fit <- ml_multilayer_perceptron_classifier(x = partitions$training,
                                    formula = "Petal_Width ~Sepal_Length+Sepal_Width+Petal_Length",layers = c(2,2))



pred <- sdf_predict(fit, partitions$test) %>% collect %>% as.data.table()



pred %>% summarise(mape = 100 * mean(abs((Petal_Width - prediction) / prediction)),
                                  rmse = sqrt(mean((Petal_Width - prediction)**2)))

T1 <- Sys.time()

T1 - t



# Package xgboost 
library(xgboost)
iris_xg <- iris %>% mutate(Petal.Width = as.numeric(Petal.Width))
model <- xgboost(data = as.matrix(iris_xg[1:100,1:4]),label = as.matrix(iris_xg[1:100,4]),nrounds = 100)
prediction <- predict(model,as.matrix(iris_xg[101:150,4])) %>% 
  as.data.table() %>% 
  cbind(.,iris[,"Petal.Width"] %>% as.data.table())

colnames(prediction) <- c("predict","Petal.Width")

resultats <- prediction %>% 
  summarise(mape = 100 * mean(abs((Petal.Width - predict) / predict)),
            rmse = sqrt(mean((Petal.Width - predict)**2)))
  
  
 
  
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
>>>>>>> 1fa2bb36fca043fddd05b571c30d8488dd621536
