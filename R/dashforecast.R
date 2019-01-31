library(h2o)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(data.table)
library(dplyr)
library(timeDate)
library(lubridate)
library(plotly)
library(sparklyr)
library(DT)

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
  mutate(jour = day(Date),mois = month(Date),numero_jour = row_number()) %>% 
  as.data.table()








dashforecast <- function(data = data,x,y,date_column, share_app = FALSE,port = NULL ){
  #spark_disconnect(sc)
  sc <- spark_connect(master = "local")
  
  
  
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
              
              column(
                dygraphOutput("output_curve",height = 250,width = 700),
                #dataTableOutput("results_table"),
                width = 10),
              
              width = 30),
          
          
          
          
          
          box(
            title = "Controls",
            
            selectInput( inputId  = "input_variables",label = "Input variables: ",
                         choices = x,
                         multiple = TRUE,selected = x),
            
            
            sliderInput("train_selector", "Choose train period:",
                        min = eval(parse(text = paste0("min(data$",date_column,")"))),
                        max = eval(parse(text = paste0("max(data$",date_column,")"))),
                        value =  eval(parse(text = paste0("c(min(data$",date_column,"),mean(data$",date_column,"))")))),
            sliderInput("test_selector", "Choose test period:",
                        min = eval(parse(text = paste0("min(data$",date_column,")"))),
                        max = eval(parse(text = paste0("max(data$",date_column,")"))),
                        value = eval(parse(text = paste0("c(mean(data$",date_column,"),max(data$",date_column,"))")))),
            
            
            actionButton("train_all","Run !",style = 'color:white; background-color:red; padding:4px; font-size:150%',
                         icon = icon("cogs",lib = "font-awesome"))
          ),
          
          box(
            title = "Gradient boosting trees",
            
            
            sliderInput(label = "Step size",min = 0,max = 1, inputId = "step_size_gbm",value = 0.1),
            sliderInput(label = "Subsampling rate",min = 0.1,max = 1, inputId = "subsampling_rate_gbm",value = 1),
            
            
            actionButton("run_gradient_boosting","Run gradient boosting model",style = 'color:white; background-color:darkgreen; padding:4px; font-size:150%',
                         icon = icon("users",lib = "font-awesome"))
            # as.Date("2015-01-01"), as.Date("2015-12-31"),
            # c(as.Date("2015-01-01"),as.Date("2015-06-01")))
          ),
          #tags$style(HTML('#run{background-color:orange}')),
          box(
            title = "Random Forest",
            
            sliderInput(label = "Number of trees",min = 1,max = 100, inputId = "num_tree_random_forest",value = 20),
            sliderInput(label = "Subsampling rate",min = 0.1,max = 1, inputId = "subsampling_rate_random_forest",value = 1),
            sliderInput(label = "Max depth",min = 0,max = 20, inputId = "max_depth_random_forest",value = 5),
            dataTableOutput("test_result"),
            dataTableOutput("date_essai"),
            
            
            actionButton("run_random_forest","Run random forest model",style = 'color:white; background-color:darkblue; padding:4px; font-size:150%',
                         icon = icon("users",lib = "font-awesome"))
            # as.Date("2015-01-01"), as.Date("2015-12-31"),
            # c(as.Date("2015-01-01"),as.Date("2015-06-01")))
          ),
          
          box(
            title = "Generalized linear regresion",
            
            # sliderInput(label = "Number of trees",min = 1,max = 100, inputId = "num_tree_random_forest",value = 20),
            # sliderInput(label = "Subsampling rate",min = 0,max = 1, inputId = "subsampling_rate_random_forest",value = 1),
            # sliderInput(label = "Max depth",min = 0,max = 20, inputId = "max_depth_random_forest",value = 5),
            # dataTableOutput("test_result"),
            # dataTableOutput("date_essai"),
            
            
            actionButton("run_glm","Run generalized linear regression",style = 'color:white; background-color:orange; padding:4px; font-size:150%',
                         icon = icon("cogs",lib = "font-awesome"))
            # as.Date("2015-01-01"), as.Date("2015-12-31"),
            # c(as.Date("2015-01-01"),as.Date("2015-06-01")))
          )
          
          
          
        )
      )
    ),
    
    server = function(session, input, output) {
      set.seed(122)
      histdata <- rnorm(500)
      
      
      
      
      t <- reactiveValues(step_size_gbm = 0.1)
      v <- reactiveValues(subsampling_rate_gbm = 1)
      
      v_grad <- reactiveValues(type_model = NA)
      v_random <- reactiveValues(type_model = NA)
      v_glm <- reactiveValues(type_model = NA)
      
      x <- reactiveValues(max_depth_random_forest = 5)
      
      observeEvent(input$run_gradient_boosting,{
        t$step_size_gbm <- input$step_size_gbm
        v$subsampling_rate_gbm <- input$subsampling_rate_gbm
        v_grad$type_model <- "ml_gradient_boosted_trees"
        
      })
      
      
      observeEvent(input$run_random_forest,{
        #r$model <- "random_forest"
        t$num_tree_random_forest <- input$num_tree_random_forest
        v$subsampling_rate_random_forest <- input$subsampling_rate_random_forest
        x$max_depth_random_forest <-  input$max_depth_random_forest
        
        v_random$type_model <- "ml_random_forest"
        
      })
      
      observeEvent(input$run_glm,{
        #r$model <- "random_forest"
        # t$num_tree_random_forest <- input$num_tree_random_forest
        # v$subsampling_rate_random_forest <- input$subsampling_rate_random_forest
        # x$max_depth_random_forest <-  input$max_depth_random_forest
        
        v_glm$type_model <- "ml_generalized_linear_regression"
        
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
      
      
      test_1 <- reactiveValues(date = eval(parse(text = paste0("mean(data$",date_column,")"))))
      
      observeEvent(input$train_all,{
        
        test_1$date <- input$test_selector[1]
        v_grad$type_model <- "ml_gradient_boosted_trees"
        
      })
      
      
      output$date_essai <- renderDataTable({
        test_1$date %>% as.data.table()
      })
      
      
      
      table_forecast <- reactive({
        
        
        table_ml_gradient_boosted <- data.table(ml_gradient_boosted_trees = NA)
        table_ml_random_forest <- data.table(ml_random_forest = NA)
        table_ml_glm <- data.table(ml_generalized_linear_regression = NA)
        
        data_results = eval(parse(text = paste0("data[,.(",date_column,",",y,")][",date_column,">","'",test_1$date,"',]")))
        
        chaine_variable <- "mois"
        
        if (length(input$input_variables) > 1 ){
          for (i in 1:length(input$input_variables)){chaine_variable <- paste(chaine_variable,"+",input$input_variables[i])}
        }
        
        
        data_spark_train <- eval(parse(text = paste0( "data %>% filter(",date_column,"<='", test_1$date,"') %>% as.data.table()")))
        data_spark_test <- eval(parse(text = paste0("data %>% filter(",date_column,">'", test_1$date,"') %>% as.data.table()")))
        
        data_spark_train <- copy_to(sc, data_spark_train, "data_spark_train", overwrite = TRUE)
        data_spark_test <- copy_to(sc, data_spark_test, "data_spark_test", overwrite = TRUE)
        
        
        if (!is.na(v_grad$type_model) & v_grad$type_model == "ml_gradient_boosted_trees"){
          
          
          eval(parse(text = paste0("fit <- data_spark_train %>%",v_grad$type_model,"(", y ," ~ " ,chaine_variable ,
                                   ",step_size =",t$step_size_gbm,
                                   ",subsampling_rate =",v$subsampling_rate_gbm,
                                   " )")))
          
          table_ml_gradient_boosted <- sdf_predict(data_spark_test, fit) %>% collect %>% as.data.frame() %>% select(prediction)
          names(table_ml_gradient_boosted)[names(table_ml_gradient_boosted) == 'prediction'] <- v_grad$type_model
        }
        
        
        
        
        if (!is.na(v_random$type_model) & v_random$type_model == "ml_random_forest"){
          
          eval(parse(text = paste0("fit <- data_spark_train %>%",v_random$type_model,"(", y ," ~ " ,chaine_variable ,
                                   ",num_trees  =",t$num_tree_random_forest,
                                   ",subsampling_rate =",v$subsampling_rate_random_forest,
                                   ",max_depth  =",x$max_depth_random_forest,
                                   ")")))
          
          table_ml_random_forest <- sdf_predict(data_spark_test, fit) %>% collect %>% as.data.frame() %>% select(prediction)
          names(table_ml_random_forest)[names(table_ml_random_forest) == 'prediction'] <- v_random$type_model
          
        }
        
        if (!is.na(v_glm$type_model) & v_glm$type_model == "ml_generalized_linear_regression"){
          
          eval(parse(text = paste0("fit <- data_spark_train %>%",v_glm$type_model,"(", y ," ~ " ,chaine_variable ,
                                   # ",num_trees  =",t$num_tree_random_forest,
                                   # ",subsampling_rate =",v$subsampling_rate_random_forest,
                                   # ",max_depth  =",x$max_depth_random_forest,
                                   ")")))
          
          table_ml_glm <- sdf_predict(data_spark_test, fit) %>% collect %>% as.data.frame() %>% select(prediction)
          names(table_ml_glm)[names(table_ml_glm) == 'prediction'] <- v_glm$type_model
          
        }
        
        
        cbind(data_results,table_ml_gradient_boosted,table_ml_random_forest,table_ml_glm)
        
      })
      
      
      
      output$output_curve <- renderDygraph({
        dygraph(data = table_forecast())
      })
      
      
      output$test_result <- renderDataTable({table_forecast()})
      
      
      output$results_table <- renderDataTable(
        
        
        
        
        
        DT::datatable(table_forecast() %>% summarise(mape = 100 * median(abs((Valeur - eval(parse(text = u$type_model))) /eval(parse(text =  u$type_model)))),
                                                     rmse = sqrt(mean((Valeur - eval(parse(text =  u$type_model)))**2))),
                      options = list(searching = FALSE,paging = FALSE,columnDefs = list(list(width = '200px',targets = "_all"))))
      )
      
      
      
      
      observeEvent(input$train_selector,{
        updateSliderInput(session,'test_selector',
                          value= c(input$train_selector[2],input$test_selector[2]) ) 
      })
      
      observeEvent(input$test_selector,{
        updateSliderInput(session,'train_selector',
                          value= c(input$train_selector[1],input$test_selector[1]) ) 
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

dashforecast(share_app = TRUE ,port = 7895,data =sequence_dates ,x = c("jour","mois","numero_jour"), y = "Valeur",date_column = "Date")




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

mlp_model <- iris_tbl %>%
  #ml_random_forest(formula = Species ~ . )
  ml_multilayer_perceptron_classifier(Sepal_Length ~ Petal_Length, layers = c(4,3,3))

pred <- sdf_predict(iris_test, mlp_model)

ml_multiclass_classification_evaluator(pred)





data_spark_train <- sequence_dates[Date <= "2017-08-01",][1:10,] %>% 
  mutate(Valeur = as.integer(Valeur)) %>% 
  mutate(Valeur = round(Valeur,0)) %>% 
  mutate(ordi = ifelse(jour == 1,"oui","non"))


data_spark_test <- sequence_dates[Date >= "2017-08-01",][1:10,] %>% 
  mutate(Valeur = as.integer(Valeur)) %>% 
  mutate(Valeur = round(Valeur,0)) %>% 
  mutate(ordi = ifelse(jour == 1,"oui","non"))


data_spark_test <- eval(parse(text = paste0("data %>% filter(",date_column,"> input$test_selector[1]) %>% as.data.table()")))

data_spark_train <- copy_to(sc, data_spark_train, "data_spark_train2", overwrite = TRUE)
data_spark_test <- copy_to(sc, data_spark_test, "data_spark_test", overwrite = TRUE)





fit <- data_spark_train %>% ml_generalized_linear_regression(Valeur ~ jour + numero_jour)
fit <- data_spark_train %>% ml_logistic_regression(Valeur ~ jour + numero_jour)



sdf_predict(data_spark_test, fit) %>% collect %>% as.data.frame()
class(data_spark_train)

?ml_multilayer_perceptron
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


ml_random_forest()


