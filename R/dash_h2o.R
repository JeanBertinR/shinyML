#' @title Use Shiny and h2o machine learning framework to compare supervised regression models on time series
#'
#' @description This function creates in one line of code a shareable web app to compare supervised regression model performances.
#'
#' @param data Time serie containing one or more input values and one output value. 
#'    The time serie must be a data.frame or a data.table and must contain at least one time-based column on Date or Posixct format.
#' @param x Vector of numerical and categorical input variables used to train and test the model. Each element of x vector must correspond to a data column with either numerical or factor type.  
#' 
#' @param y the numerical output variable to forecast (must correpond to one data column)
#' 
#' @param date_column the name of time-based column ( must correspond to one data column)
#' 
#' @param share_app a logical value indicating whether the app must be shared on local LAN 
#' 
#' @param port a four-digit number corresponding to the port the application should listen to. This parameter is necessary only  if share_app option is set to TRUE
#' 
#' @return NULL
#'
#' @examples
#'\dontrun{
#' library(dashR)
#' library(dplyr)
#' Sys.setenv(http_proxy="") 
#' Sys.setenv(http_proxy_user="") 
#' Sys.setenv(https_proxy_user="")
#' longley2 <- longley %>% mutate(Year = as.Date(as.character(Year),format = "%Y"))
#' dash_h20(data =longley2,x = c("GNP_deflator","Unemployed" ,"Armed_Forces","Employed"),
#'   y = "GNP",date_column = "Year",share_app = TRUE,port = 3951)
#'}
#' @import shiny shinydashboard dygraphs data.table  ggplot2
#' @importFrom dplyr %>% select mutate group_by summarise arrange rename
#' @importFrom tidyr gather
#' @importFrom DT datatable
#' @importFrom h2o h2o.init as.h2o h2o.deeplearning h2o.varimp h2o.predict h2o.gbm h2o.glm h2o.randomForest
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom shinyWidgets materialSwitch
#' @importFrom stats predict reorder
#' 
#' 
#' @export

dash_h20 <- function(data = data,x,y,date_column, share_app = FALSE,port = NULL ){

  h2o.init(port = 54321)
  

  data <- data.table(data)
  
  app <- shinyApp(
    
    ui = dashboardPage(skin = "black",
            dashboardHeader(title = "H2O"),
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),
                      materialSwitch(inputId = "bar_chart_mode",label = "Bar chart mode",status = "primary",value = TRUE)
                           )
                         )),
                       
            dashboardBody(
              fluidPage(
                column(width = 12,
                  column(width = 8,
                    fluidRow(
                      column(width = 12,box(dygraphOutput("input_curve", height = 120, width = 930),width = 12)),
                      column(width = 12,
                                    tabBox(id = "results_models",
                                                           tabPanel("Result charts on test period",dygraphOutput("output_curve", height = 200, width = 930)),
                                                           tabPanel("Compare models performances",dataTableOutput("date_essai")),
                                                           tabPanel("Feature importance",plotlyOutput("feature_importance")),
                                                           tabPanel("Table of results",dataTableOutput("table_of_results")), width = 12
                                             )
                                           )
                                         )
                                         
                                  ),
                                  
                  column(width = 4,align="center",
                                fluidRow(
                                  box(
                                             title = "Controls",
                                             selectInput( inputId  = "input_variables",label = "Input variables: ",choices = x,multiple = TRUE,selected = x),
                                             sliderInput("train_selector", "Choose train period:",
                                                         min = eval(parse(text = paste0("min(data$",date_column,")"))),
                                                         max = eval(parse(text = paste0("max(data$",date_column,")"))),
                                                         value =  eval(parse(text = paste0("c(min(data$",date_column,"),mean(data$",date_column,"))")))),
                                             sliderInput("test_selector", "Choose test period:",
                                                         min = eval(parse(text = paste0("min(data$",date_column,")"))),
                                                         max = eval(parse(text = paste0("max(data$",date_column,")"))),
                                                         value = eval(parse(text = paste0("c(mean(data$",date_column,"),max(data$",date_column,"))")))),
                                             actionButton("train_all","Run all models !",style = 'color:white; background-color:red; padding:4px; font-size:150%',
                                                          icon = icon("cogs",lib = "font-awesome")),width = 12,height = 425
                                           )
                                         )
                                  )
                           ),
                           
                column(width = 12,align = "center",
                              fluidRow(
                                    
                                box(
                                      title = "Generalized linear regresion",status = "warning",
                                      column(
                                        radioButtons(label = "Family",inputId = "glm_family",choices = c("gaussian","poisson", "gamma","tweedie"),
                                                     selected = "gaussian"),width = 6),
                                      column(
                                        radioButtons(label = "Link",inputId = "glm_link",choices = c("identity","log"),selected = "identity"),width = 6),
                                      
                                      materialSwitch(label = "Intercept term",inputId = "intercept_term_glm",status = "primary",value = TRUE),
                                      sliderInput(label = "Lambda",inputId = "reg_param_glm",min = 0,max = 10,value = 0),
                                      sliderInput(label = "Alpha (0:Ridge <-> 1:Lasso)",inputId = "alpha_param_glm",min = 0,max = 1,value = 0),
                                      sliderInput(label = "Maximum iteraions",inputId = "max_iter_glm",min = 50,max = 300,value = 100),
                                      actionButton("run_glm","Run generalized linear regression",style = 'color:white; background-color:orange; padding:4px; font-size:150%',
                                                   icon = icon("cogs",lib = "font-awesome"))
                                      ,width = 3 ),
                                    
                                box(
                                      title = "Random Forest",status = "danger",
                                      
                                      sliderInput(label = "Number of trees",min = 1,max = 100, inputId = "num_tree_random_forest",value = 20),
                                      sliderInput(label = "Subsampling rate",min = 0.1,max = 1, inputId = "subsampling_rate_random_forest",value = 1),
                                      sliderInput(label = "Max depth",min = 1,max = 20, inputId = "max_depth_random_forest",value = 5),
                                      actionButton("run_random_forest","Run random forest model",style = 'color:white; background-color:darkblue; padding:4px; font-size:150%',
                                                   icon = icon("users",lib = "font-awesome"))
                                      
                                      ,width = 3),
                                    
                                    
                                box(
                                      title = "Neural network model",status = "primary",
                                      
                                      column(
                                        radioButtons(label = "Activation function",inputId = "activation_neural_net",
                                                     choices = c( "Rectifier", "Maxout","Tanh", "RectifierWithDropout", "MaxoutWithDropout","TanhWithDropout"),selected = "Rectifier"),
                                        width = 6),
                                      column(
                                        radioButtons(label = "Loss function",inputId = "loss_neural_net",
                                                     choices = c("Automatic", "Quadratic", "Huber", "Absolute", "Quantile"),selected = "Automatic"),
                                        width = 6),
                                      
                                      textInput(label = "Hidden layers",inputId = "hidden_neural_net",value = "c(200,200)"),
                                      sliderInput(label = "Epochs",min = 10,max = 100, inputId = "epochs_neural_net",value = 10),
                                      sliderInput(label = "Learning rate",min = 0.001,max = 0.1, inputId = "rate_neural_net",value = 0.005),
                                      
                                      actionButton("run_neural_network","Run neural network regression",style = 'color:white; background-color:orange; padding:4px; font-size:150%',
                                                   icon = icon("cogs",lib = "font-awesome"))
                                      ,width = 3 ),
                                    
                                    
                                    
                                    
                                box(
                                      title = "Gradient boosting trees",status = "success",
                                      
                                      
                                      sliderInput(label = "Max depth",min = 1,max = 20, inputId = "max_depth_gbm",value = 5),
                                      sliderInput(label = "Number of trees",min = 1,max = 100, inputId = "n_trees_gbm",value = 50),
                                      
                                      sliderInput(label = "Subsampling rate",min = 0.1,max = 1, inputId = "subsampling_rate_gbm",value = 1),
                                      sliderInput(label = "Learn rate",min = 0.1,max = 1, inputId = "learn_rate_gbm",value = 0.1),
                                      
                                      actionButton("run_gradient_boosting","Run gradient boosting model",style = 'color:white; background-color:darkgreen; padding:4px; font-size:150%',
                                                   icon = icon("users",lib = "font-awesome"))
                                      
                                      ,width = 3)
                                  )
                           )
                         )
                       )
    ),
    
    
    # 
    # dataTableOutput("date_essai")
    
    
    
    server = function(session, input, output) {
      set.seed(122)
      model <- reactiveValues()
      test_1 <- reactiveValues(date = eval(parse(text = paste0("mean(data$",date_column,")"))))
      table_neural_network <- data.table(`Neural network` = NA)
      table_gradient_boosting <- data.table(`Gradient boosted trees` = NA)
      table_glm <- data.table(`Generalized linear regression` = NA)
      table_random_forest <-data.table(`Random forest` = NA) 
      
      
      
      v_neural <- reactiveValues(type_model = NA)
      v_grad <- reactiveValues(type_model = NA)
      v_glm <- reactiveValues(type_model = NA)
      v_random <- reactiveValues(type_model = NA)
      
      
      t <- reactiveValues()
      v <- reactiveValues()
      f <- reactiveValues()
      x <- reactiveValues()
      k <- reactiveValues()
      
      time_gbm <- data.table()
      time_random_forest <- data.table()
      time_glm <- data.table()
      time_neural_network <- data.table()
      
      
      importance_gbm <- data.table()
      importance_random_forest <- data.table()
      importance_neural_network <- data.table()
      
      scaled_importance <- NULL
      variable <- NULL
      Predicted_value <- NULL
      Model <- NULL
      `.` <- NULL
      `MAPE(%)` <- NULL
      
      observeEvent(input$train_all,{
        
        test_1$date <- input$test_selector[1]
        model$train_variables <- input$input_variables
        v_neural$type_model <- "ml_neural_network"
        v_grad$type_model <- "ml_gradient_boosted_trees"
        v_glm$type_model <- "ml_generalized_linear_regression"
        v_random$type_model <- "ml_random_forest"
        
        f$family_glm <- input$glm_family
        x$reg_param_glm <- input$reg_param_glm
        x$alpha_param_glm <- input$alpha_param_glm
        x$max_iter_glm <- input$max_iter_glm
        
        
        t$num_tree_random_forest <- input$num_tree_random_forest
        v$subsampling_rate_random_forest <- input$subsampling_rate_random_forest
        x$max_depth_random_forest <-  input$max_depth_random_forest
        
        v$subsampling_rate_gbm <- input$subsampling_rate_gbm
        x$n_trees_gbm <- input$n_trees_gbm
        x$max_depth_gbm <- input$max_depth_gbm
        x$learn_rate_gbm <- input$learn_rate_gbm
        
        k$hidden_neural_net <- input$hidden_neural_net
        x$epochs_neural_net <- input$epochs_neural_net
        f$activation_neural_net <- input$activation_neural_net
        f$loss_neural_net <- input$loss_neural_net
        x$rate_neural_net <- input$rate_neural_net
      })
      
      
      
      
      observeEvent(input$run_neural_network,{
        
        test_1$date <- input$test_selector[1]
        model$train_variables <- input$input_variables
        v_neural$type_model <- "ml_neural_network"
        v_grad$type_model <- NA
        v_glm$type_model <- NA
        v_random$type_model <- NA
        
        k$hidden_neural_net <- input$hidden_neural_net
        x$epochs_neural_net <- input$epochs_neural_net
        f$activation_neural_net <- input$activation_neural_net
        f$loss_neural_net <- input$loss_neural_net
        x$rate_neural_net <- input$rate_neural_net
      })
      
      
      observeEvent(input$run_gradient_boosting,{
        
        test_1$date <- input$test_selector[1]
        model$train_variables <- input$input_variables
        v_grad$type_model <- "ml_gradient_boosted_trees"
        v_neural$type_model <- NA
        v_glm$type_model <- NA
        v_random$type_model <- NA
        
        v$subsampling_rate_gbm <- input$subsampling_rate_gbm
        x$n_trees_gbm <- input$n_trees_gbm
        x$max_depth_gbm <- input$max_depth_gbm
        x$learn_rate_gbm <- input$learn_rate_gbm
      })
      
      
      observeEvent(input$run_glm,{
        
        test_1$date <- input$test_selector[1]
        model$train_variables <- input$input_variables
        v_grad$type_model <- NA
        v_neural$type_model <- NA
        v_random$type_model <- NA
        v_glm$type_model <- "ml_generalized_linear_regression"
        
        f$family_glm <- input$glm_family
        i$intercept_glm <- input$intercept_term_glm
        x$reg_param_glm <- input$reg_param_glm
        x$alpha_param_glm <- input$alpha_param_glm
        x$max_iter_glm <- input$max_iter_glm
      })
      
      
      observeEvent(input$run_random_forest,{
        
        test_1$date <- input$test_selector[1]
        model$train_variables <- input$input_variables
        v_grad$type_model <- NA
        v_neural$type_model <- NA
        v_glm$type_model <- NA
        v_random$type_model <- "ml_random_forest"
        
        t$num_tree_random_forest <- input$num_tree_random_forest
        v$subsampling_rate_random_forest <- input$subsampling_rate_random_forest
        x$max_depth_random_forest <-  input$max_depth_random_forest
        
      })
      
      
      
      output$input_curve <- renderDygraph({
        
        
        curve_entries <- dygraph(data = eval(parse(text = paste0("data.table::data.table:::`[.data.table::data.table`(data,j =.(",date_column,",",y,"))"))))  %>%
          dyShading(from = input$train_selector[1],to = input$train_selector[2],color = "snow" ) %>%
          dyShading(from = input$test_selector[1],to = input$test_selector[2],color = "azure" ) %>%
          dyEvent(x = input$train_selector[1]) %>%
          dyEvent(x = input$train_selector[2]) %>%
          dyEvent(x = input$test_selector[2]) %>%
          dySeries(y,fillGraph = TRUE) %>% 
          dyAxis("y",valueRange = c(0,1.5 * max(eval(parse(text =paste0("data$",y))))))
        
        
        
        if (input$bar_chart_mode == TRUE){
          curve_entries <- curve_entries %>% dyBarChart()
        }
        curve_entries
        
      })
      
      
       
      output$output_curve <- renderDygraph({
        
        
        output_dygraph <- dygraph(data = table_forecast()[['results']]) %>%
          dyAxis("y",valueRange = c(0,1.5 * max(eval(parse(text =paste0("table_forecast()[['results']]$",y)))))) 
        
        if (input$bar_chart_mode == TRUE){
          output_dygraph <- output_dygraph %>% dyBarChart()
        }
        
        output_dygraph %>% dyLegend(width = 800)
        
        
        
      })
      
      
      
      table_forecast <- reactive({
        
        data_results <- eval(parse(text = paste0("data[,.(",date_column,",",y,")][",date_column,">'",test_1$date,"',]")))
        var_input_list <- c()
        
        for (i in 1:length(model$train_variables)){
          var_input_list <- c(var_input_list,model$train_variables[i])
          
        }
        
        if (length(var_input_list) != 0){   
          
          data_train <- eval(parse(text = paste0("data %>% filter(",date_column,"<='", test_1$date,"') %>% as.data.table()")))
          data_test <- eval(parse(text = paste0("data %>% filter(",date_column,">'", test_1$date,"') %>% as.data.table()")))
          
          data_h2o_train <- as.h2o(data_train)
          data_h2o_test <- as.h2o(data_test)
          
          if (!is.na(v_neural$type_model) & v_neural$type_model == "ml_neural_network"){
            
            t1 <- Sys.time()
            dl_fit1 <- h2o.deeplearning(x = as.character(var_input_list),
                                        y = y,
                                        training_frame = data_h2o_train,
                                        model_id = "dl_fit1",
                                        activation = f$activation_neural_net,
                                        loss = f$loss_neural_net,
                                        hidden = eval(parse(text = k$hidden_neural_net)) ,
                                        epochs = x$epochs_neural_net,
                                        rate = x$rate_neural_net,
                                        seed = 1)
            t2 <- Sys.time()
            
            time_neural_network <- data.frame(`Training time` =  paste0(round(t2 - t1,1)," seconds"), Model = "Neural network")
            importance_neural_network <- h2o.varimp(dl_fit1) %>% as.data.table() %>% select(`variable`,scaled_importance) %>% mutate(model = "Neural network")
            table_neural_network <- h2o.predict(dl_fit1,data_h2o_test) %>% as.data.table() %>% mutate(predict = round(predict,3)) %>% rename(`Neural network` = predict)
            
          }
          
          
          if (!is.na(v_grad$type_model) & v_grad$type_model == "ml_gradient_boosted_trees"){
            
            t1 <- Sys.time()
            dl_fit1 <- h2o.gbm(x = as.character(var_input_list),
                               y = y,
                               sample_rate = v$subsampling_rate_gbm,
                               ntrees = x$n_trees_gbm,
                               max_depth = x$max_depth_gbm,
                               learn_rate = x$learn_rate_gbm,
                               training_frame = data_h2o_train,
                               model_id = "dl_fit1",min_rows = 1,
                               seed = 1)
            t2 <- Sys.time()
            time_gbm <- data.frame(`Training time` =  paste0(round(t2 - t1,1)," seconds"), Model = "Gradient boosted trees") 
            importance_gbm <- h2o.varimp(dl_fit1) %>% as.data.table() %>% select(`variable`,scaled_importance) %>% mutate(model = "Gradient boosted trees")
            table_gradient_boosting <- h2o.predict(dl_fit1,data_h2o_test) %>% as.data.table() %>% mutate(predict = round(predict,3)) %>% rename(`Gradient boosted trees` = predict)
            
          }
          
          
          if (!is.na(v_glm$type_model) & v_glm$type_model == "ml_generalized_linear_regression"){
            
            t1 <- Sys.time()
            dl_fit1 <- h2o.glm(x = as.character(var_input_list),
                               y = y,
                               family = f$family_glm,
                               intercept = input$intercept_term_glm,
                               lambda = x$reg_param_glm,
                               alpha = x$alpha_param_glm,
                               max_iterations = x$max_iter_glm,
                               training_frame = data_h2o_train,
                               model_id = "dl_fit1",
                               seed = 1)
            t2 <- Sys.time()
            time_glm <- data.frame(`Training time` =  paste0(round(t2 - t1,1)," seconds"), Model = "Generalized linear regression")
            table_glm <- h2o.predict(dl_fit1,data_h2o_test) %>% as.data.table() %>% mutate(predict = round(predict,3)) %>% rename(`Generalized linear regression` = predict)
            
          }
          
          
          if (!is.na(v_random$type_model) & v_random$type_model == "ml_random_forest"){
            
            t1 <- Sys.time()
            dl_fit1 <- h2o.randomForest(x = as.character(var_input_list),
                                        y = y,
                                        ntrees = t$num_tree_random_forest,
                                        sample_rate = v$subsampling_rate_random_forest,
                                        max_depth = x$max_depth_random_forest,
                                        training_frame = data_h2o_train,
                                        model_id = "dl_fit1",
                                        seed = 1)
            t2 <- Sys.time()
            time_random_forest <- data.frame(`Training time` =  paste0(round(t2 - t1,1)," seconds"), Model = "Random forest")
            importance_random_forest <- h2o.varimp(dl_fit1) %>% as.data.table() %>% select(`variable`,scaled_importance) %>% mutate(model = "Random forest")
            table_random_forest<- h2o.predict(dl_fit1,data_h2o_test) %>% as.data.table() %>% mutate(predict = round(predict,3))  %>% rename(`Random forest` = predict)
            
          }
          
          
          
          
          
        }
        
        
        table_training_time <- rbind(time_gbm,time_random_forest,time_glm,time_neural_network)
        table_importance <- rbind(importance_gbm,importance_random_forest,importance_neural_network) %>% as.data.table()
        table_results <- cbind(data_results,table_glm,table_random_forest,table_neural_network,table_gradient_boosting)%>% as.data.table()
        
        list(traning_time = table_training_time, table_importance = table_importance, results = table_results)
        
        
      })
      
      
      output$date_essai <- renderDataTable({
        
        
        performance_table <-  table_forecast()[['results']] %>% 
          gather(key = Model,value = Predicted_value,-date_column,-y) %>% 
          group_by(Model) %>% 
          summarise(`MAPE(%)` = round(100 * mean(abs((Predicted_value - eval(parse(text = y)))/eval(parse(text = y))),na.rm = TRUE),1),
                    RMSE = round(sqrt(mean((Predicted_value - eval(parse(text = y)))**2)),0)) 
        
        if (nrow(table_forecast()[['traning_time']]) != 0){
          performance_table <- performance_table %>% merge(.,table_forecast()[['traning_time']],by = "Model")
        }
        
        datatable(
          performance_table %>% arrange(`MAPE(%)`) %>% as.data.table()
          , extensions = 'Buttons', options = list(dom = 'Bfrtip',buttons = c('csv', 'excel', 'pdf', 'print'))
        )
      })
      
      
      output$table_of_results <- renderDataTable({
        
        datatable(
          table_forecast()[['results']],
          extensions = 'Buttons', options = list(dom = 'Bfrtip',buttons = c('csv', 'excel', 'pdf', 'print'))
        ) 
        
        
      })
      
      
      output$table_test <-renderDataTable({
        
        table_forecast()[['table_importance']]
      })
      
      
      output$feature_importance <- renderPlotly({
        
        if (nrow(table_forecast()[['table_importance']]) != 0){
          ggplotly(
            ggplot(data = table_forecast()[['importance']])+
              ggplot2::geom_bar(aes(reorder(variable,scaled_importance),scaled_importance,fill =  model),stat = "identity",width = 0.3)+
              facet_wrap(~ model)+
              coord_flip()+
              xlab("")+
              ylab("")+
              theme(legend.position="none")
          )
        }
        
      })
      
      
      
      
      
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
  
  
}





