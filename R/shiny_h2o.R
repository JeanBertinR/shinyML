#' @title Implement a shiny web app to compare h2o supervised regression models on time series
#'
#' @description This function creates in one line of code a shareable web app to compare supervised regression model performance (framework: H2O).
#'
#' @param data Time serie containing one or more input values and one output value. 
#'    The time serie must be a data.frame or a data.table and must contain at least one time-based column on Date or POSIXct format.
#' @param x Vector of numerical and categorical input variables used to train and test the model. Each element of x vector must correspond to a data column with either numerical or factor type.  
#' 
#' @param y the numerical output variable to forecast (must correpond to one data column)
#' 
#' @param date_column the name of time-based column ( must correspond to one data column). Must correspond to Date or POSIXct format. 
#' 
#' @param share_app a logical value indicating whether the app must be shared on local LAN 
#' 
#' @param port a four-digit number corresponding to the port the application should listen to. This parameter is necessary only  if share_app option is set to TRUE
#' 
#' @return NULL
#'
#' @examples
#'\dontrun{
#' library(shinyML)
#' longley2 <- longley %>% mutate(Year = as.Date(as.character(Year),format = "%Y"))
#' shiny_h2o(data =longley2,x = c("GNP_deflator","Unemployed" ,"Armed_Forces","Employed"),
#'   y = "GNP",date_column = "Year",share_app = FALSE)
#'}
#' @import shiny shinydashboard dygraphs data.table ggplot2 shinycssloaders
#' @importFrom dplyr %>% select mutate group_by summarise arrange rename select_if
#' @importFrom tidyr gather
#' @importFrom DT renderDT DTOutput datatable
#' @importFrom h2o h2o.init as.h2o h2o.deeplearning h2o.varimp h2o.predict h2o.gbm h2o.glm h2o.randomForest h2o.automl h2o.clusterStatus
#' @importFrom plotly plotlyOutput renderPlotly ggplotly plot_ly
#' @importFrom shinyWidgets materialSwitch sendSweetAlert knobInput
#' @importFrom stats predict reorder cor
#' 
#' 
#' @export

shiny_h2o <- function(data = data,x,y,date_column, share_app = FALSE,port = NULL){
  
  
  # Convert input data must be a data table object
  data <- data.table(data)
  
  # Run h2o instance (might require to unset proxy authentification credentials )
  Sys.setenv(http_proxy="")
  Sys.setenv(http_proxy_user="")
  Sys.setenv(https_proxy_user="")
  h2o.init()
  h2o::h2o.no_progress()
  cluster_status <- h2o.clusterStatus()
  
  # Replace '.' by '_' in dataset column names ( if necessary )
  x <- gsub("\\_",".",x)
  
  # Test if date_column class correspond to Date or POSIXct
  if (!(eval(parse(text = paste0("class(data$",date_column,")"))) %in% c("Date","POSIXct"))){
    stop("date_column class must be Date or POSIXct")
  }
  
  # Test if y class correspond to numeric
  if (!(eval(parse(text = paste0("class(data$",y,")"))) == "numeric")){
    stop("y column class must be numeric")
  }
  
  # Test if input data does not exceed one million rows
  if (nrow(data) > 1000000) {
    stop("Input dataset must not exceed one million rows")
  }
  
  
  app <- shinyApp(
    
    # Define ui side of shiny app
    ui = dashboardPage(skin = "black",
                       dashboardHeader(title = "H2O"),
                       dashboardSidebar(
                         sidebarMenu(
                           menuItem(
                             materialSwitch(inputId = "bar_chart_mode",label = "Bar chart mode",status = "primary",value = TRUE)
                           ),
                           br(),
                           # Modify size of font awesome icons 
                           tags$head( 
                             tags$style(HTML(".fa { font-size: 40px; }"))
                           ),
                           valueBoxOutput("h2o_cluster_mem",width = 12),
                           valueBoxOutput("h2o_cpu",width = 12)
                           
                         )),
                       
                       dashboardBody(
                         fluidPage(
                           column(width = 12,
                                  column(width = 8,
                                         fluidRow(
                                           column(width = 12,
                                                  tabBox(id = "explore_input_data", 
                                                         tabPanel("Input data chart",withSpinner(dygraphOutput("input_curve", height = 180, width = 1100))),
                                                         tabPanel("Variables Summary",
                                                                  fluidRow( 
                                                                    column(width = 6,
                                                                           withSpinner(DTOutput("variables_class_input", height = 180, width = 500))),
                                                                    column(width = 6,
                                                                           withSpinner(plotlyOutput("variable_boxplot", height = 180, width = 500)))
                                                                  )
                                                         ),
                                                         tabPanel("Correlation matrix",withSpinner(plotlyOutput("correlation_matrix", height = 180, width = 1100))),
                                                         width = 12)
                                           ),
                                           column(width = 12,
                                                  tabBox(id = "results_models",
                                                         tabPanel("Result charts on test period",withSpinner(dygraphOutput("output_curve", height = 200, width = 1100))),
                                                         tabPanel("Compare models performances",withSpinner(DTOutput("score_table"))),
                                                         tabPanel("Feature importance",withSpinner(plotlyOutput("feature_importance"))),
                                                         tabPanel("Table of results",withSpinner(DTOutput("table_of_results"))), width = 12
                                                  )
                                           )
                                         )
                                         
                                  ),
                                  
                                  column(width = 4,align="center",
                                         fluidRow(
                                           box(width = 12,
                                               tabBox(id = "global_tabbox",width = 12,
                                                      tabPanel("Global parameters",
                                                               selectInput( inputId  = "input_variables",label = "Input variables: ",choices = x,multiple = TRUE,selected = x),
                                                               sliderInput("train_selector", "Choose train period:",
                                                                           min = eval(parse(text = paste0("min(data$",date_column,")"))),
                                                                           max = eval(parse(text = paste0("max(data$",date_column,")"))),
                                                                           value =  eval(parse(text = paste0("c(min(data$",date_column,"),mean(data$",date_column,"))")))),
                                                               sliderInput("test_selector", "Choose test period:",
                                                                           min = eval(parse(text = paste0("min(data$",date_column,")"))),
                                                                           max = eval(parse(text = paste0("max(data$",date_column,")"))),
                                                                           value = eval(parse(text = paste0("c(mean(data$",date_column,"),max(data$",date_column,"))")))),
                                                               actionButton("train_all","Run tuned models !",style = 'color:white; background-color:red; padding:4px; font-size:150%',
                                                                            icon = icon("cogs",lib = "font-awesome")),
                                                               width = 12,height = 425),
                                                      tabPanel("Auto ML",
                                                               br(),
                                                               knobInput(inputId = "run_time_auto_ml",label = "Max running time (in seconds)",value = 15,min = 10,max = 60,
                                                                         displayPrevious = TRUE, lineCap = "round",fgColor = "#428BCA",inputColor = "#428BCA"
                                                                         
                                                               ),
                                                               
                                                               br(),
                                                               actionButton("run_auto_ml","Run auto ML",style = 'color:white; background-color:red; padding:4px; font-size:150%',
                                                                            icon = icon("cogs",lib = "font-awesome")))
                                               )
                                           )
                                         )
                                  )
                           ),
                           
                           column(width = 12,align = "center",
                                  fluidRow(
                                    
                                    # Define UI objects for generalized linear regression box
                                    box(
                                      title = "Generalized linear regresion",status = "warning",
                                      column(
                                        radioButtons(label = "Family",inputId = "glm_family",choices = c("gaussian","poisson", "gamma","tweedie"),
                                                     selected = "gaussian"),width = 6),
                                      column(
                                        radioButtons(label = "Link",inputId = "glm_link",choices = c("identity","log"),selected = "identity"),width = 6),
                                      
                                      materialSwitch(label = "Intercept term",inputId = "intercept_term_glm",status = "primary",value = TRUE),
                                      sliderInput(label = "Lambda",inputId = "reg_param_glm",min = 0,max = 10,value = 0),
                                      sliderInput(label = "Alpha (0:Ridge <-> 1:Lasso)",inputId = "alpha_param_glm",min = 0,max = 1,value = 0.5),
                                      sliderInput(label = "Maximum iteraions",inputId = "max_iter_glm",min = 50,max = 300,value = 100),
                                      actionButton("run_glm","Run generalized linear regression",style = 'color:white; background-color:orange; padding:4px; font-size:150%',
                                                   icon = icon("cogs",lib = "font-awesome"))
                                      ,width = 3
                                      
                                    ),
                                    
                                    # Define UI objects for Random Forest box
                                    box(
                                      title = "Random Forest",status = "danger",
                                      
                                      sliderInput(label = "Number of trees",min = 1,max = 100, inputId = "num_tree_random_forest",value = 50),
                                      sliderInput(label = "Subsampling rate",min = 0.1,max = 1, inputId = "subsampling_rate_random_forest",value = 0.6),
                                      sliderInput(label = "Max depth",min = 1,max = 50, inputId = "max_depth_random_forest",value = 20),
                                      sliderInput(label = "Number of bins",min = 2,max = 100, inputId = "n_bins_random_forest",value = 20),
                                      actionButton("run_random_forest","Run random forest model",style = 'color:white; background-color:red; padding:4px; font-size:150%',
                                                   icon = icon("cogs",lib = "font-awesome"))
                                      
                                      ,width = 3
                                      
                                    ),
                                    
                                    
                                    # Define UI objects for Neural Network box
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
                                      column(
                                        textInput(label = "Hidden layers",inputId = "hidden_neural_net",value = "c(200,200)"),
                                        sliderInput(label = "Epochs",min = 10,max = 100, inputId = "epochs_neural_net",value = 10),
                                        sliderInput(label = "Learning rate",min = 0.001,max = 0.1, inputId = "rate_neural_net",value = 0.005),
                                        actionButton("run_neural_network","Run neural network regression",style = 'color:white; background-color:darkblue; padding:4px; font-size:150%',
                                                     icon = icon("cogs",lib = "font-awesome")),width = 12)
                                      ,width = 3
                                      
                                    ),
                                    
                                    # Define UI objects for Gradient boosting box
                                    box(
                                      title = "Gradient boosting trees",status = "success",
                                      
                                      
                                      sliderInput(label = "Max depth",min = 1,max = 20, inputId = "max_depth_gbm",value = 5),
                                      sliderInput(label = "Number of trees",min = 1,max = 100, inputId = "n_trees_gbm",value = 50),
                                      sliderInput(label = "Sample rate",min = 0.1,max = 1, inputId = "sample_rate_gbm",value = 1),
                                      sliderInput(label = "Learn rate",min = 0.1,max = 1, inputId = "learn_rate_gbm",value = 0.1),
                                      actionButton("run_gradient_boosting","Run gradient boosting model",style = 'color:white; background-color:darkgreen; padding:4px; font-size:150%',
                                                   icon = icon("cogs",lib = "font-awesome"))
                                      
                                      ,width = 3
                                      
                                    )
                                    
                                  )
                                  
                           )
                         )
                       )
    ),
    
    # Define server side of shiny app
    server = function(session, input, output) {
      
      # Initialize all variables
      model <- reactiveValues()
      train_1 <- reactiveValues()
      
      # By default, start date and stop dates for test period correspond to mean and max of values of date_colum
      test_1 <- reactiveValues(date = eval(parse(text = paste0("mean(data$",date_column,")"))))
      test_2 <- reactiveValues(date = eval(parse(text = paste0("max(data$",date_column,")"))))
      
      # Will be used to activate all models calculation when the user click to "Run tuned model" button
      v_neural <- reactiveValues(type_model = NA)
      v_grad <- reactiveValues(type_model = NA)
      v_glm <- reactiveValues(type_model = NA)
      v_random <- reactiveValues(type_model = NA)
      v_auto_ml <- reactiveValues(type_model = NA)
      
      parameter <- reactiveValues()
      
      # Intitalization of calculation time per model
      time_gbm <- data.table()
      time_random_forest <- data.table()
      time_glm <- data.table()
      time_neural_network <- data.table()
      time_auto_ml <- data.table()
      
      # Intitalization of variables importances per model (not available for generalized linear regression)
      importance_gbm <- data.table()
      importance_random_forest <- data.table()
      importance_neural_network <- data.table()
      
      scaled_importance <- NULL
      variable <- NULL
      Predicted_value <- NULL
      Model <- NULL
      `.` <- NULL
      `MAPE(%)` <- NULL
      
      
      # Make all parameters correspond to cursors and radiobuttons choices when user click on "Run tuned models!" button
      observeEvent(input$train_all,{
        
        train_1$date <- input$train_selector[1]
        test_1$date <- input$test_selector[1]
        test_2$date <- input$test_selector[2]
        
        model$train_variables <- input$input_variables
        
        v_neural$type_model <- "ml_neural_network"
        v_grad$type_model <- "ml_gradient_boosted_trees"
        v_glm$type_model <- "ml_generalized_linear_regression"
        v_random$type_model <- "ml_random_forest"
        v_auto_ml$type_model <- NA
        
        parameter$family_glm <- input$glm_family
        parameter$reg_param_glm <- input$reg_param_glm
        parameter$alpha_param_glm <- input$alpha_param_glm
        parameter$max_iter_glm <- input$max_iter_glm
        
        
        parameter$num_tree_random_forest <- input$num_tree_random_forest
        parameter$subsampling_rate_random_forest <- input$subsampling_rate_random_forest
        parameter$max_depth_random_forest <-  input$max_depth_random_forest
        parameter$n_bins_random_forest <- input$n_bins_random_forest
        
        parameter$sample_rate_gbm <- input$sample_rate_gbm
        parameter$n_trees_gbm <- input$n_trees_gbm
        parameter$max_depth_gbm <- input$max_depth_gbm
        parameter$learn_rate_gbm <- input$learn_rate_gbm
        
        parameter$hidden_neural_net <- input$hidden_neural_net
        parameter$epochs_neural_net <- input$epochs_neural_net
        parameter$activation_neural_net <- input$activation_neural_net
        parameter$loss_neural_net <- input$loss_neural_net
        parameter$rate_neural_net <- input$rate_neural_net
        
        showTab(inputId = "results_models", target = "Feature importance")
        
        
      })
      
      # Make glm parameters correspond to cursors and radiobuttons choices when user click on "Run generalized linear regression" button (and disable other models)
      observeEvent(input$run_glm,{
        
        train_1$date <- input$train_selector[1]
        test_1$date <- input$test_selector[1]
        test_2$date <- input$test_selector[2]
        model$train_variables <- input$input_variables
        v_grad$type_model <- NA
        v_neural$type_model <- NA
        v_random$type_model <- NA
        v_auto_ml$type_model <- NA
        
        v_glm$type_model <- "ml_generalized_linear_regression"
        
        parameter$family_glm <- input$glm_family
        parameter$reg_param_glm <- input$reg_param_glm
        parameter$alpha_param_glm <- input$alpha_param_glm
        parameter$max_iter_glm <- input$max_iter_glm
        
        hideTab(inputId = "results_models", target = "Feature importance")
      })
      
      
      # Make random forest parameters correspond to cursors when user click on "Run random forest model" button (and disable other models)
      observeEvent(input$run_random_forest,{
        
        train_1$date <- input$train_selector[1]
        test_1$date <- input$test_selector[1]
        test_2$date <- input$test_selector[2]
        model$train_variables <- input$input_variables
        v_grad$type_model <- NA
        v_neural$type_model <- NA
        v_glm$type_model <- NA
        v_auto_ml$type_model <- NA
        
        v_random$type_model <- "ml_random_forest"
        
        
        parameter$num_tree_random_forest <- input$num_tree_random_forest
        parameter$subsampling_rate_random_forest <- input$subsampling_rate_random_forest
        parameter$max_depth_random_forest <-  input$max_depth_random_forest
        parameter$n_bins_random_forest <- input$n_bins_random_forest
        
        showTab(inputId = "results_models", target = "Feature importance")
        
      })
      
      
      # Make neural network parameters correspond to cursors and radiobuttons choices when user click on "Run neural network regression" button (and disable other models)
      observeEvent(input$run_neural_network,{
        
        train_1$date <- input$train_selector[1]
        test_1$date <- input$test_selector[1]
        test_2$date <- input$test_selector[2]
        model$train_variables <- input$input_variables
        
        v_neural$type_model <- "ml_neural_network"
        v_grad$type_model <- NA
        v_glm$type_model <- NA
        v_random$type_model <- NA
        v_auto_ml$type_model <- NA
        
        parameter$hidden_neural_net <- input$hidden_neural_net
        parameter$epochs_neural_net <- input$epochs_neural_net
        parameter$activation_neural_net <- input$activation_neural_net
        parameter$loss_neural_net <- input$loss_neural_net
        parameter$rate_neural_net <- input$rate_neural_net
        
        showTab(inputId = "results_models", target = "Feature importance")
      })
      
      # Make gradient boosting parameters correspond to cursors when user click on "Run gradient boosting model" button (and disable other models)
      observeEvent(input$run_gradient_boosting,{
        
        train_1$date <- input$train_selector[1]
        test_1$date <- input$test_selector[1]
        test_2$date <- input$test_selector[2]
        model$train_variables <- input$input_variables
        v_grad$type_model <- "ml_gradient_boosted_trees"
        v_neural$type_model <- NA
        v_glm$type_model <- NA
        v_random$type_model <- NA
        v_auto_ml$type_model <- NA
        
        parameter$sample_rate_gbm <- input$sample_rate_gbm
        parameter$n_trees_gbm <- input$n_trees_gbm
        parameter$max_depth_gbm <- input$max_depth_gbm
        parameter$learn_rate_gbm <- input$learn_rate_gbm
        
        showTab(inputId = "results_models", target = "Feature importance")
      })
      
      
      
      observeEvent(input$run_auto_ml,{
        
        train_1$date <- input$train_selector[1]
        test_1$date <- input$test_selector[1]
        test_2$date <- input$test_selector[2]
        model$train_variables <- input$input_variables
        
        v_grad$type_model <- NA
        v_neural$type_model <- NA
        v_glm$type_model <- NA
        v_random$type_model <- NA
        v_auto_ml$type_model <- "ml_auto"
        
        parameter$run_time_auto_ml <-  input$run_time_auto_ml
        hideTab(inputId = "results_models", target = "Feature importance")
        
        
      })
      
      
      
      # Define input data chart and train/test periods splitting
      output$input_curve <- renderDygraph({
        
        
        curve_entries <- dygraph(data = eval(parse(text = paste0("data[,.(",date_column,",",y,")]"))),
                                 main = paste("Evolution of",y,"as a function of time")) %>%
          dyShading(from = input$train_selector[1],to = input$train_selector[2],color = "snow" ) %>%
          dyShading(from = input$test_selector[1],to = input$test_selector[2],color = "azure" ) %>%
          dyEvent(x = input$train_selector[1]) %>%
          dyEvent(x = input$train_selector[2]) %>%
          dyEvent(x = input$test_selector[2]) %>%
          dySeries(y,fillGraph = TRUE) %>%
          dyAxis("y",valueRange = c(0,1.5 * max(eval(parse(text =paste0("data$",y)))))) %>%
          dyOptions(colors = "darkblue",animatedZooms = TRUE)
        
        
        # chart can be displayed with bar or line mode
        if (input$bar_chart_mode == TRUE){
          curve_entries <- curve_entries %>% dyBarChart()
        }
        curve_entries
        
      })
      
      # Define input data chart and train/test periods splitting
      output$correlation_matrix <- renderPlotly({
        
        data_correlation <- as.matrix(select_if(data, is.numeric))
        plot_ly(x = colnames(data_correlation) , y = colnames(data_correlation), z =cor(data_correlation)  ,type = "heatmap", source = "heatplot")
      })
      
      # Define input data summary with class of each variable 
      output$variables_class_input <- renderDT({
        table_classes <- data.table()
        
        for (i in 1:ncol(data)){
          
          table_classes <- rbind(table_classes,
                                 data.frame(Variable = colnames(data)[i],
                                            Class = class(eval(parse(text = paste0("data$",colnames(data)[i]))))
                                 )
          )
        }
        
        datatable(table_classes,options = list(pageLength =3,searching = FALSE,lengthChange = FALSE),selection = list(mode = "single",selected = c(1))
        )
      })
      
      # Define boxplot corresponding to  selected variable in variables_class_input 
      output$variable_boxplot <- renderPlotly({
        
        column_name <- colnames(data)[input$variables_class_input_rows_selected]
        plot_ly(y = eval(parse(text = paste0("data[,",column_name,"]"))),
                type = "box",
                name = column_name
        )
        
      })
      
      
      # Define output chart comparing predicted vs real values on test period for selected model(s)
      output$output_curve <- renderDygraph({
        
        
        output_dygraph <- dygraph(data = table_forecast()[['results']],main = "Prediction results on test period") %>%
          dyAxis("y",valueRange = c(0,1.5 * max(eval(parse(text =paste0("table_forecast()[['results']]$",y)))))) %>%
          dyOptions(animatedZooms = TRUE)
        
        # chart can be displayed with bar or line mode
        if (input$bar_chart_mode == TRUE){
          output_dygraph <- output_dygraph %>% dyBarChart()
        }
        
        output_dygraph %>% dyLegend(width = 800)
        
        
        
      })
      
      
      # Define the table of predicted data
      # If "Run tuned models!" button is clicked, prediction results on test period are stored in four additional columns
      table_forecast <- reactive({
        
        
        data_results <- eval(parse(text = paste0("data[,.(",date_column,",",y,")][",date_column,">'",test_1$date,"',][",date_column,"< '",test_2$date,"',]")))
        table_results <- data_results
        dl_auto_ml <- NA
        var_input_list <- c()
        
        for (i in 1:length(model$train_variables)){
          var_input_list <- c(var_input_list,model$train_variables[i])
          
        }
        
        # Verify that at least one explanatory variable is selected
        if (length(var_input_list) != 0){
          
          
          data_train <- eval(parse(text = paste0("data[",date_column,"<='",test_1$date,"',][",date_column,">='",train_1$date,"',]")))
          data_test <- eval(parse(text = paste0("data[",date_column,">'",test_1$date,"',][",date_column,"< '",test_2$date,"',]")))
          
          data_h2o_train <- as.h2o(data_train)
          data_h2o_test <- as.h2o(data_test)
          
          
          # Calculation of glm predictions and associated calculation time
          if (!is.na(v_glm$type_model) & v_glm$type_model == "ml_generalized_linear_regression"){
            
            t1 <- Sys.time()
            dl_fit1 <- h2o.glm(x = as.character(var_input_list),
                               y = y,
                               training_frame = data_h2o_train,
                               family = parameter$family_glm,
                               intercept = input$intercept_term_glm,
                               lambda = parameter$reg_param_glm,
                               alpha = parameter$alpha_param_glm,
                               max_iterations = parameter$max_iter_glm,
                               seed = 1
            )
            t2 <- Sys.time()
            time_glm <- data.frame(`Training time` =  paste0(round(t2 - t1,1)," seconds"), Model = "Generalized linear regression")
            table_glm <- h2o.predict(dl_fit1,data_h2o_test) %>% as.data.table() %>% mutate(predict = round(predict,3)) %>% rename(`Generalized linear regression` = predict)
            table_results <- cbind(data_results,table_glm)%>% as.data.table()
            
          }
          
          # Calculation of random forest predictions and associated calculation time
          if (!is.na(v_random$type_model) & v_random$type_model == "ml_random_forest"){
            
            t1 <- Sys.time()
            dl_fit1 <- h2o.randomForest(x = as.character(var_input_list),
                                        y = y,
                                        training_frame = data_h2o_train,
                                        ntrees = parameter$num_tree_random_forest,
                                        sample_rate = parameter$subsampling_rate_random_forest,
                                        max_depth = parameter$max_depth_random_forest,
                                        nbins = parameter$n_bins_random_forest,
                                        seed = 1
            )
            t2 <- Sys.time()
            time_random_forest <- data.frame(`Training time` =  paste0(round(t2 - t1,1)," seconds"), Model = "Random forest")
            importance_random_forest <- h2o.varimp(dl_fit1) %>% as.data.table() %>% select(`variable`,scaled_importance) %>% mutate(model = "Random forest")
            table_random_forest<- h2o.predict(dl_fit1,data_h2o_test) %>% as.data.table() %>% mutate(predict = round(predict,3))  %>% rename(`Random forest` = predict)
            table_results <- cbind(data_results,table_random_forest)%>% as.data.table()
            
          }
          
          # Calculation of neural network predictions and associated calculation time
          if (!is.na(v_neural$type_model) & v_neural$type_model == "ml_neural_network"){
            
            t1 <- Sys.time()
            dl_fit1 <- h2o.deeplearning(x = as.character(var_input_list),
                                        y = y,
                                        training_frame = data_h2o_train,
                                        activation = parameter$activation_neural_net,
                                        loss = parameter$loss_neural_net,
                                        hidden = eval(parse(text = parameter$hidden_neural_net)) ,
                                        epochs = parameter$epochs_neural_net,
                                        rate = parameter$rate_neural_net,
                                        reproducible = T,
                                        seed = 1
            )
            t2 <- Sys.time()
            
            time_neural_network <- data.frame(`Training time` =  paste0(round(t2 - t1,1)," seconds"), Model = "Neural network")
            importance_neural_network <- h2o.varimp(dl_fit1) %>% as.data.table() %>% select(`variable`,scaled_importance) %>% mutate(model = "Neural network")
            table_neural_network <- h2o.predict(dl_fit1,data_h2o_test) %>% as.data.table() %>% mutate(predict = round(predict,3)) %>% rename(`Neural network` = predict)
            table_results <- cbind(data_results,table_neural_network)%>% as.data.table()
            
          }
          
          # Calculation of gradient boosted trees predictions and associated calculation time
          if (!is.na(v_grad$type_model) & v_grad$type_model == "ml_gradient_boosted_trees"){
            
            t1 <- Sys.time()
            dl_fit1 <- h2o.gbm(x = as.character(var_input_list),
                               y = y,
                               training_frame = data_h2o_train,
                               sample_rate = parameter$sample_rate_gbm,
                               ntrees = parameter$n_trees_gbm,
                               max_depth = parameter$max_depth_gbm,
                               learn_rate = parameter$learn_rate_gbm,
                               min_rows = 2,
                               seed = 1
            )
            t2 <- Sys.time()
            time_gbm <- data.frame(`Training time` =  paste0(round(t2 - t1,1)," seconds"), Model = "Gradient boosted trees")
            importance_gbm <- h2o.varimp(dl_fit1) %>% as.data.table() %>% select(`variable`,scaled_importance) %>% mutate(model = "Gradient boosted trees")
            table_gradient_boosting <- h2o.predict(dl_fit1,data_h2o_test) %>% as.data.table() %>% mutate(predict = round(predict,3)) %>% rename(`Gradient boosted trees` = predict)
            table_results <- cbind(data_results,table_gradient_boosting)%>% as.data.table()
            
          }
          
          
          
          # Calculation of autoML predictions (max calculation time has been set to 60 seconds)
          if (!is.na(v_auto_ml$type_model) & v_auto_ml$type_model == "ml_auto"){
            
            
            
            dl_auto_ml <- h2o.automl(x = as.character(var_input_list),
                                     y = y,
                                     training_frame = data_h2o_train,
                                     max_runtime_secs = parameter$run_time_auto_ml,
                                     seed = 1
                                     
            )
            
            
            
            time_auto_ml <- data.frame(`Training time` =  paste0(parameter$run_time_auto_ml," seconds"), Model = "Auto ML")
            table_auto_ml<- h2o.predict(dl_auto_ml,data_h2o_test) %>% as.data.table() %>% mutate(predict = round(predict,3))  %>% rename(`Auto ML` = predict)
            table_results <- cbind(data_results,table_auto_ml)%>% as.data.table()
            
          }
          
          # Assembly results of all models (some column might remain empty)
          if (!is.na(v_neural$type_model) & !is.na(v_grad$type_model) & !is.na(v_glm$type_model) & !is.na(v_random$type_model)){
            
            table_results <- cbind(data_results,table_glm,table_random_forest,table_neural_network,table_gradient_boosting)%>% as.data.table()
          }
          
        }
        
        table_training_time <- rbind(time_gbm,time_random_forest,time_glm,time_neural_network,time_auto_ml)
        table_importance <- rbind(importance_gbm,importance_random_forest,importance_neural_network) %>% as.data.table()
        
        # Used a list to access to different tables from only on one reactive objet
        list(traning_time = table_training_time, table_importance = table_importance, results = table_results,auto_ml_model = dl_auto_ml)
        
        
      })
      
      # Define performance table visible on "Compare models performances" tab
      output$score_table <- renderDT({
        
        
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
      
      # Define importance features table table visible on "Feature importance" tab
      output$feature_importance <- renderPlotly({
        
        validate(
          need(!is.na(v_neural$type_model)|!is.na(v_grad$type_model)|!is.na(v_glm$type_model)|!is.na(v_random$type_model)|!is.na(v_auto_ml$type_model),
               
               "Please run at least one model to see results"))
        
        
        if (nrow(table_forecast()[['table_importance']]) != 0){
          
          
          ggplotly(
            
            ggplot(data = table_forecast()[['table_importance']])+
              geom_bar(aes(x = reorder(`variable`,scaled_importance),y = scaled_importance,fill =  `model`),stat = "identity",width = 0.3)+
              facet_wrap( model ~ .)+
              coord_flip()+
              xlab("")+
              ylab("")+
              theme(legend.position="none")
          )
        }
        
      })
      
      
      
      # Define results table visible on "Table of results" tab
      output$table_of_results <- renderDT({
        
        datatable(
          table_forecast()[['results']],
          extensions = 'Buttons', options = list(dom = 'Bfrtip',buttons = c('csv', 'excel', 'pdf', 'print'))
        )
        
        
      },server = FALSE)
      
      
      
      # Synchronize train and test cursors
      observeEvent(input$train_selector,{
        updateSliderInput(session,'test_selector',
                          value= c(input$train_selector[2],input$test_selector[2]) )
      })
      
      observeEvent(input$test_selector,{
        updateSliderInput(session,'train_selector',
                          value= c(input$train_selector[1],input$test_selector[1]) )
      })
      
      
      # When "Run tuned models!" button is clicked, send messagebox once all models have been trained
      observe({
        
        if ("Generalized linear regression" %in% colnames(table_forecast()[['results']]) &
            "Random forest" %in% colnames(table_forecast()[['results']]) &
            "Neural network" %in% colnames(table_forecast()[['results']]) &
            "Gradient boosted trees" %in% colnames(table_forecast()[['results']])
        ){
          
          
          sendSweetAlert(
            session = session,
            title = "The four machine learning models have been trained !",
            text = "Click ok to see results",
            type = "success"
            
            
          )
        }
      })
      
      # When "Run auto ML" button is clicked, send messagebox once searching time is reached
      observe({
        
        
        if("Auto ML" %in% colnames(table_forecast()[['results']])){
          
          list <- c(HTML(paste0("<b>Selected model:</b> ",table_forecast()[['auto_ml_model']]@leader@algorithm)))
          
          for (i in 1:ncol(table_forecast()[['auto_ml_model']]@leader@model$model_summary)){
            list <- rbind(list,HTML(paste0("<b>",colnames(table_forecast()[['auto_ml_model']]@leader@model$model_summary[i]),":</b> ",
                                           table_forecast()[['auto_ml_model']]@leader@model$model_summary[i])))
          }
          
          # The message box indicates best model family and all associated hyper-parameter values
          sendSweetAlert(
            session = session,
            title = "Auto ML algorithm succeed!",
            text = HTML(paste0(
              "<br>",
              list)),
            type = "success",
            html = TRUE
          )
        }
      })
      
      # Define Value Box concerning memory used by h2o cluster  
      output$h2o_cluster_mem <- renderValueBox({
        
        valueBox(
          paste(round(as.numeric(cluster_status$free_mem)/1024**3,2), "GB", sep = ""),
          "H2O Cluster Total Memory", icon = icon("server"),
          color = "maroon"
        )
      })
      
      # Define Value Box concerning number of cpu used by h2o cluster
      output$h2o_cpu <- renderValueBox({
        
        valueBox(
          cluster_status$num_cpus,
          "Number of CPUs in Use", icon = icon("microchip"),
          color = "light-blue"
        )
      })
      
      
    }
  )
  
  # Allow to share the dashboard on local LAN
  if (share_app == TRUE){
    
    if(is.null(port)){stop("Please choose a port to share dashboard")}
    else if (nchar(port) != 4) {stop("Incorrect format of port")}
    else if (nchar(port) == 4){
      ip_adress <- gsub(".*? ([[:digit:]])", "\\1", system("ipconfig", intern=TRUE)[grep("IPv4", system("ipconfig", intern=TRUE))])[2]
      message("Forecast dashboard shared on LAN at ",ip_adress,":",port)
      runApp(app,host = "0.0.0.0",port = port,quiet = TRUE)
    }
  }
  
  else {runApp(app)}
  
  
}