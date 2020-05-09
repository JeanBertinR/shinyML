library(data.table)
library(shiny)
library(argonDash)
library(argonR)
library(plotly)
library(dygraphs)
library(h2o)
library(shinycssloaders)
library(shinyWidgets)
library(DT)
library(tidyr)
library(dplyr)
library(sparklyr)
#' @title Implement a shiny web app to compare h2o supervised regression models on time series
#'
#' @description This function creates in one line of code a shareable web app to compare supervised regression model performance (framework: H2O).
#'
#' @param data Time serie containing one or more input values and one output value. 
#'    The time serie must be a data.frame or a data.table and must contain at least one time-based column on Date or POSIXct format.
#' 
#' @param y the numerical output variable to forecast (must correpond to one data column)
#' 
#' @param framework the machine learning framework choosed to train and test models (either h2o or Spark). h2o by default.
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
#' shiny_h2o(data =longley2,y = "GNP",share_app = FALSE)
#'}
#' @import shiny argonDash argonR dygraphs data.table ggplot2 shinycssloaders
#' @importFrom dplyr %>% select mutate group_by summarise arrange rename select_if
#' @importFrom tidyr gather
#' @importFrom DT renderDT DTOutput datatable
#' @importFrom h2o h2o.init as.h2o h2o.deeplearning h2o.varimp h2o.predict h2o.gbm h2o.glm h2o.randomForest h2o.automl h2o.clusterStatus
#' @importFrom plotly plotlyOutput renderPlotly ggplotly plot_ly layout
#' @importFrom shinyWidgets materialSwitch switchInput sendSweetAlert knobInput awesomeCheckbox
#' @importFrom stats predict reorder cor
#' @export
#' 
#' 
#' 
library(shiny)
library(argonR)
library(argonDash)
library(magrittr)
library(lubridate)
library(h2o)
library(data.table)
library(dplyr)
library(tidyr)
library(dygraphs)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(sparklyr)
library(argonR)
library(argonDash)
longley2 <- longley %>% mutate(Year = as.Date(as.character(Year),format = "%Y"))


shinyML_regression <- function(data = data,y,framework = "h2o", share_app = FALSE,port = NULL){
  
  if(!(framework %in% c("h2o","spark"))){stop("framework must be selected between h2o or spark")}
  
  
  # Convert input data must be a data table object
  data <- data.table(data) 
  
  
  # Replace '.' by '_' in data colnames
  colnames(data) <- gsub("\\.","_",colnames(data))
  
  # Replace '.' by '_' in output variable
  y <- gsub("\\.","_",y)
  
  # Test if y is in data colnames
  if (!(y %in% colnames(data))){
    stop("y must match one data input variable")
  }
  
  # Test if y class correspond to numeric
  if (!(eval(parse(text = paste0("class(data$",y,")"))) == "numeric")){
    stop("y column class must be numeric")
  }
  
  # Assign x as data colnames excepted output variable name 
  x <- setdiff(colnames(data),y)
  
  # Test if input data does not exceed one million rows
  if (nrow(data) > 1000000) {
    stop("Input dataset must not exceed one million rows")
  }
  
  # Initialize all variables
  model <- reactiveValues()
  train_1 <- reactiveValues()
  
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
  
  
  ## ---------------------------------------------------------------------------- NAVBAR -----------------------------------
  
  
  
  argonNav <- argonDashNavbar(
    argonDropNav(
      title = HTML("shiny<font color='orange'>ML</font>"), 
      src = "https://www.zupimages.net/up/20/09/djw2.png", 
      orientation = "left"
    )
  )
  
  ## ---------------------------------------------------------------------------- FOOTER -----------------------------------
  
  
  argonFooter <- argonDashFooter(
    copyrights = "@Divad Nojnarg, 2018",
    src = "https://github.com/DivadNojnarg",
    argonFooterMenu(
      argonFooterItem("RinteRface", src = "https://github.com/RinteRface"),
      argonFooterItem("argon", src = "https://demos.creative-tim.com/argon-design-system/index.html")
    )
  )
  
  
  dashheader_explore_input <-  argonDashHeader(
    
    gradient = TRUE,
    color = "info",
    separator = FALSE,
    
    
    div(align = "center",
        argonH1(HTML("<font color='white'> Explore input data</font>"),display = 4)
    ),
    br(),
    argonRow(
      argonColumn(width = 9,
                  argonCard(width = 12,
                            src = NULL,
                            hover_lift = TRUE,
                            icon = argonIcon(name = "world-2", color = "info"),
                            shadow = TRUE,
                            argonTabSet(
                              width = 12,
                              id = "tab_input_data",
                              card_wrapper = TRUE,
                              horizontal = TRUE,
                              circle = FALSE,
                              size = "sm",
                              iconList = list(
                                argonIcon("cloud-upload-96"), 
                                argonIcon("bell-55"), 
                                argonIcon("calendar-grid-58"),
                                argonIcon("calendar-grid-58")
                              ),
                              
                              argonTab(
                                tabName = "Explore dataset",
                                active = TRUE,
                                div(align = "center", column(width = 6,uiOutput("X_axis_explore_dataset")),
                                    column(width = 6,selectInput(inputId = "y_variable_input_curve",label = "Y-axis variable",choices = colnames(data),selected = y))), 
                                #div(align = "center", column(width = 6,selectInput(inputId = "y_variable_input_curve",label = "Y-axis variable",choices = colnames(data),selected = y))),
                                
                                br(),
                                br(),
                                br(),
                                withSpinner(plotlyOutput("explore_dataset_chart",height = 250, width = 1100))
                                
                              ),
                              
                              
                              argonTab(
                                tabName = "Variables Summary",
                                active = FALSE,
                                fluidRow( 
                                  column(width = 6,
                                         withSpinner(DTOutput("variables_class_input", height = 180, width = 500))),
                                  column(width = 6,
                                         div(align = "center",
                                             radioButtons(inputId = "input_var_graph_type",label = "",choices = c("Boxplot","Histogram"),
                                                          selected = "Boxplot",inline = T)),
                                         withSpinner(plotlyOutput("variable_boxplot", height = 180, width = 500)))
                                )
                                
                              ),
                              
                              argonTab(
                                tabName = "Correlation matrix",
                                active = FALSE,
                                withSpinner(plotlyOutput("correlation_matrix", height = 180, width = 1100))
                                
                              )
                            )
                            
                            
                            
                            
                  )
                  
                  
      ),
      
      argonColumn(width = 3,
                  argonCard(width = 12,src = NULL,hover_lift = T,shadow = TRUE,
                            div(align = "center",
                                argonColumn(width = 6,uiOutput("Time_series_checkbox")),
                                argonColumn(width = 6,uiOutput("time_series_column")),
                                uiOutput("Variables_input_selection"),
                                uiOutput("slider_time_series_train"),
                                uiOutput("slider_time_series_test"),
                                uiOutput("slider_percentage")
                                
                                
                                
                            )
                            
                  )
                  
      )
    )
  )
  
  
  
  
  
  dashheader_explore_results <- argonDashHeader(gradient = TRUE,
                                                color = "primary",
                                                separator = FALSE,
                                  div(align = "center",
                                    argonH1(HTML("<font color='white'> Explore results </font>"),display = 4)
                                                   ),
                                    br(),
                                    argonRow(
                                    argonCard(width = 9,
                                        title = "Predictions on test period",
                                        src = NULL,
                                        hover_lift = TRUE,
                                        shadow = TRUE,
                                        icon = icon("cogs"),
                                                               status = "danger",
                                                               argonTabSet(
                                                                 width = 12,
                                                                 id = "results_models",
                                                                 card_wrapper = TRUE,
                                                                 horizontal = TRUE,
                                                                 circle = FALSE,
                                                                 size = "sm",
                                                                 iconList = list(
                                                                   argonIcon("cloud-upload-96"), 
                                                                   argonIcon("bell-55"), 
                                                                   argonIcon("calendar-grid-58"),
                                                                   argonIcon("calendar-grid-58")
                                                                 ),
                                                                 argonTab(
                                                                   tabName = "Result charts on test period",
                                                                   active = TRUE,
                                                                   withSpinner(dygraphOutput("output_curve", height = 200, width = 1100)),
                                                                   br(),
                                                                   div(align = "center",
                                                                       switchInput(label = "Bar chart mode",inputId = "bar_chart_mode",value = TRUE)
                                                                   )
                                                                 ),
                                                                 argonTab(
                                                                   tabName = "Compare models performances",
                                                                   active = FALSE,
                                                                   withSpinner(DTOutput("score_table"))
                                                                 ),
                                                                 argonTab(tabName = "Feature importance",
                                                                          active = FALSE,
                                                                          withSpinner(plotlyOutput("feature_importance"))
                                                                          
                                                                 ),
                                                                 argonTab(tabName = "Table of results",
                                                                          active = FALSE,
                                                                          withSpinner(DTOutput("table_of_results")))
                                                               ),
                                                               br(),
                                                               br(),
                                                               div(align = "center",
                                                                   actionButton("train_all","Run all models !",style = 'color:white; background-color:red; padding:4px; font-size:120%',
                                                                                icon = icon("cogs",lib = "font-awesome"))
                                                               )
                                                               
                                                               
                                                     ),
                                                     
                                                     
                                                     argonCard(
                                                       width = 3,
                                                       src = NULL,
                                                       hover_lift = T,
                                                       icon = icon("cogs"),
                                                       status = "warning",
                                                       shadow = TRUE,
                                                       #border_level = 2,
                                                       hover_shadow = TRUE,
                                                       title = "Auto Machine Learning",
                                                       div(align = "center",
                                                           knobInput(inputId = "run_time_auto_ml",label = "Max running time (in seconds)",value = 15,min = 10,max = 60,
                                                                     displayPrevious = TRUE, lineCap = "round",fgColor = "#428BCA",inputColor = "#428BCA"
                                                                     
                                                           ),
                                                           
                                                           br(),
                                                           actionButton("run_auto_ml","Run auto ML",style = 'color:white; background-color:red; padding:4px; font-size:120%',
                                                                        icon = icon("cogs",lib = "font-awesome"))
                                                       )
                                                       
                                                     )
                                                     
                                                   )
  )
  
  server = function(session,input, output) {
    
    if(framework == "h2o"){
      
    }
    
    
    else if(framework == "spark"){
    
    
    # Initialize all variables
    model <- reactiveValues()
    
    # # By default, start date and stop dates for test period correspond to mean and max of values of the date column
    
    test_1 <- reactiveValues() 
    test_2 <- reactiveValues()
    
    observe({
      req(!is.null(input$time_serie_select_column))
      test_1$date <-  eval(parse(text = paste0("mean(as.Date(data$",input$time_serie_select_column,"))")))
      test_2$date <-  eval(parse(text = paste0("max(as.Date(data$",input$time_serie_select_column,"))")))
      
    }) 
    
    
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
      parameter$glm_link <- input$glm_link
      parameter$intercept_term_glm <- input$intercept_term_glm
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
      
      showTab(inputId = "results_models", target = "Compare models performances")
      showTab(inputId = "results_models", target = "Feature importance")
      showTab(inputId = "results_models", target = "Table of results")        
      
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
      parameter$glm_link <- input$glm_link
      parameter$intercept_term_glm <- input$intercept_term_glm
      parameter$reg_param_glm <- input$reg_param_glm
      parameter$alpha_param_glm <- input$alpha_param_glm
      parameter$max_iter_glm <- input$max_iter_glm
      
      hideTab(inputId = "results_models", target = "Feature importance")
      showTab(inputId = "results_models", target = "Compare models performances")
      showTab(inputId = "results_models", target = "Table of results")
      
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
      
      showTab(inputId = "results_models", target = "Compare models performances")
      showTab(inputId = "results_models", target = "Feature importance")
      showTab(inputId = "results_models", target = "Table of results")
      
      
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
      
      showTab(inputId = "results_models", target = "Compare models performances")
      showTab(inputId = "results_models", target = "Feature importance")
      showTab(inputId = "results_models", target = "Table of results")
      
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
      
      showTab(inputId = "results_models", target = "Compare models performances")
      showTab(inputId = "results_models", target = "Feature importance")
      showTab(inputId = "results_models", target = "Table of results")
      
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
      showTab(inputId = "results_models", target = "Compare models performances")
      showTab(inputId = "results_models", target = "Table of results")
      
      
    })
    
    dates_variable_list <- reactive({
      dates_columns_list <- c()
      for (i in colnames(data)){
        if (is.Date(eval(parse(text = paste0("data$",i)))) | is.POSIXct(eval(parse(text = paste0("data$",i))))){
          dates_columns_list <- c(dates_columns_list,i)
        }
      }
      dates_columns_list
    })
    
    
    output$Time_series_checkbox <- renderUI({
      if (length(dates_variable_list()) >= 1){value = TRUE}
      else{value = FALSE}
      
      awesomeCheckbox("checkbox_time_series", "Time series",status = "primary",value = value)
      
    })
    
    
    
    output$slider_time_series_train <- renderUI({
      
      req(!is.null(input$checkbox_time_series))
      req(!is.null(input$time_serie_select_column))
      
      if (input$checkbox_time_series == TRUE){
        sliderInput("train_selector", "Choose train period:",
                    min = eval(parse(text = paste0("min(data$",input$time_serie_select_column,")"))),
                    max = eval(parse(text = paste0("max(data$",input$time_serie_select_column,")"))),
                    value =  eval(parse(text = paste0("c(min(data$",input$time_serie_select_column,"),mean(data$",input$time_serie_select_column,"))"))))
      }
    })
    
    
    output$slider_time_series_test <- renderUI({
      
      req(!is.null(input$checkbox_time_series))
      req(!is.null(input$time_serie_select_column))
      
      if (input$checkbox_time_series == TRUE){
        sliderInput("test_selector", "Choose test period:",
                    min = eval(parse(text = paste0("min(data$",input$time_serie_select_column,")"))),
                    max = eval(parse(text = paste0("max(data$",input$time_serie_select_column,")"))),
                    value = eval(parse(text = paste0("c(mean(data$",input$time_serie_select_column,"),max(data$",input$time_serie_select_column,"))"))))
      }
      
    })
    
    output$slider_percentage <- renderUI({
      
      req(!is.null(input$checkbox_time_series))
      
      if (input$checkbox_time_series == FALSE){
        
        selectInput(label = "Train/ Test splitting",inputId = "percentage_selector",choices = paste0(c(50:99),"%"),selected = 70,multiple = FALSE)
        #sliderInput(label = "Train/ Test splitting",inputId = "train_selector",min = 0, max = 100, post  = " %", value = 70)
        
      }
    })
    
    output$time_series_column <- renderUI({
      
      req(!is.null(input$checkbox_time_series))
      
      if (input$checkbox_time_series == TRUE){
        selectInput(inputId = "time_serie_select_column",label = "Date column",choices = dates_variable_list(),multiple = FALSE)
      }
      
    })
    
    output$Variables_input_selection<- renderUI({
      
      
      req(!is.null(input$checkbox_time_series))
      variable_input_list <- x[!(x %in% dates_variable_list())]
      
      selectInput( inputId  = "input_variables",label = "Input variables: ",choices = x,multiple = TRUE,selected = variable_input_list)
    })
    
    output$X_axis_explore_dataset <- renderUI({
      
      req(!is.null(input$checkbox_time_series))
      
      if (input$checkbox_time_series == TRUE){
        req(!is.null(input$time_serie_select_column))
        selected_column <- input$time_serie_select_column        
      }
      
      else {selected_column <- colnames(data)[1]}
      
      selectInput(inputId = "x_variable_input_curve",label = "X-axis variable",choices = colnames(data),selected = selected_column)
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
      
      if (input$input_var_graph_type == "Histogram"){chart_type <- "histogram"}
      else if (input$input_var_graph_type == "Boxplot"){chart_type <- "box"}
      
      plot_ly(x = eval(parse(text = paste0("data[,",column_name,"]"))),
              type = chart_type,
              name = column_name
      )
      
      
    })
    
    
    
    
    
    
    # Define plotly chart to explore dependencies between variables 
    output$explore_dataset_chart <- renderPlotly({
      
      req(!is.null(input$checkbox_time_series))
      
      plot_ly(data = data, x = eval(parse(text = paste0("data$",input$x_variable_input_curve))), 
              y = eval(parse(text = paste0("data$",input$y_variable_input_curve))),
              type = "scatter",mode = "markers") %>% 
        layout(xaxis = list(title = input$x_variable_input_curve),  yaxis = list(title = input$y_variable_input_curve))
    })
    
    # Define input data chart and train/test periods splitting
    output$correlation_matrix <- renderPlotly({
      
      data_correlation <- as.matrix(select_if(data, is.numeric))
      plot_ly(x = colnames(data_correlation) , y = colnames(data_correlation), z =cor(data_correlation)  ,type = "heatmap", source = "heatplot")
    })
    
    
    # Define the table of predicted data
    # If "Run all models!" button is clicked, prediction results on test period are stored in four additional columns
    table_forecast <- reactive({
      
      # Make sure a value is set to checkbox_time_series checkbox 
      req(!is.null(input$checkbox_time_series))
      if (input$checkbox_time_series == TRUE){
        req(!is.null(test_1$date))
        data_results <- eval(parse(text = paste0("data[,.(",dates_variable_list(),",",y,")][",dates_variable_list(),">'",test_1$date,"',][",dates_variable_list(),"< '",test_2$date,"',]")))
      }
      
      else if (input$checkbox_time_series == FALSE){
        
        
        req(!is.null(input$percentage_selector))
        
        data_train <- data %>% sample_frac(as.numeric(as.character(gsub("%","",input$percentage_selector)))*0.01)
        data_test <- data %>% anti_join(data_train)
        data_results <- data_test
        
        data_h2o_train <- as.h2o(data_train)
        data_h2o_test <- as.h2o(data_test)
      }
      
      
      table_results <- data_results
      dl_auto_ml <- NA
      var_input_list <- c()
      for (i in 1:length(model$train_variables)){
        var_input_list <- c(var_input_list,model$train_variables[i])
        
      }
      
      
      # Verify that at least one explanatory variable is selected
      if (length(var_input_list) != 0){
        
        if (input$checkbox_time_series == TRUE){
          data_h2o_train <- as.h2o(eval(parse(text = paste0("data[",dates_variable_list(),"<='",test_1$date,"',][",dates_variable_list(),">='",train_1$date,"',]"))))
          data_h2o_test <- as.h2o(eval(parse(text = paste0("data[",dates_variable_list(),">'",test_1$date,"',][",dates_variable_list(),"< '",test_2$date,"',]"))))
          
        }
        
        
        # Calculation of glm predictions and associated calculation time
        if (!is.na(v_glm$type_model) & v_glm$type_model == "ml_generalized_linear_regression"){
          
          t1 <- Sys.time()
          dl_fit1 <- h2o.glm(x = as.character(var_input_list),
                             y = y,
                             training_frame = data_h2o_train,
                             family = parameter$family_glm,
                             link = parameter$glm_link,
                             intercept = parameter$intercept_term_glm,
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
    
    # Define output chart comparing predicted vs real values on test period for selected model(s)
    output$output_curve <- renderDygraph({
      
      
      req(!is.null(input$checkbox_time_series))
      
      if (input$checkbox_time_series == TRUE){
        
        data_output_curve <- table_forecast()[['results']]
        
      }
      
      else if (input$checkbox_time_series == FALSE){
        
        
        data_output_curve <- table_forecast()[['results']] %>% 
          select(-c(setdiff(colnames(data),y))) %>% 
          mutate(Counter = row_number()) %>% 
          select(Counter,everything())
        
        
      }
      
      output_dygraph <- dygraph(data = data_output_curve ,main = "Prediction results on test period") %>%
        dyAxis("x",valueRange = c(0,nrow(data))) %>% 
        dyAxis("y",valueRange = c(0,1.5 * max(eval(parse(text =paste0("table_forecast()[['results']]$",y)))))) %>%
        dyOptions(animatedZooms = TRUE,fillGraph = T)
      
      
      
      # chart can be displayed with bar or line mode
      if (input$bar_chart_mode == TRUE){
        output_dygraph <- output_dygraph %>% dyBarChart()
      }
      
      output_dygraph %>% dyLegend(width = 800)
      
    })
    
    # Define performance table visible on "Compare models performances" tab
    output$score_table <- renderDT({
      
      req(!is.null(input$checkbox_time_series))
      
      if (input$checkbox_time_series == TRUE){
        
        req(!is.null(input$time_serie_select_column))
        
        performance_table <-  eval(parse(text = paste0("table_forecast()[['results']] %>% 
                                                         gather(key = Model,value = Predicted_value,-",input$time_serie_select_column,",-y) %>% 
                                                         as.data.table()")))
      }
      
      else if (input$checkbox_time_series == FALSE){
        
        performance_table <-  table_forecast()[['results']] %>%
          select(-c(setdiff(colnames(data),y))) %>% 
          gather(key = Model,value = Predicted_value,-y) %>%
          as.data.table()
      }
      
      performance_table <- performance_table %>% 
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
    
    
    # Send a warning if user clicks on "Time series" option and no date or Posixct date column exists on input data frame 
    observeEvent(input$checkbox_time_series,{
      
      if (input$checkbox_time_series == TRUE & length(dates_variable_list()) == 0){
        
        
        sendSweetAlert(
          session = session,
          title = "No Date or Posixct column has been detected on input data frame !",
          text = "Click ok to go back",
          type = "warning"
          
          
        )
      }
    })
    
    
    # Hide tabs of results_models tabItem when no model has been runed 
    observe({
      
      if (is.na(v_glm$type_model) & is.na(v_random$type_model) & is.na(v_neural$type_model) & is.na(v_grad$type_model) & is.na(v_auto_ml$type_model)){
        
        hideTab(inputId = "results_models", target = "Compare models performances")
        hideTab(inputId = "results_models", target = "Feature importance")
        hideTab(inputId = "results_models", target = "Table of results")
        
        
      }
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
    
  }
    
    
  }
  
  
  
  
  if(framework == "h2o"){
  
  # Run h2o instance (might require to unset proxy authentification credentials )
  Sys.setenv(http_proxy="")
  Sys.setenv(http_proxy_user="")
  Sys.setenv(https_proxy_user="")
  h2o.init()
  h2o::h2o.no_progress()
  cluster_status <- h2o.clusterStatus()
  
  ## ---------------------------------------------------------------------------- HEADER -----------------------------------
  
  argonHeader <- argonColumn(width = "100%",
    
    dashheader_explore_input ,
      
      
    argonDashHeader(gradient = TRUE,
                    color = "default",
                    separator = FALSE,
      
      div(align = "center",
          argonH1(HTML("<font color='white'> Select models parameters</font>"),display = 4)
      ),
  
    br(),
    argonRow(
      argonColumn(width = 6,div(align = "center",uiOutput("h2o_cluster_mem"))),
      argonColumn(width = 6,div(align = "center",uiOutput("h2o_cpu")))
    ),


    argonRow(
      argonCard(
        width = 3,
        src = NULL,
        hover_lift = T,
        icon = icon("cogs"),
        status = "success",
        shadow = TRUE,
        hover_shadow = TRUE,
        title = "Generalized linear regression",
        div(align = "center",
          argonRow(
            argonColumn(
                radioButtons(label = "Family",inputId = "glm_family",choices = c("gaussian","poisson", "gamma","tweedie"),
                                   selected = "gaussian"),width = 6),
                    
                    argonColumn(
                      radioButtons(label = "Link",inputId = "glm_link",choices = c("identity","log"),selected = "identity"),
                      switchInput(label = "Intercept term",inputId = "intercept_term_glm",value = TRUE,width = "auto"),width = 6)
                  ),
                  
                  sliderInput(label = "Lambda",inputId = "reg_param_glm",min = 0,max = 10,value = 0),
                  sliderInput(label = "Alpha (0:Ridge <-> 1:Lasso)",inputId = "alpha_param_glm",min = 0,max = 1,value = 0.5),
                  sliderInput(label = "Maximum iteraions",inputId = "max_iter_glm",min = 50,max = 300,value = 100),
                  actionButton("run_glm","Run glm",style = 'color:white; background-color:green; padding:4px; font-size:120%',
                               icon = icon("cogs",lib = "font-awesome"))
              )
              
              
              
            ),
            
            argonCard(
              width = 3,
              src = NULL,
              hover_lift = T,
              icon = icon("cogs"),
              status = "danger",
              shadow = TRUE,
              #border_level = 2,
              hover_shadow = TRUE,
              title = "Random Forest",
              div(align = "center",
                  sliderInput(label = "Number of trees",min = 1,max = 100, inputId = "num_tree_random_forest",value = 50),
                  sliderInput(label = "Subsampling rate",min = 0.1,max = 1, inputId = "subsampling_rate_random_forest",value = 0.6),
                  sliderInput(label = "Max depth",min = 1,max = 50, inputId = "max_depth_random_forest",value = 20),
                  sliderInput(label = "Number of bins",min = 2,max = 100, inputId = "n_bins_random_forest",value = 20),
                  actionButton("run_random_forest","Run random forest",style = 'color:white; background-color:red; padding:4px; font-size:120%',
                               icon = icon("cogs",lib = "font-awesome"))
              )
              
            ),
            
            
            argonCard(
              width = 3,
              src = NULL,
              hover_lift = T,
              icon = icon("cogs"),
              status = "primary",
              shadow = TRUE,
              #border_level = 2,
              hover_shadow = TRUE,
              title = "Neural network",
              div(align = "center",
                  argonRow(
                    argonColumn(
                      radioButtons(label = "Activation function",inputId = "activation_neural_net",
                                   choices = c( "Rectifier", "Maxout","Tanh", "RectifierWithDropout", "MaxoutWithDropout","TanhWithDropout"),selected = "Rectifier"),width = 6),
                    
                    argonColumn(
                      radioButtons(label = "Loss function",inputId = "loss_neural_net",
                                   choices = c("Automatic", "Quadratic", "Huber", "Absolute", "Quantile"),selected = "Automatic"),width = 6)
                  ),
                  
                  textInput(label = "Hidden layers",inputId = "hidden_neural_net",value = "c(200,200)"),
                  sliderInput(label = "Epochs",min = 10,max = 100, inputId = "epochs_neural_net",value = 10),
                  sliderInput(label = "Learning rate",min = 0.001,max = 0.1, inputId = "rate_neural_net",value = 0.005),
                  actionButton("run_neural_network","Run neural network",style = 'color:white; background-color:darkblue; padding:4px; font-size:120%',
                               icon = icon("cogs",lib = "font-awesome"))
              )
              
            ),
            
            argonCard(
              width = 3,
              src = NULL,
              hover_lift = T,
              icon = icon("cogs"),
              status = "warning",
              shadow = TRUE,
              #border_level = 2,
              hover_shadow = TRUE,
              title = "Gradient boosting",
              div(align = "center",
                  sliderInput(label = "Max depth",min = 1,max = 20, inputId = "max_depth_gbm",value = 5),
                  sliderInput(label = "Number of trees",min = 1,max = 100, inputId = "n_trees_gbm",value = 50),
                  sliderInput(label = "Sample rate",min = 0.1,max = 1, inputId = "sample_rate_gbm",value = 1),
                  sliderInput(label = "Learn rate",min = 0.1,max = 1, inputId = "learn_rate_gbm",value = 0.1),
                  actionButton("run_gradient_boosting","Run gradient boosting",style = 'color:white; background-color:orange; padding:4px; font-size:120%',
                               icon = icon("cogs",lib = "font-awesome"))
              )
              
            )
            
          )
    ),
    
    
  dashheader_explore_results 
      
    

  )
  
  
  

  
  
  
  ## ---------------------------------------------------------------------------- LANCEMENT APPLI -----------------------------------
  
  
  # App
  app <- shiny::shinyApp(
    ui = argonDashPage(
      title = "shinyML_regression",
      author = "Jean",
      description = "Use of shinyML_regression function",
      #sidebar = argonSidebar,
      navbar = argonNav,
      header = argonHeader,
      footer = argonFooter
    ),
    server = function(session,input, output) {
      
      
      # Initialize all variables
      model <- reactiveValues()
      
      # # By default, start date and stop dates for test period correspond to mean and max of values of the date column

      test_1 <- reactiveValues() 
      test_2 <- reactiveValues()
      
      observe({
        req(!is.null(input$time_serie_select_column))
        test_1$date <-  eval(parse(text = paste0("mean(as.Date(data$",input$time_serie_select_column,"))")))
        test_2$date <-  eval(parse(text = paste0("max(as.Date(data$",input$time_serie_select_column,"))")))
        
        }) 
      
    
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
        parameter$glm_link <- input$glm_link
        parameter$intercept_term_glm <- input$intercept_term_glm
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
        
        showTab(inputId = "results_models", target = "Compare models performances")
        showTab(inputId = "results_models", target = "Feature importance")
        showTab(inputId = "results_models", target = "Table of results")        
        
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
        parameter$glm_link <- input$glm_link
        parameter$intercept_term_glm <- input$intercept_term_glm
        parameter$reg_param_glm <- input$reg_param_glm
        parameter$alpha_param_glm <- input$alpha_param_glm
        parameter$max_iter_glm <- input$max_iter_glm
        
        hideTab(inputId = "results_models", target = "Feature importance")
        showTab(inputId = "results_models", target = "Compare models performances")
        showTab(inputId = "results_models", target = "Table of results")
        
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
        
        showTab(inputId = "results_models", target = "Compare models performances")
        showTab(inputId = "results_models", target = "Feature importance")
        showTab(inputId = "results_models", target = "Table of results")
        
        
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
        
        showTab(inputId = "results_models", target = "Compare models performances")
        showTab(inputId = "results_models", target = "Feature importance")
        showTab(inputId = "results_models", target = "Table of results")
        
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
        
        showTab(inputId = "results_models", target = "Compare models performances")
        showTab(inputId = "results_models", target = "Feature importance")
        showTab(inputId = "results_models", target = "Table of results")
        
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
        showTab(inputId = "results_models", target = "Compare models performances")
        showTab(inputId = "results_models", target = "Table of results")
        
        
      })
      
      dates_variable_list <- reactive({
        dates_columns_list <- c()
        for (i in colnames(data)){
          if (is.Date(eval(parse(text = paste0("data$",i)))) | is.POSIXct(eval(parse(text = paste0("data$",i))))){
            dates_columns_list <- c(dates_columns_list,i)
          }
        }
        dates_columns_list
      })
      
      
      output$Time_series_checkbox <- renderUI({
        if (length(dates_variable_list()) >= 1){value = TRUE}
        else{value = FALSE}
        
        awesomeCheckbox("checkbox_time_series", "Time series",status = "primary",value = value)
        
      })
      
      
      
      output$slider_time_series_train <- renderUI({
        
        req(!is.null(input$checkbox_time_series))
        req(!is.null(input$time_serie_select_column))
        
        if (input$checkbox_time_series == TRUE){
          sliderInput("train_selector", "Choose train period:",
                      min = eval(parse(text = paste0("min(data$",input$time_serie_select_column,")"))),
                      max = eval(parse(text = paste0("max(data$",input$time_serie_select_column,")"))),
                      value =  eval(parse(text = paste0("c(min(data$",input$time_serie_select_column,"),mean(data$",input$time_serie_select_column,"))"))))
        }
      })
      
      
      output$slider_time_series_test <- renderUI({
        
        req(!is.null(input$checkbox_time_series))
        req(!is.null(input$time_serie_select_column))
        
        if (input$checkbox_time_series == TRUE){
          sliderInput("test_selector", "Choose test period:",
                    min = eval(parse(text = paste0("min(data$",input$time_serie_select_column,")"))),
                    max = eval(parse(text = paste0("max(data$",input$time_serie_select_column,")"))),
                    value = eval(parse(text = paste0("c(mean(data$",input$time_serie_select_column,"),max(data$",input$time_serie_select_column,"))"))))
        }
        
      })
          
      output$slider_percentage <- renderUI({
        
        req(!is.null(input$checkbox_time_series))
        
        if (input$checkbox_time_series == FALSE){
          
          selectInput(label = "Train/ Test splitting",inputId = "percentage_selector",choices = paste0(c(50:99),"%"),selected = 70,multiple = FALSE)
          #sliderInput(label = "Train/ Test splitting",inputId = "train_selector",min = 0, max = 100, post  = " %", value = 70)
          
        }
      })
      
      output$time_series_column <- renderUI({
        
        req(!is.null(input$checkbox_time_series))

        if (input$checkbox_time_series == TRUE){
          selectInput(inputId = "time_serie_select_column",label = "Date column",choices = dates_variable_list(),multiple = FALSE)
        }
        
      })
      
      output$Variables_input_selection<- renderUI({
        
        
        req(!is.null(input$checkbox_time_series))
        variable_input_list <- x[!(x %in% dates_variable_list())]
        
        selectInput( inputId  = "input_variables",label = "Input variables: ",choices = x,multiple = TRUE,selected = variable_input_list)
      })

      output$X_axis_explore_dataset <- renderUI({
        
        req(!is.null(input$checkbox_time_series))
        
        if (input$checkbox_time_series == TRUE){
          req(!is.null(input$time_serie_select_column))
          selected_column <- input$time_serie_select_column        
        }
        
        else {selected_column <- colnames(data)[1]}
        
        selectInput(inputId = "x_variable_input_curve",label = "X-axis variable",choices = colnames(data),selected = selected_column)
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
        
        if (input$input_var_graph_type == "Histogram"){chart_type <- "histogram"}
        else if (input$input_var_graph_type == "Boxplot"){chart_type <- "box"}
        
        plot_ly(x = eval(parse(text = paste0("data[,",column_name,"]"))),
                type = chart_type,
                name = column_name
        )
        
        
      })
      
      

      
      
      
      # Define plotly chart to explore dependencies between variables 
      output$explore_dataset_chart <- renderPlotly({
        
        req(!is.null(input$checkbox_time_series))
        
        plot_ly(data = data, x = eval(parse(text = paste0("data$",input$x_variable_input_curve))), 
                y = eval(parse(text = paste0("data$",input$y_variable_input_curve))),
                type = "scatter",mode = "markers") %>% 
          layout(xaxis = list(title = input$x_variable_input_curve),  yaxis = list(title = input$y_variable_input_curve))
      })
      
      # Define input data chart and train/test periods splitting
      output$correlation_matrix <- renderPlotly({
        
        data_correlation <- as.matrix(select_if(data, is.numeric))
        plot_ly(x = colnames(data_correlation) , y = colnames(data_correlation), z =cor(data_correlation)  ,type = "heatmap", source = "heatplot")
      })
      
      
      # Define the table of predicted data
      # If "Run all models!" button is clicked, prediction results on test period are stored in four additional columns
      table_forecast <- reactive({
        
        # Make sure a value is set to checkbox_time_series checkbox 
        req(!is.null(input$checkbox_time_series))
        if (input$checkbox_time_series == TRUE){
          req(!is.null(test_1$date))
          data_results <- eval(parse(text = paste0("data[,.(",dates_variable_list(),",",y,")][",dates_variable_list(),">'",test_1$date,"',][",dates_variable_list(),"< '",test_2$date,"',]")))
        }
     
        else if (input$checkbox_time_series == FALSE){
          
          
          req(!is.null(input$percentage_selector))

          data_train <- data %>% sample_frac(as.numeric(as.character(gsub("%","",input$percentage_selector)))*0.01)
          data_test <- data %>% anti_join(data_train)
          data_results <- data_test
          
          data_h2o_train <- as.h2o(data_train)
          data_h2o_test <- as.h2o(data_test)
        }
        
        
        table_results <- data_results
        dl_auto_ml <- NA
        var_input_list <- c()
        for (i in 1:length(model$train_variables)){
          var_input_list <- c(var_input_list,model$train_variables[i])
          
        }
        
        
        # Verify that at least one explanatory variable is selected
        if (length(var_input_list) != 0){
          
          if (input$checkbox_time_series == TRUE){
            data_h2o_train <- as.h2o(eval(parse(text = paste0("data[",dates_variable_list(),"<='",test_1$date,"',][",dates_variable_list(),">='",train_1$date,"',]"))))
            data_h2o_test <- as.h2o(eval(parse(text = paste0("data[",dates_variable_list(),">'",test_1$date,"',][",dates_variable_list(),"< '",test_2$date,"',]"))))
          
          }
          
          
          # Calculation of glm predictions and associated calculation time
          if (!is.na(v_glm$type_model) & v_glm$type_model == "ml_generalized_linear_regression"){
            
            t1 <- Sys.time()
            dl_fit1 <- h2o.glm(x = as.character(var_input_list),
                               y = y,
                               training_frame = data_h2o_train,
                               family = parameter$family_glm,
                               link = parameter$glm_link,
                               intercept = parameter$intercept_term_glm,
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
      
      # Define output chart comparing predicted vs real values on test period for selected model(s)
      output$output_curve <- renderDygraph({
        
        
        req(!is.null(input$checkbox_time_series))
        
        if (input$checkbox_time_series == TRUE){
          
          data_output_curve <- table_forecast()[['results']]
          
        }
        
        else if (input$checkbox_time_series == FALSE){
          

          data_output_curve <- table_forecast()[['results']] %>% 
            select(-c(setdiff(colnames(data),y))) %>% 
            mutate(Counter = row_number()) %>% 
            select(Counter,everything())
          
          
        }
          
        output_dygraph <- dygraph(data = data_output_curve ,main = "Prediction results on test period") %>%
          dyAxis("x",valueRange = c(0,nrow(data))) %>% 
          dyAxis("y",valueRange = c(0,1.5 * max(eval(parse(text =paste0("table_forecast()[['results']]$",y)))))) %>%
          dyOptions(animatedZooms = TRUE,fillGraph = T)
        
        
        
        # chart can be displayed with bar or line mode
        if (input$bar_chart_mode == TRUE){
          output_dygraph <- output_dygraph %>% dyBarChart()
        }
        
        output_dygraph %>% dyLegend(width = 800)
        
      })
      
      # Define performance table visible on "Compare models performances" tab
      output$score_table <- renderDT({
        
        req(!is.null(input$checkbox_time_series))
        
        if (input$checkbox_time_series == TRUE){
          
          req(!is.null(input$time_serie_select_column))
          
          performance_table <-  eval(parse(text = paste0("table_forecast()[['results']] %>% 
                                                         gather(key = Model,value = Predicted_value,-",input$time_serie_select_column,",-y) %>% 
                                                         as.data.table()")))
        }

        else if (input$checkbox_time_series == FALSE){
          
          performance_table <-  table_forecast()[['results']] %>%
            select(-c(setdiff(colnames(data),y))) %>% 
            gather(key = Model,value = Predicted_value,-y) %>%
            as.data.table()
        }
          
          performance_table <- performance_table %>% 
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
      
      
      # Send a warning if user clicks on "Time series" option and no date or Posixct date column exists on input data frame 
      observeEvent(input$checkbox_time_series,{
        
        if (input$checkbox_time_series == TRUE & length(dates_variable_list()) == 0){
          
          
          sendSweetAlert(
            session = session,
            title = "No Date or Posixct column has been detected on input data frame !",
            text = "Click ok to go back",
            type = "warning"
            
            
          )
        }
      })
      
      
      # Hide tabs of results_models tabItem when no model has been runed 
      observe({
        
        if (is.na(v_glm$type_model) & is.na(v_random$type_model) & is.na(v_neural$type_model) & is.na(v_grad$type_model) & is.na(v_auto_ml$type_model)){
          
          hideTab(inputId = "results_models", target = "Compare models performances")
          hideTab(inputId = "results_models", target = "Feature importance")
          hideTab(inputId = "results_models", target = "Table of results")
          
          
        }
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
      output$h2o_cluster_mem <- renderUI({
        
        argonInfoCard(
          value = paste(round(as.numeric(cluster_status$free_mem)/1024**3,2), "GB", sep = ""),
          title = "H2O Cluster Total Memory",gradient = TRUE,width = 7,
          # stat = -1.10, 
          # stat_icon = icon("arrow-down"),
          # description = "Since yesterday", 
          icon = icon("server"), 
          icon_background = "yellow",
          background_color = "lightblue"
        )
        
      })
      
      # Define Value Box concerning number of cpu used by h2o cluster
      output$h2o_cpu <- renderUI({
        
        argonInfoCard(
          value = cluster_status$num_cpus,gradient = TRUE,width = 7,
          title = "Number of CPUs in Use",
          icon = icon("microchip"), 
          icon_background = "yellow",
          background_color = "lightblue"
        )
        
      })
      
      
    }
  )
  
  }
  
  else if(framework == "spark"){
    
    
    argonHeader <- argonColumn(width = "100%",
                               
                               dashheader_explore_input ,
                               
                               
                               argonDashHeader(gradient = TRUE,
                                               color = "default",
                                               separator = FALSE,
                                               
                                               div(align = "center",
                                                   argonH1(HTML("<font color='white'> Select models parameters</font>"),display = 4)
                                               ),
                                               
                                               argonRow(
                                                 argonCard(
                                                   width = 3,
                                                   src = NULL,
                                                   hover_lift = T,
                                                   icon = icon("cogs"),
                                                   status = "success",
                                                   shadow = TRUE,
                                                   #border_level = 2,
                                                   hover_shadow = TRUE,
                                                   title = "Generalized linear regression",
                                                   div(align = "center",
                                                       argonRow(
                                                         argonColumn(
                                                           radioButtons(label = "Family",inputId = "glm_family",choices = c("gaussian","Gamma","poisson"),
                                                                        selected = "gaussian"),width = 6),
                                                         
                                                         argonColumn(
                                                           radioButtons(label = "Link",inputId = "glm_link",choices = c("identity","log"),selected = "identity"),
                                                           switchInput(label = "Intercept term",inputId = "intercept_term_glm",value = TRUE,width = "auto"),width = 6)
                                                       ),
                                                       
                                                       sliderInput(label = "Lambda",inputId = "reg_param_glm",min = 0,max = 10,value = 0),
                                                       sliderInput(label = "Alpha (0:Ridge <-> 1:Lasso)",inputId = "alpha_param_glm",min = 0,max = 1,value = 0.5),
                                                       sliderInput(label = "Maximum iteraions",inputId = "max_iter_glm",min = 50,max = 300,value = 100),
                                                       actionButton("run_glm","Run glm",style = 'color:white; background-color:green; padding:4px; font-size:120%',
                                                                    icon = icon("cogs",lib = "font-awesome"))
                                                   )
                                                   
                                                   
                                                   
                                                 ),
                                                 
                                                 argonCard(
                                                   width = 3,
                                                   src = NULL,
                                                   hover_lift = T,
                                                   icon = icon("cogs"),
                                                   status = "danger",
                                                   shadow = TRUE,
                                                   #border_level = 2,
                                                   hover_shadow = TRUE,
                                                   title = "Random Forest",
                                                   div(align = "center",
                                                       sliderInput(label = "Number of trees",min = 1,max = 100, inputId = "num_tree_random_forest",value = 50),
                                                       sliderInput(label = "Subsampling rate",min = 0.1,max = 1, inputId = "subsampling_rate_random_forest",value = 1),
                                                       sliderInput(label = "Max depth",min = 1,max = 50, inputId = "max_depth_random_forest",value = 20),
                                                       sliderInput(label = "Number of bins",min = 2,max = 100, inputId = "n_bins_random_forest",value = 20),
                                                       actionButton("run_random_forest","Run random forest",style = 'color:white; background-color:red; padding:4px; font-size:120%',
                                                                    icon = icon("cogs",lib = "font-awesome"))
                                                   )
                                                   
                                                 ),
                                                 
                                                 
                                                 argonCard(
                                                   width = 3,
                                                   src = NULL,
                                                   hover_lift = T,
                                                   icon = icon("cogs"),
                                                   status = "primary",
                                                   shadow = TRUE,
                                                   #border_level = 2,
                                                   hover_shadow = TRUE,
                                                   title = "Decision tree",
                                                   div(align = "center",
                                                       argonRow(
                                                         argonColumn(
                                                           sliderInput(label = "Max depth",inputId = "max_depth_decision_tree",min = 1,max = 30,value = 20),
                                                           sliderInput(label = "Max bins",inputId = "max_bins_decision_tree",min = 2,max = 60,value = 32),
                                                           sliderInput(label = "Min instance per node",inputId = "min_instance_decision_tree",min = 1,max = 10,value = 1),
                                                           actionButton("run_decision_tree","Run decision tree regression",style = 'color:white; background-color:darkblue; padding:4px; font-size:120%',
                                                                        icon = icon("cogs",lib = "font-awesome"))
                                                         )
                                                         
                                                       )
                                                   )
                                                 ),
                                                 
                                                 argonCard(
                                                   width = 3,
                                                   src = NULL,
                                                   hover_lift = T,
                                                   icon = icon("cogs"),
                                                   status = "warning",
                                                   shadow = TRUE,
                                                   hover_shadow = TRUE,
                                                   title = "Gradient boosting",
                                                   div(align = "center",
                                                       sliderInput(label = "Step size",min = 0,max = 1, inputId = "step_size_gbm",value = 0.1),
                                                       sliderInput(label = "Subsampling rate",min = 0.1,max = 1, inputId = "subsampling_rate_gbm",value = 1),
                                                       sliderInput(label = "Max depth",min = 1,max = 30, inputId = "max_depth_gbm",value = 20),
                                                       actionButton("run_gradient_boosting","Run gradient boosting",style = 'color:white; background-color:orange; padding:4px; font-size:120%',
                                                                    icon = icon("cogs",lib = "font-awesome"))
                                                   )
                                                   
                                                 )
                                                 
                                               )
                               ),
                               
                               
                               dashheader_explore_results 
 
                       
    )
    
    ## ---------------------------------------------------------------------------- LANCEMENT APPLI -----------------------------------
    
    
    # App
    app <- shiny::shinyApp(
      ui = argonDashPage(
        title = "shinyML_regression",
        author = "Jean",
        description = "Use of shinyML_regression function",
        navbar = argonNav,
        header = argonHeader,
        footer = argonFooter
      ),
      server = server 
    )
 
  }
  
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



shinyML_regression(data = longley2,y = "Population",share_app = F,framework = "h2o")



