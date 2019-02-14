#' @title Create a shiny app to implement and compare supervised regression models on time series (framework used: Spark)
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
#' longley2 <- longley %>% mutate(Year = as.Date(as.character(Year),format = "%Y"))
#' dashforecast(data =longley2,x = c("GNP_deflator","Unemployed" ,"Armed_Forces", "Population","Employed"),y = "GNP",date_column = "Year",share_app = TRUE,port = 5845)
#' @export

dashforecast <- function(data = data,x,y,date_column, share_app = FALSE,port = NULL ){
  
  
  data <- data.table(data)
  if (nrow(spark_installed_versions()) == 0){spark_install()}
  sc <- spark_connect(master = "local")
  
  app <- shinyApp(
    
    ui = dashboardPage(
      dashboardHeader(title = "Compare forecast models"),
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
                          column(width = 12,box(
                            dygraphOutput("input_curve", height = 120, width = 930),width = 12
                          )
                          ),
                          column(width = 12,tabBox(id = "results_models",
                                                   tabPanel("Result charts on test period",dygraphOutput("output_curve",height = 200,width = 930)),
                                                   tabPanel("Compare models performances",dataTableOutput("date_essai",width = 700)),
                                                   tabPanel("Feature importance",plotlyOutput("feature_importance")),
                                                   tabPanel("Table of results",dataTableOutput("table_of_results")),width = 12
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
                       radioButtons(label = "Family",inputId = "glm_family",choices = c("gaussian","Gamma","poisson"),selected = "gaussian"),width = 6),
                     column(
                       radioButtons(label = "Link",inputId = "glm_link",choices = c("identity","log"),selected = "identity"),width = 6),
                     
                     materialSwitch(label = "Intercept term",inputId = "intercept_term_glm",status = "primary",value = TRUE),
                     sliderInput(label = "Regularization parameter (lambda)",inputId = "reg_param_glm",min = 0,max = 10,value = 0),
                     sliderInput(label = "Maximum iteraions",inputId = "max_iter_glm",min = 50,max = 300,value = 100),
                     actionButton("run_glm","Run generalized linear regression",style = 'color:white; background-color:orange; padding:4px; font-size:150%',
                                  icon = icon("cogs",lib = "font-awesome"))
                     ,width = 3 ),
                   
                   box(
                     title = "Decision tree",status = "danger",
                     
                     sliderInput(label = "Max depth",inputId = "max_depth_decision_tree",min = 0,max = 20,value = 5),
                     sliderInput(label = "Max bins",inputId = "max_bins_decision_tree",min = 2,max = 60,value = 32),
                     sliderInput(label = "Min instance per node",inputId = "min_instance_decision_tree",min = 1,max = 10,value = 1),
                     actionButton("run_decision_tree","Run decision tree regression",style = 'color:white; background-color:red; padding:4px; font-size:150%',
                                  icon = icon("cogs",lib = "font-awesome"))
                     
                     ,width = 3),
                   
                   box(
                     title = "Random Forest",status = "primary",
                     
                     sliderInput(label = "Number of trees",min = 1,max = 100, inputId = "num_tree_random_forest",value = 20),
                     sliderInput(label = "Subsampling rate",min = 0.1,max = 1, inputId = "subsampling_rate_random_forest",value = 1),
                     sliderInput(label = "Max depth",min = 0,max = 20, inputId = "max_depth_random_forest",value = 5),
                     actionButton("run_random_forest","Run random forest model",style = 'color:white; background-color:darkblue; padding:4px; font-size:150%',
                                  icon = icon("users",lib = "font-awesome"))
                     
                     ,width = 3),
                   
                   
                   box(
                     title = "Gradient boosting trees",status = "success",
                     
                     
                     sliderInput(label = "Step size",min = 0,max = 1, inputId = "step_size_gbm",value = 0.1),
                     sliderInput(label = "Subsampling rate",min = 0.1,max = 1, inputId = "subsampling_rate_gbm",value = 1),
                     
                     
                     actionButton("run_gradient_boosting","Run gradient boosting model",style = 'color:white; background-color:darkgreen; padding:4px; font-size:150%',
                                  icon = icon("users",lib = "font-awesome"))
                     
                     ,width = 3)
                   
                 )
                 
          )
        )
      )
    ),
    
    server = function(session, input, output) {
      set.seed(122)
      table_ml_gradient_boosted <- data.table(`Gradient boosted trees` = NA)
      table_ml_random_forest <- data.table(`Random forest` = NA)
      table_ml_glm <- data.table(`Generalized linear regression` = NA)
      table_ml_decision_tree <- data.table(`Decision tree` = NA)
      
      
      
      time_gbm <- data.table()
      time_random_forest <- data.table()
      time_glm <- data.table()
      time_decision_tree <- data.table()
      
      importance_gbm <- data.table()
      importance_random_forest <- data.table()
      importance_decision_tree <- data.table()
      
      
      model <- reactiveValues(train_variables = NA)
      
      
      t <- reactiveValues(step_size_gbm = 0.1)
      v <- reactiveValues(subsampling_rate_gbm = 1)
      
      f <- reactiveValues(family_glm = "gaussian")
      l <- reactiveValues(link_glm = "identity")
      i <- reactiveValues(intercept_glm = TRUE) 
      
      
      v_grad <- reactiveValues(type_model = NA)
      v_random <- reactiveValues(type_model = NA)
      v_glm <- reactiveValues(type_model = NA)
      v_decision_tree <- reactiveValues(type_model = NA)
      
      x <- reactiveValues(max_depth_random_forest = 5)
      
      
      observeEvent(input$run_gradient_boosting,{
        
        test_1$date <- input$test_selector[1]
        model$train_variables <- input$input_variables
        t$step_size_gbm <- input$step_size_gbm
        v$subsampling_rate_gbm <- input$subsampling_rate_gbm
        v_grad$type_model <- "ml_gradient_boosted_trees"
        v_random$type_model <- NA
        v_glm$type_model <- NA
        v_decision_tree$type_model <- NA
        
      })
      
      
      observeEvent(input$run_random_forest,{
        
        test_1$date <- input$test_selector[1]
        model$train_variables <- input$input_variables
        t$num_tree_random_forest <- input$num_tree_random_forest
        v$subsampling_rate_random_forest <- input$subsampling_rate_random_forest
        x$max_depth_random_forest <-  input$max_depth_random_forest
        v_random$type_model <- "ml_random_forest"
        v_grad$type_model <- NA
        v_glm$type_model <- NA
        v_decision_tree$type_model <- NA
        
      })
      
      observeEvent(input$run_glm,{
        
        test_1$date <- input$test_selector[1]
        model$train_variables <- input$input_variables
        f$family_glm <- input$glm_family
        l$link_glm <- input$glm_link
        i$intercept_glm <- input$intercept_term_glm
        v_glm$type_model <- "ml_generalized_linear_regression"
        
        x$reg_param_glm <- input$reg_param_glm
        x$max_iter_glm <- input$max_iter_glm
        
        
        v_grad$type_model <- NA
        v_random$type_model <- NA
        v_decision_tree$type_model <- NA
        
      })
      
      observeEvent(input$run_decision_tree,{
        
        test_1$date <- input$test_selector[1]
        model$train_variables <- input$input_variables
        x$max_depth_decision_tree <- input$max_depth_decision_tree
        x$max_bins_decision_tree <- input$max_bins_decision_tree
        x$min_instance_decision_tree <- input$min_instance_decision_tree
        
        v_decision_tree$type_model <- "ml_decision_tree"
        
        v_glm$type_model <- NA
        v_grad$type_model <- NA
        v_random$type_model <- NA
        
      })
      
      
      output$input_curve <- renderDygraph({
        
        data <- as.data.table(data)
        
        curve_entries <- dygraph(data = eval(parse(text = paste0("data.table:::`[.data.table`(data,j =.(",date_column,",",y,"))"))))  %>% 
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
      
      
      test_1 <- reactiveValues(date = eval(parse(text = paste0("mean(data$",date_column,")"))))
      
      observeEvent(input$train_all,{
        
        test_1$date <- input$test_selector[1]
        model$train_variables <- input$input_variables
        v_decision_tree$type_model <- "ml_decision_tree"
        v_glm$type_model <- "ml_generalized_linear_regression"
        v_grad$type_model <- "ml_gradient_boosted_trees"
        v_random$type_model <- "ml_random_forest"
        
        t$step_size_gbm <- input$step_size_gbm
        v$subsampling_rate_gbm <- input$subsampling_rate_gbm
        
        t$num_tree_random_forest <- input$num_tree_random_forest
        v$subsampling_rate_random_forest <- input$subsampling_rate_random_forest
        x$max_depth_random_forest <-  input$max_depth_random_forest
        
        
        f$family_glm <- input$glm_family
        l$link_glm <- input$glm_link
        i$intercept_glm <- input$intercept_term_glm
        x$reg_param_glm <- input$reg_param_glm
        x$max_iter_glm <- input$max_iter_glm
        
        x$max_depth_decision_tree <- input$max_depth_decision_tree
        x$max_bins_decision_tree <- input$max_bins_decision_tree
        x$min_instance_decision_tree <- input$min_instance_decision_tree
        
        
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
      
      
      table_forecast <- reactive({
        
        
        data_results <- eval(parse(text = paste0("data.table:::`[.data.table`(data,j =.(",date_column,",",y,"))")))
        data_results <- eval(parse(text = paste0("data_results %>% filter(",date_column,">'", test_1$date,"') %>% as.data.table()")))
        var_input_list <- ""
        
        
        for (i in 1:length(model$train_variables)){var_input_list <- paste0(var_input_list,"+",model$train_variables[i])}
        var_input_list <- ifelse(startsWith(var_input_list,"+"),substr(var_input_list,2,nchar(var_input_list)),var_input_list)
        
        if (var_input_list != "+"){  
          
          
          data_spark_train <- eval(parse(text = paste0( "data %>% filter(",date_column,"<='", test_1$date,"') %>% as.data.table()")))
          data_spark_test <- eval(parse(text = paste0("data %>% filter(",date_column,">'", test_1$date,"') %>% as.data.table()")))
          
          data_spark_train <- copy_to(sc, data_spark_train, "data_spark_train", overwrite = TRUE)
          data_spark_test <- copy_to(sc, data_spark_test, "data_spark_test", overwrite = TRUE)
          
          
          if (!is.na(v_grad$type_model) & v_grad$type_model == "ml_gradient_boosted_trees"){
            
            
            t1 <- Sys.time()
            eval(parse(text = paste0("fit <- data_spark_train %>%",v_grad$type_model,"(", y ," ~ " ,var_input_list ,
                                     ",step_size =",t$step_size_gbm,
                                     ",subsampling_rate =",v$subsampling_rate_gbm,
                                     " )")))
            t2 <- Sys.time()
            
            time_gbm <- data.frame(`Training time` =  paste0(round(t2 - t1,1)," seconds"), Model = "Gradient boosted trees") 
            importance_gbm <- ml_feature_importances(fit) %>% mutate(model = "Gradient boosted trees")
            
            table_ml_gradient_boosted <- sdf_predict(data_spark_test, fit) %>% collect %>% as.data.frame() %>% select(prediction) %>% mutate(prediction = round(prediction,3))
            names(table_ml_gradient_boosted)[names(table_ml_gradient_boosted) == 'prediction'] <- "Gradient boosted trees"
            
          }
          
          if (!is.na(v_random$type_model) & v_random$type_model == "ml_random_forest"){
            
            t1 <- Sys.time()
            eval(parse(text = paste0("fit <- data_spark_train %>%",v_random$type_model,"(", y ," ~ " ,var_input_list ,
                                     ",num_trees  =",t$num_tree_random_forest,
                                     ",subsampling_rate =",v$subsampling_rate_random_forest,
                                     ",max_depth  =",x$max_depth_random_forest,
                                     ")")))
            t2 <- Sys.time()
            time_random_forest <- data.frame(`Training time` =  paste0(round(t2 - t1,1)," seconds"), Model = "Random forest")
            importance_random_forest <- ml_feature_importances(fit) %>% mutate(model = "Random forest")
            
            table_ml_random_forest <- sdf_predict(data_spark_test, fit) %>% collect %>% as.data.frame() %>% select(prediction)%>% mutate(prediction = round(prediction,3))
            names(table_ml_random_forest)[names(table_ml_random_forest) == 'prediction'] <- "Random forest"
            
          }
          
          if (!is.na(v_glm$type_model) & v_glm$type_model == "ml_generalized_linear_regression"){
            
            t1 <- Sys.time()
            eval(parse(text = paste0("fit <- data_spark_train %>%",v_glm$type_model,"(", y ," ~ " ,var_input_list ,
                                     ",family  = ", f$family_glm,
                                     ",link =",l$link_glm,
                                     ",fit_intercept =",input$intercept_term_glm,
                                     ",reg_param =",x$reg_param_glm,
                                     ",max_iter =",x$max_iter_glm,
                                     ")")))
            t2 <- Sys.time()
            time_glm <- data.frame(`Training time` =  paste0(round(t2 - t1,1)," seconds"), Model = "Generalized linear regression")
            
            table_ml_glm <- sdf_predict(data_spark_test, fit) %>% collect %>% as.data.frame() %>% select(prediction)%>% mutate(prediction = round(prediction,3))
            names(table_ml_glm)[names(table_ml_glm) == 'prediction'] <- "Generalized linear regression"
            
          }
          
          if (!is.na(v_decision_tree$type_model) & v_decision_tree$type_model == "ml_decision_tree"){
            
            t1 <- Sys.time()
            eval(parse(text = paste0("fit <- data_spark_train %>%",v_decision_tree$type_model,"(", y ," ~ " ,var_input_list ,
                                     ",max_depth  =",x$max_depth_decision_tree,
                                     ",max_bins  =",x$max_bins_decision_tree,
                                     ",min_instances_per_node  =",x$min_instance_decision_tree,
                                     ")")))
            t2 <- Sys.time()
            time_decision_tree <- data.frame(`Training time` =  paste0(round(t2 - t1,1)," seconds"), Model = "Decision tree")
            importance_decision_tree <- ml_feature_importances(fit) %>% mutate(model = "Decision tree")
            
            table_ml_decision_tree <- sdf_predict(data_spark_test, fit) %>% collect %>% as.data.frame() %>% select(prediction)%>% mutate(prediction = round(prediction,3))
            names(table_ml_decision_tree)[names(table_ml_decision_tree) == 'prediction'] <- "Decision tree"
            
          }
          
        }
        
        table_training_time <- rbind(time_gbm,time_random_forest,time_glm,time_decision_tree)
        table_importance <- rbind(importance_gbm,importance_random_forest,importance_decision_tree) %>% as.data.table()
        
        table_results <- cbind(data_results,table_ml_gradient_boosted,table_ml_random_forest,table_ml_glm,table_ml_decision_tree) %>% 
          as.data.table()
        
        list(traning_time = table_training_time, importance = table_importance, results = table_results)
        
      })
      
      
      output$output_curve <- renderDygraph({
        
        output_dygraph <- dygraph(data = table_forecast()[['results']]) %>% 
          dyAxis("y",valueRange = c(0,1.5 * max(eval(parse(text =paste0("table_forecast()[['results']]$",y)))))) 
        
        
        if (input$bar_chart_mode == TRUE){
          output_dygraph <- output_dygraph %>% dyBarChart()
        }
        
        output_dygraph %>% dyLegend(width = 800)
        
      })
      
      output$table_of_results <- renderDataTable({
        
        datatable(
          table_forecast()[['results']],
          extensions = 'Buttons', options = list(dom = 'Bfrtip',buttons = c('csv', 'excel', 'pdf', 'print'))
        ) 
        
        
      })
      
      
      
      output$feature_importance <- renderPlotly({
        
        if (nrow(table_forecast()[['importance']]) != 0){
          ggplotly(
            
            ggplot(data = table_forecast()[['importance']])+
              geom_bar(aes(reorder(feature,importance),importance,fill =  model),stat = "identity",width = 0.3)+
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

