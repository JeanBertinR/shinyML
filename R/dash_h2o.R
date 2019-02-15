#' @title Create a shiny app to implement and compare supervised regression models on time series (framework used: h2o)
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
#' dash_h20(data =longley2,x = c("GNP_deflator","Unemployed" ,"Armed_Forces", "Population","Employed"),y = "GNP",date_column = "Year",share_app = TRUE,port = 5845)
#' @export

dash_h20 <- function(data = data,x,y,date_column, share_app = FALSE,port = NULL ){
  
  h2o.init()
  
  
  data <- data.table(data)
  
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
                 fluidRow(
                   fluidRow(
                     box(
                       dygraphOutput("input_curve", height = 120, width = 930)),
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
                     ),
                     
                     box(
                       title = "Neural network model",status = "warning",
                       
                       materialSwitch(label = "Intercept term",inputId = "intercept_term_glm",status = "primary",value = TRUE),
                       sliderInput(label = "Regularization parameter (lambda)",inputId = "reg_param_glm",min = 0,max = 10,value = 0),
                       sliderInput(label = "Maximum iteraions",inputId = "max_iter_glm",min = 50,max = 300,value = 100),
                       actionButton("run_neural_network","Run generalized linear regression",style = 'color:white; background-color:orange; padding:4px; font-size:150%',
                                    icon = icon("cogs",lib = "font-awesome"))
                       ,width = 3 ),
                     
                     dataTableOutput("table_test")
                     
                   )
                   
                   
                 )
          )
        )
      )
    )
    ,
    
    server = function(session, input, output) {
      set.seed(122)
      model <- reactiveValues(train_variables = NA)
      test_1 <- reactiveValues(date = eval(parse(text = paste0("mean(data$",date_column,")"))))
      
      
      observeEvent(input$run_neural_network,{
        
        test_1$date <- input$test_selector[1]
        model$train_variables <- input$input_variables
      })
      
      
      
      output$input_curve <- renderDygraph({
        
        
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
      
      
      table_forecast <- reactive({
        
        data_results <- eval(parse(text = paste0("data.table:::`[.data.table`(data,j =.(",date_column,",",y,"))")))
        var_input_list <- vector()
        
        for (i in 1:length(model$train_variables)){
          var_input_list <- c(var_input_list,model$train_variables[i])
          
        }
        
        #var_input_list <- ifelse(startsWith(var_input_list,"+"),substr(var_input_list,2,nchar(var_input_list)),var_input_list)
        
        
        if (var_input_list != "+"){  
          data_h2o_train <- eval(parse(text = paste0( "as.h2o(data %>% filter(",date_column,"<='", test_1$date,"') %>% as.data.table())")))
          data_h2o_test <- eval(parse(text = paste0("as.h2o(data %>% filter(",date_column,">'", test_1$date,"') %>% as.data.table())")))
          
          
          dl_fit1 <- h2o.deeplearning(x = "Unemployed",
                                      y = y,
                                      training_frame = data_h2o_train,
                                      model_id = "dl_fit1",
                                      hidden = c(5,5),
                                      seed = 1)
          
        }
        
        
        var_input_list %>% as.data.table()
        
      })
      
      output$table_test <-renderDataTable({
        table_forecast()
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



dash_h20(data =longley2,x = c("GNP_deflator","Unemployed" ,"Armed_Forces", "Population","Employed"),y = "GNP",date_column = "Year",share_app = TRUE,port = 5845)
