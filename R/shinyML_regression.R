library(data.table)
library(shiny)
library(shinydashboard)
library(plotly)
library(dygraphs)
library(h2o)
library(shinycssloaders)
library(shinyWidgets)
library(DT)
library(tidyr)
library(dplyr)
library(sparklyr)

library(shiny)
library(argonR)
library(argonDash)
library(magrittr)

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


shinyML_regression <- function(data = data,y, share_app = FALSE,port = NULL){
  
  
  
  # Convert input data must be a data table object
  data <- data.table(data)
  
  # Run h2o instance (might require to unset proxy authentification credentials )
  Sys.setenv(http_proxy="")
  Sys.setenv(http_proxy_user="")
  Sys.setenv(https_proxy_user="")
  h2o.init()
  h2o::h2o.no_progress()
  cluster_status <- h2o.clusterStatus()
  
  
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
  
  # # Test if date_column is in data colnames
  # if (!(date_column %in% colnames(data))){
  #   stop("date_column must match one data input variable")
  # }
  
  
  # Test if input data does not exceed one million rows
  if (nrow(data) > 1000000) {
    stop("Input dataset must not exceed one million rows")
  }
  
  
  # Initialize all variables
  model <- reactiveValues()
  train_1 <- reactiveValues()
  
  # By default, start date and stop dates for test period correspond to mean and max of values of date_colum
  # test_1 <- reactiveValues(date = eval(parse(text = paste0("mean(data$",date_column,")"))))
  # test_2 <- reactiveValues(date = eval(parse(text = paste0("max(data$",date_column,")"))))
  
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
  
  
  
  ## ---------------------------------------------------------------------------- SIDEBAR -----------------------------------
  
  
  argonSidebar <- argonDashSidebar(
    vertical = T,side = "right",
    skin = "light",
    background = "darkblue",
    size = "md",
    id = "my_sidebar",
    brand_url = "https://jeanbertinr.github.io/shinyMLpackage/",
    brand_logo = "https://www.zupimages.net/up/20/09/djw2.png",
    dropdownMenus = argonDropNav(
      title = "Dropdown Mdsfenu", 
      src = "https://demos.creative-tim.com/argonhgf-dashboard/assets/img/theme/team-4-800x800.jpg", 
      orientation = "right",
      argonDropNavTitle(title = "Welcome!"),
      argonDropNavItem(
        title = "Item 1", 
        src = "https://www.google.com", 
        icon = argonIcon("single-02")
      ),
      argonDropNavItem(
        title = "Item 2", 
        src = NULL, 
        icon = argonIcon("settings-gear-65")
      ),
      argonDropNavDivider(),
      argonDropNavItem(
        title = "Item 3", 
        src = "#", 
        icon = argonIcon("calendar-grid-58")
      )
    ),
    argonSidebarHeader(title = "Framework"),
    argonSidebarMenu(
      argonSidebarItem(
        tabName = "tab_h2o",
        icon = argonIcon(name = "tv-2", color = "info"),
        "H2O"
      ),
      argonSidebarItem(
        tabName = "tab_spark",
        icon = argonIcon(name = "tv-1", color = "green"),
        "Spark"
      )
      
    ),
    argonSidebarDivider(),
    argonSidebarHeader(title = "Other Items")
  )
  
  ## ---------------------------------------------------------------------------- NAVBAR -----------------------------------
  
  
  
  argonNav <- argonDashNavbar(
    argonDropNav(
      title = "Dropdown Menu", 
      src = "https://demos.creative-tim.com/argon-dashboard/assets/img/theme/team-4-800x800.jpg", 
      orientation = "right",
      argonDropNavTitle(title = "Welcome!"),
      argonDropNavItem(
        title = "Item 1", 
        src = "https://www.google.com", 
        icon = argonIcon("single-02")
      ),
      argonDropNavItem(
        title = "Item 2", 
        src = NULL, 
        icon = argonIcon("settings-gear-65")
      ),
      argonDropNavDivider(),
      argonDropNavItem(
        title = "Item 3", 
        src = "#", 
        icon = argonIcon("calendar-grid-58")
      )
    )
  )
  
  
  
  
  
  
  
  
  ## ---------------------------------------------------------------------------- HEADER -----------------------------------
  
  
  argonHeader <- argonDashHeader(
    gradient = TRUE,
    color = "default",
    separator = TRUE,
    separator_color = "secondary"
    # argonCard(
    #   title = "Argon Card",
    #   src = "http://www.google.com",
    #   hover_lift = TRUE,
    #   shadow = TRUE,
    #   shadow_size = NULL,
    #   hover_shadow = FALSE,
    #   border_level = 0,
    #   icon = argonIcon("atom"),
    #   status = "primary",
    #   background_color = NULL,
    #   gradient = FALSE, 
    #   floating = FALSE,
    #   "This is the content"
    # )
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
  
  
  ## ---------------------------------------------------------------------------- ONGLET H20 -----------------------------------
  
  
  h2o_tab <- argonTabItem(
    tabName = "tab_h2o",
    
    # classic cards
    argonH1("Classic Cards", display = 4),
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
                radioButtons(label = "Family",inputId = "glm_family",choices = c("gaussian","poisson", "gamma","tweedie"),
                             selected = "gaussian"),width = 6),
              
              argonColumn(
                radioButtons(label = "Link",inputId = "glm_link",choices = c("identity","log"),selected = "identity"),
                switchInput(label = "Intercept term",inputId = "intercept_term_glm",value = TRUE,width = "auto"),width = 6)
            ),
            
            sliderInput(label = "Lambda",inputId = "reg_param_glm",min = 0,max = 10,value = 0),
            sliderInput(label = "Alpha (0:Ridge <-> 1:Lasso)",inputId = "alpha_param_glm",min = 0,max = 1,value = 0.5),
            sliderInput(label = "Maximum iteraions",inputId = "max_iter_glm",min = 50,max = 300,value = 100),
            #argonButton(name = "Run glm",src = "run_glm",status = "warning",icon = icon("cogs",lib = "font-awesome"),size = "sm")
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
        
      ),
      
      
      
      
      
      
      
      
      argonCard(
        width = 12,
        title = "Argon Card",
        src = NULL,
        hover_lift = TRUE,
        shadow = TRUE,
        shadow_size = NULL,
        hover_shadow = FALSE,
        border_level = 0,
        icon = argonIcon("atom"),
        status = "primary",
        background_color = NULL,
        gradient = FALSE, 
        floating = FALSE,
        argonRow(
          argonColumn(
            width = 6,
            radioButtons(
              "dist", 
              "Distribution type:",
              c("Normal" = "norm",
                "Uniform" = "unif",
                "Log-normal" = "lnorm",
                "Exponential" = "exp")
            )
          ),
          argonColumn(width = 6, plotOutput("plot"))
        )
      ) 
    ),
    br(),
    
    # info cards
    argonH1("Info Cards", display = 4),
    argonRow(
      argonInfoCard(
        value = "350,897", 
        title = "TRAFFIC", 
        stat = 3.48, 
        stat_icon = icon("arrow-up"),
        description = "Since last month", 
        icon = argonIcon("planet"), 
        icon_background = "danger",
        hover_lift = TRUE
      ),
      argonInfoCard(
        value = "2,356", 
        title = "NEW USERS", 
        stat = -3.48, 
        stat_icon = icon("arrow-down"),
        description = "Since last week", 
        icon = icon("chart-pie"), 
        icon_background = "warning",
        shadow = TRUE
      ),
      argonInfoCard(
        value = "924", 
        title = "SALES", 
        stat = -1.10, 
        stat_icon = icon("arrow-down"),
        description = "Since yesterday", 
        icon = icon("users"), 
        icon_background = "yellow",
        background_color = "default"
      ),
      argonInfoCard(
        value = "49,65%", 
        title = "PERFORMANCE", 
        stat = 12, 
        stat_icon = icon("arrow-up"),
        description = "Since last month", 
        icon = icon("percent"), 
        icon_background = "info",
        gradient = TRUE,
        background_color = "orange",
        hover_lift = TRUE
      )
    ),
    
    # profile cards
    argonH1("User Cards", display = 4),
    argonRow(
      argonColumn(
        width = 3,
        argonUser(
          title = "Ryan Tompson",
          subtitle = "Web Developer",
          src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-1-800x800.jpg"
        )
      ),
      argonColumn(
        width = 3,
        argonUser(
          title = "Romina Hadid",
          subtitle = "Marketing Strategist",
          src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-2-800x800.jpg"
        )
      ),
      argonColumn(
        width = 3,
        argonUser(
          title = "Alexander Smith",
          subtitle = "UI/UX Designer",
          src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-3-800x800.jpg"
        )
      ),
      argonColumn(
        width = 3,
        argonUser(
          title = "John Doe",
          subtitle = "Founder and CEO",
          src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-4-800x800.jpg"
        )
      )
    ),
    br(), br(),
    
    argonH1("Profile Card", display = 4),
    argonRow(
      argonColumn(
        width = 12,
        argonProfile(
          title = "John",
          subtitle = "Japan, Kagoshima",
          src = "https://demos.creative-tim.com/argon-design-system/assets/img/theme/team-1-800x800.jpg",
          url = "https://www.google.com",
          url_1 = "https://www.google.com",
          url_2 = "https://www.google.com",
          stats = argonProfileStats(
            argonProfileStat(
              value = 22,
              description = "Friends"
            ),
            argonProfileStat(
              value = 10,
              description = "Photos"
            ),
            argonProfileStat(
              value = 89,
              description = "Comments"
            )
          ),
          "An artist of considerable range, Ryan — 
                  the name taken by Melbourne-raised, 
                  Brooklyn-based Nick Murphy — writes, 
                  performs and records all of his own music, 
                  giving it a warm, intimate feel with a solid 
                  groove structure. An artist of considerable 
                  range."
        )
      )
    )
  )
  
  
  
  
  
  
  
  
  
  ## ---------------------------------------------------------------------------- ONGLET SPARK -----------------------------------
  
  spark_tab <- argonTabItem(
    tabName = "tab_spark",
    radioButtons(
      inputId = "cardWrap", 
      inline = TRUE,
      label = "Enable card wrap?",
      choices = c("Enable", "Disable"), 
      selected = "Enable"
    ),
    uiOutput("argonTable")
  )
  
  
  
  ## ---------------------------------------------------------------------------- LANCEMENT APPLI -----------------------------------
  
  
  # App
  app <- shiny::shinyApp(
    ui = argonDashPage(
      title = "Argon Dashboard Demo",
      author = "David",
      description = "Argon Dash Test",
      sidebar = argonSidebar,
      navbar = argonNav,
      header = argonHeader,
      body = argonDashBody(
        argonTabItems(
          h2o_tab,
          spark_tab
          
        )
      ),
      footer = argonFooter
    ),
    server = function(input, output) {
      output$distPlot <- renderPlot({
        hist(rnorm(input$obs))
      })
      
      output$plot <- renderPlot({
        dist <- switch(
          input$dist,
          norm = rnorm,
          unif = runif,
          lnorm = rlnorm,
          exp = rexp,
          rnorm
        )
        
        hist(dist(500))
      })
      
      # argonTable
      output$argonTable <- renderUI({
        
        wrap <- if (input$cardWrap == "Enable") TRUE else FALSE
        
        argonTable(
          cardWrap = wrap,
          headTitles = c(
            "PROJECT",
            "BUDGET",
            "STATUS",
            "USERS",
            "COMPLETION",
            ""
          ),
          argonTableItems(
            argonTableItem("Argon Design System"),
            argonTableItem(dataCell = TRUE, "$2,500 USD"),
            argonTableItem(
              dataCell = TRUE, 
              argonBadge(
                text = "Pending",
                status = "danger"
              )
            ),
            argonTableItem(
              argonAvatar(
                size = "sm",
                src = "https://image.flaticon.com/icons/svg/219/219976.svg"
              )
            ),
            argonTableItem(
              dataCell = TRUE, 
              argonProgress(value = 60, status = "danger")
            ),
            argonTableItem(
              argonButton(
                name = "Click me!",
                status = "warning",
                icon = "atom",
                size = "sm"
              )
            )
          )
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


shinyML_regression(data = longley2,y = "Population",share_app = F)
install.packages("argonDash")
library(argonDash)
