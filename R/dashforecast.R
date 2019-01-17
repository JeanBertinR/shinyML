library(h2o)
library(shiny)
library(shinydashboard)
library(plotly)

dashforecast <- function(data,x,y, share_app = FALSE,port = NULL ){
  
  app <- shinyApp(
    ui = dashboardPage(
      dashboardHeader(title = "Compare forecast models"),
      dashboardSidebar(    
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
        )),
      
      dashboardBody(
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
      
      
      output$plot2 <- renderPlotly({
        plot_ly(data = data,x = iris$Sepal.Width,y = iris$Petal.Length)
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

dashforecast(share_app = TRUE,port = 4655)
plot_ly(data = iris,x = eval(parse(text = paste0(data,"$",x))), y = eval(parse(text = paste0(data,"$",y))))


y <- "Petal.Length"

x <- "Sepal.Width"
data <- "iris"
