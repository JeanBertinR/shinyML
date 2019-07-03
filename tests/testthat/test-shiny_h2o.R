context("shiny_h2o")




describe("shiny_h2o", {
  it("returns an uninitialized MWController in a non interactive situation", {
    longley2 <- longley %>% mutate(Year = as.Date(as.character(Year),format = "%Y"))
    C <- shiny_h2o(data =longley2,x = c("GNP_deflator","Unemployed" ,"Armed_Forces","Employed"),
              y = "GNP",date_column = "Year",share_app = TRUE,port = 1234)
    
    
    
  })
  
})