
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dashR

Implement in one line of code a shareable web app to compare supervised
machine learning regression models\!

With dashR, you can compare your favorite regression models issued from
H2O or Spark frameworks without any effort.

## Installation

You can install the released version of dashR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("dashR")
```

You can also install the latest development version from github:

``` r
devtools::install_github("JeanBertinR/dashR", ref="develop")
```

## Getting started

This is a basic example which shows you how to run the app:

``` r

library(dashR)
longley2 <- longley %>% mutate(Year = as.Date(as.character(Year),format = "%Y"))
#dash_spark(data =longley2,x = c("GNP_deflator","Unemployed" ,"Armed_Forces","Employed"),y = "GNP",date_column = "Year",share_app = TRUE,port = 3951)
```

![An example of output of manipulateWidget](vignettes/run_glm.gif)
