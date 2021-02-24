## Resubmission
This is a resubmission. In this version I have:

* Changed invalid URL on Readme.md ( https://www.tidyverse.org/lifecycle/) to the appropriate one (https://lifecycle.r-lib.org/articles/stages.html)

* Added files contained in vignettes folder to .Rbuidignore in order to decrease the size of installed package under 5 MB

## New patch version of shinyML package
This is a new patch version (1.0.1) of the shinyML package (I've fixed bugs without adding any significant new features). 
In this new version I have:

* Changed the style of histogram plots on both `shinyML_regression` and `shinyML_classification` functions and added the **density curve** on this chart

* Forced **reproducibility of machine learning results** on both `shinyML_regression` and `shinyML_classification` functions

* Fixed a bug to make `shinyML_regression` and `shinyML_classification` functions work with **POSIXct** input data column

## New major version of shinyML package
This is a new major version (1.0.0) of the shinyML package. 
In this new version I have:

* Merged `shiny_h2o` and `shiny_spark` functions to one unique function `shinyML_regression`: framework can now be chosen using the **framework** parameter.

* Added a new function `shinyML_classification` to train and test machine learning model on **classification tasks**. This function uses the same arguments as `shinyML_regression` function, specifying however that y output variable class must be categorical.

* Added checkboxes to choose which machine learning models are authorized during the **autoML** searching phase.

* Changed user interfaces on shiny apps for both `shinyML_regression` and `shinyML_classification` functions : `argonDash` and `argonR` shiny API have been used to make user experience even more friendly.

* Added an automatic detection of time-based columns on the input dataset to consequently adapt training and testing dataset splitting.

* Added two new **info cards** on the app upper part to precise which machine learning task is running (regression or classification) and the dimension of input dataset. 

* Added **autocorrelation plots** for numerical variables on **Variable summary** tab.



## New minor version of shinyML package
This is a new minor version (0.2.0) of the shinyML package. 
In this new version I have:

* Added three new tabs **Compare models performances**, **Feature importance** and **Table of results** at the top of dashboards to explore input dataset before running the models 

* Added two valueboxes on the left of the dashboard to indicate memory cluster size and number of used CPU(s). 

* Fixed an issue on "Link" parameter of **Generalized linear regression** model: this parameter was not taken into account by the model

* Fixed an issue on user interface on **Generalized linear regression** box: the cursor was not at the right position when selected

* Removed x parameter on `shiny_h2o` and `shiny_spark` functions to make it run even more easily. 

## Resubmission
This is a resubmission. In this version I have:

* Rewrote CRAN URL in canonical form (https://CRAN.R-project.org/package=shinyML) on Readme.Rmd file


## New patch version of shinyML package
This is a new patch version (0.1.1) of the shinyML package (I've fixed bugs without adding any significant new features). 
In this version I have:

* Added a missing block to shiny_h2o.R to make autoML method working 

* Default 'share_app' argument of `shiny_h2o` and `shiny_spark` examples have been set to FALSE


## Resubmission
This is a resubmission. In this version I have:

* Capitalized only the first letter of names instead of the whole name in the DESCRIPTION file 

* Rewrote all descritption field on the DESCRIPTION file to make it clearer: please notice that the package is just an implemnetation of a shiny app 


## Resubmission
This is a resubmission. In this version I have:

* Modified the title to make it describe the package more precisely

* Checked the new title lenght remains less than 65 characters.

* Modified the title in the Readme.Rmd file to make it correspond to the title in the DESCRIPTION file  

* Written "License: GPL-3" instead of "GPL-3 | file LICENSE" in the DESCRIPTION file

* Added a reference in the description field on the DESCRIPTION file in the form "authors (year, ISBN:...)" that describe the methods in the package 


## Resubmission
This is a resubmission. In this version I have:

* Converted the DESCRIPTION title to title case.

* Right spelled words in DESCRIPTION:
    differents (9:111) -> different
    serie (9:145) -> series
    shareable (8:66) -> this word has been deleted 
    
* Prevented the Description field to start with the package name 

## Test environments
* OS X (on travis-ci), R-oldrel, R-release
* linux (on travis-ci), R-oldrel, R-release
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
There are currently no downstream dependencies for this package