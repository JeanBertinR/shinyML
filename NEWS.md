# shinyML 1.0.1 (2021-02-21)

## New features
* **Histogram plots** on `shiny_h2o` and `shiny_spark` functions now integrate density curves. 
* `shiny_h2o` and `shiny_spark` functions ensure **reproducibility** of results when user reproduce the same parameters for a given machine learning model
* `shiny_h2o` and `shiny_spark` functions now work with an input dataset that contains a **POSIXct** column


# shinyML 1.0.0 (2020-10-02)

## New features
* `shiny_h2o` and `shiny_spark` functions have merged into `shinyML_regression` function: H2O or Spark can now be chosen just using the `framework` argument. 
* A new function `shinyML_classification` has been implemented to train and test machine learning models for **classification tasks** : classification results can be viewed through confusion matrix charts in addition to existing  available item on old package versions . 
* When the framework is set to **H2O** for `shinyML_regression` or `shinyML_classification` function, authorized model families for auto ML searching can be manually specified. 
* Two new **info cards** have been set on the upper part to precise the type of machine learning task (regression or classification) and the dimension of input dataset. 
* **Autocorrelation plots** are now available for numerical variables on **Variable summary** tab

## Breaking changes 
* User interface completely changed on shiny apps for both `shinyML_regression` and `shinyML_classification` functions : `argonDash` and `argonR` shiny API have been used to make user experience even more friendly.
* Both `shinyML_regression` and `shinyML_classification` **automatically detect if input dataset contains a time-based column**: in that case, training and testing dataset splitting is done in order to respect chronology. On the other case, rows are randomly assigned to training or testing dataset according to a splitting percentage parameter. 



# shinyML 0.2.0 (2019-10-28)

## New features
* Informations about cluster memory and number of used CPU(s) are now available on the left side when running `shiny_h2o` and `shiny_h2o` functions 
* Three new tabs are now available at the top of the `shiny_h2o` and `shiny_h2o` dashboards to explore input data set. The **Variable Summary** tab allows to check types and box plot of each input variable. The **Explore dataset** tab gives the possibility to understand dependencies by plotting each data variable as a function of another. An overview of all variables dependencies is also available in the  **Correlation matrix** tab.

## Breaking changes 
* Output tabs like **Compare models performances**, **Feature importance** and **Table of results** are now hidden when no model has been running. It showed a message indicating that output couldn't be calculated because no model was trained. 
* x input parameter of `shiny_h2o` and `shiny_h2o` have been removed to give even more simplicity for the user: the dashboard now indicates at the top right of the dashboard which input variable are available to train the model (output variable y is automatically removed from the list). 

## Bug fixes
* Intercept term button for **Generalized linear regression** model has changed on both functions due to problem on the UI: the cursor was not at the right position when selected. 
* Link button of **Generalized linear regression** model doesn't have any effect on the output variable due to omission to take this parameter in account. This issue has been fixed. 



# shinyML 0.1.1 (2019-08-07)

## Bug fixes
* autoML method is now working on `shiny_h2o` function: the user now just need to set maximum calculation time. 

## Breaking changes
* Default `share_app` argument of `shiny_h2o` and `shiny_spark` examples have been set to FALSE.

