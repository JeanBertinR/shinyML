# shinyML 0.2.0 (2019-10-28)

## New features
* Informations about cluster memory and number of used CPU(s) are now available on the left side when running `shiny_h2o` and `shiny_h2o` functions 
* Three new tabs are now available at the top of the `shiny_h2o` and `shiny_h2o` dashboards to explore input data set. The **Variable Summary** tab allows to check types and box plot of each input variable. The **Explore dataset** tab gives the possibility to understand dependencies by plotting each data variable as a function of another. An overview of all variables dependencies is also available in the  **Correlation matrix** tab.

## Breaking changes 
* Output tabs like **Compare models performances**, **Feature importance** and **Table of results** are now hidden when no model has been runed. It showed a message indicating that ouput couldn't be calculated because no model was trained. 
* x input parameter of `shiny_h2o` and `shiny_h2o` have been removed to give even more simplicity for the user: the dashboard now indicates  at the top right of the dashboard which input variable are available to train the model (output variable y is automatically removed from the list). 

## Bug fixes
* Intercept term button for **Generalized linear regression** model has changed on both functions due to problem on the UI: the cursor was not at the right position when selected. 
* Link button of **Generalized linear regression** model doesn't have any effect on the output variable due to omission to take this parameter in account. This issue has been fixed. 



# shinyML 0.1.1 (2019-08-07)

## Bug fixes
* autoML method is now working on `shiny_h2o` function: the user now just need to set maximum calculation time. 

## Breaking changes
* Default `share_app` argument of `shiny_h2o` and `shiny_spark` examples have been set to FALSE.

