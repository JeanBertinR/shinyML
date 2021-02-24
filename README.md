Compare Supervised Machine Learning Models Using Shiny App
================

<img src="vignettes/logo_shinyML.jpg" width=200 align="right" />

[![CRAN Status
Badge](http://www.r-pkg.org/badges/version/shinyML)](https://CRAN.R-project.org/package=shinyML)
[![CRAN Downloads
Badge](https://cranlogs.r-pkg.org/badges/shinyML)](https://CRAN.R-project.org/package=shinyML)
[![CRAN Downloads
Badge](https://cranlogs.r-pkg.org/badges/grand-total/shinyML)](https://CRAN.R-project.org/package=shinyML)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-red.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Travis-CI Build
Status](https://travis-ci.org/JeanBertinR/shinyML.svg?branch=master)](https://travis-ci.org/JeanBertinR/shinyML)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/JeanBertinR/shinyML?branch=master&svg=true)](https://ci.appveyor.com/project/JeanBertinR/shinyML)
[![Lifecycle
Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)

# shinyML

Implement in one line of code a shareable web app to compare
**supervised machine learning models** for regression and classification
tasks!
<p align="center">
<img src="vignettes/shinyML.png">
</p>

With `shinyML`, you can compare your favorite regression or
classification models issued from **H2O** or **Spark** frameworks
without any effort.

### Installation

The package can be installed from **CRAN**:

``` r
install.packages("shinyML")
```

You can also install the latest development version from **github**:

``` r
devtools::install_github("JeanBertinR/shinyML")
```

### Getting started: create the shinyML web app in just one list of code

This is a basic examples which shows you how to run the app:

``` r
library(shinyML)

# An example of regression task 
shinyML_regression(data = iris,y = "Petal.Width",framework = "h2o")

# An example of classification task  
shinyML_classification(data = iris,y = "Species",framework = "h2o")
```

Please note that `shinyML_regression` and `shinyML_classification` will
automatically detect if you input dataset contains time-based column: in
that case, train/test splitting will be adapted to time-series
forecasting.

``` r
# An example of time-series forecasting
longley2 <- longley %>% mutate(Year = as.Date(as.character(Year),format = "%Y"))
shinyML_regression(data = longley2,y = "Population",framework = "h2o")
```

### Explore input dataset before running the models…

Before running machine learning models, it can be useful to inspect the
**distribution of each variable** and to have an insight of
**dependencies between explanatory variables**. Both`shinyML_regression`
and `shinyML_classification` functions allows to check **classes of
explanatory variables**, plot **histograms** of each distribution and
show **correlation matrix** between all variables. This tabs can be used
to determine if some variable are strongly correlated to another and
eventually removed from the training phase.You can also plot variation
of every variable as a function of another using the **“Explore
dataset”** tab.

<p align="center">
<img src="vignettes/explore_data.gif">
</p>

### Test different machine learning techniques and hyper-parameters configurations with just a few clicks

To test supervised machine learning models on `shinyML` package, the
first step consist in **separating train and test period** from your
dataset: this can be done in one second using slider button on the right
shinyML app side. You can also remove variables from your initial
selection directly from app just simply using “Input variable” textbox.
You are then free to **select hyper-parameters configuration** for your
favorite machine learning model.  
Note that hidden layers of deep learning technique can be set inside the
corresponding text box: the default c(200,200) configuration corresponds
to a two hidden-layers neural network, with 200 neurons for each layer.
<p align="center">
<img src="vignettes/one_model.gif">
</p>

### Run at the same time all machine learning techniques to compare variable importances and error metrics

You can easily use `shinyML` package to compare **different machine
learning techniques with your own hyper-parameters configuration**. For
that, you will just need to use shiny app buttons corresponding to your
parameters and click then to **“Run tuned models !”**
<p align="center">
<img src="vignettes/all_models.gif">
</p>

You will see a **validation message box** once all models have been
trained: at that point, you can have an overview of your results
comparing variables importances and error metrics like **MAPE** or
\*RMSE\*\*.

### Run autoML algorithm to find automatically configure the best machine learning regression model associated to your dataset

`AutoML` algorithm will **automatically find the best algorithm** to
suit your regression or classification task: the user will be informed
of the **machine learning model** that has been selected and know
**which hyper-parameters** have been chosen.

The only setting that must be adjusted by the user is the **maximum time
authorized for searching**.
<p align="center">
<img src="vignettes/auto_ML.gif">
</p>

For more information take a look at the [package
vignette](https://github.com/JeanBertinR/shinyML/blob/master/vignettes/vignettes.Rmd).
