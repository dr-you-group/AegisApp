
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AegisApp

<!-- badges: start -->
<!-- badges: end -->

AegisApp is a shiny app for AegisFunc that is a tool for Spatio-temporal
epidemiology based on OHDSI CDM.  
<https://github.com/dr-you-group/AegisFunc>

## Requirements

### AegisFunc

-   [AegisFunc](https://github.com/dr-you-group/AegisFunc)

## Installation

You can install the development version of AegisApp like so:

``` r
devtools::install()
```

## Load

Load AegisApp package before use.

``` r
library(AegisApp)
```

## Run

Run AegisApp to launch application.

``` r
AegisApp()
```

Launch AegisApp with sample data.  
- The sample data consist of 1) cdm source, 2) cohort list, 3) cohort
table.

``` r
shiny::runApp(
  appDir = AegisApp(),
  port = 8888,
  launch.browser = function(appUrl) {
    url <- paste0(appUrl, "/?demo=TRUE")
    utils::browseURL(url)
  },
  host = "0.0.0.0"
)
```
