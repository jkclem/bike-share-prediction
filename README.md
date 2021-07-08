# bike-share-prediction

## Purpose

The purpose of this project is to demonstrate how to build and use predictive models and automate Markdown reports using R.

## Required Packages

The following packages are required for this project:

- `readr`: reading in CSV files containing data
- `tidyverse`: general data manipulation and plotting
- `cowplot`: plotting in grids
- `caret`: testing and making machine learning models
- `rmarkdown`: rendering Rmd files as HTML files


# Automation

```
weekday <- unique(bikeday$weekday)
output_file <- paste0( weekday, ".html")
params=lapply(weekday, FUN=function(x){list(weekday=x)})
reports <- tibble(output_file, params)
reports
apply(reports, MARGIN=1, 
    FUN=function(x){
                    render(input="../bike-share-prediction/Analysis.Rmd",
                           output_file=x[[1]],params=x[[2]])
    })
```