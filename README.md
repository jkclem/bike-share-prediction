# bike-share-prediction

## Purpose

The purpose of this project is to demonstrate how to build and use predictive models and automate Markdown reporting using R. We use the [bike share dataset](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset) and automate the exploratory analysis and predictive modeling for the number of bike rentals per day for each day of the week.

## Required Packages

The following packages are required for this project:

- `readr`: reading in CSV files containing data
- `tidyverse`: general data manipulation and plotting
- `cowplot`: plotting in grids
- `caret`: testing and making machine learning models
- `rmarkdown`: rendering Rmd files as HTML files

## Output Files

- The analysis for [Monday is available here](Reports/Monday.md)
- The analysis for [Tueday is available here](Reports/Tuesday.md)
- The analysis for [Wednesday is available here](Reports/Wednesday.md)
- The analysis for [Thursday is available here](Reports/Thursday.md)
- The analysis for [Friday is available here](Reports/Friday.md)
- The analysis for [Saturday is available here](Reports/Saturday.md)
- The analysis for [Sunday is available here](Reports/Sunday.md)

## Automation

We automate the analysis process in the `render.R` file, which contains the code below. It loops through the weekday ID numbers and runs the analysis and puts the rendered file in the **Reports** folder.

```
# Create a list of weekday names in the same order as the weekday ids. 
weekdayList <- list("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                    "Friday", "Saturday")

# Loop through the weekday indexes going from Sunday to Saturday.
for(weekday in c(0,1,2,3,4,5,6)){
  # Render Analysis.Rmd file...
  rmarkdown::render("Analysis.Rmd",
                    # and put the output in the Reports folder naming the file
                    # as the corresponding weekday by adding 1 to the id to get
                    # the weekday name for the file name...
                    output_file = paste0("Reports/", 
                                         weekdayList[[weekday+1]], 
                                         ".html"),
                    # and pass the weekday id as the weekday parameter.
                    params=list(weekday=weekday,
                                dayName=weekdayList[[weekday+1]]))
```