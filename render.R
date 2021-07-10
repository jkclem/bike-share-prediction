###
# Authors: Jingjing Li and John Clements
# Date: 07/06/2021
# Purpose: Run and Render the Analysis.Rmd file for each day of the week.
###

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
                                         weekdayList[[weekday+1]]),
                    # and pass the weekday id as the weekday parameter.
                    params=list(weekday=weekday,
                                dayName=weekdayList[[weekday+1]]))
}