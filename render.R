###
# Authors: Jingjing Li and John Clements
# Date: 07/06/2021
# Purpose: Run and Render the Analysis.Rmd file for each day of the week.
###

library(rmarkdown)

rmarkdown::render("./Analysis.Rmd",
                  params=list(weekday=c(0,1,2,3,4,5,6)))