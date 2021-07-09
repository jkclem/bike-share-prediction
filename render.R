###
# Authors: Jingjing Li and John Clements
# Date: 07/06/2021
# Purpose: Run and Render the Analysis.Rmd file for each day of the week.
###

library(rmarkdown)

rmarkdown::render("Analysis.Rmd",
                  params=list(weekday=c(0,1,2,3,4,5,6)))

#output_file <- paste0( weekday, ".html")
#params<-lapply(weekday, FUN=function(x){list(weekday=x)})
#reports <- tibble(output_file, params)

apply(reports, MARGIN=1, 
      FUN=function(x){
        render(input="Analysis.Rmd",
               output_file=paste0("Reports\\", x[[1]] ),
               params=list(weekday=c(0,1,2,3,4,5,6))#x[[2]])
      }))