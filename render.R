library(rmarkdown)
rmarkdown::render("../bike-share-prediction/Analysis.Rmd",params=list(weekday=c(0,1,2,3,4,5,6)))
