library(rmarkdown)

render("../project/bike-share-prediction.Rmd",
       params=list(weekday=c(0,1,2,3,4,5,6)))
