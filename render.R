library(rmarkdown)
<<<<<<< HEAD

render("../project/bike-share-prediction.Rmd",
       params=list(weekday=c(0,1,2,3,4,5,6)))
||||||| 79c0e2b
rmarkdown::render("../project/bike-share-prediction.Rmd",params=list(weekday=c(0,1,2,3,4,5,6)))
=======
rmarkdown::render("../bike-share-prediction/Analysis.Rmd",params=list(weekday=c(0,1,2,3,4,5,6)))
>>>>>>> 6869753780f6e0aacc1603a70b36d7ccf2872e1b
