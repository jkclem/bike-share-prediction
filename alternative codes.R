bikeDayTrainSubset$season <- as.numeric(bikeDayTrainSubset$season)
pairs(bikeDayTrainSubset )
lmfit1a <- lm(casual~temp, data=bikeDayTrainSubset)
summary(lmfit1a)

lmfit1b <- lm(casual~ mnth, data=bikeDayTrainSubset)
summary(lmfit1b) 

lmfit1c <- lm(casual~ windspeed , data=bikeDayTrainSubset)
summary(lmfit1c) 

lmfit1d <- lm(casual~season , data=bikeDayTrainSubset)
summary(lmfit1d) 

lmfit2a <- lm(casual~temp*season, data=bikeDayTrainSubset)
summary(lmfit2a)

lmfit2b <- lm(casual~temp*windspeed, data=bikeDayTrainSubset)
summary(lmfit2b)

lmfit2c <- lm(casual~season*windspeed, data=bikeDayTrainSubset)
summary(lmfit2c)

lmfit3a <- lm(casual~ temp + I(temp^2), data=bikeDayTrainSubset)
summary(lmfit3a) 

lmfit3b <- lm(casual~ windspeed + I(windspeed ^2), data=bikeDayTrainSubset)
summary(lmfit3b)
lmfit3c <- lm(casual~ season + I(season^2), data=bikeDayTrainSubset)
summary(lmfit3c) 

lmfit4a <- lm(casual~temp*windspeed, data=bikeDayTrainSubset)
summary(lmfit4a)

lmfit4b <- lm(casual~temp*season, data=bikeDayTrainSubset)
summary(lmfit4b)

lmfit5a <- lm(casual~temp*windspeed*season, data=bikeDayTrainSubset)
summary(lmfit5a)

lmfit5b <- lm(casual~temp*windspeed*season + I(temp^2)+ I(windspeed^2)+ I(season^2), data=bikeDayTrainSubset)
summary(lmfit5b)

trainFit <- function (fit) {
  model <- round(c(summary(fit)$adj.r.squared, AIC(fit), 
                   MuMIn::AICc(fit), BIC(fit)), 3)
  name <- c("adj.r.sqr", "AIC","AICc", "BIC")
  df_stat <- data.frame(name, model)
  df_stat2 <- spread(df_stat, name, model)
}
stat1a <- trainFit(lmfit1a)
stat1b <- trainFit(lmfit1b)
stat1c <- trainFit(lmfit1c)
stat1d <- trainFit(lmfit1c)

stat2a <- trainFit(lmfit2a)
stat2b <- trainFit(lmfit2b)
stat2c <- trainFit(lmfit2c)

stat3a <- trainFit(lmfit3a)
stat3b <- trainFit(lmfit3b)
stat3c <- trainFit(lmfit3c)

stat4a <- trainFit(lmfit4a)        
stat4b <- trainFit(lmfit4b) 

stat5a<- trainFit(lmfit5a)
stat5b<- trainFit(lmfit5a)

stat_sum <- rbind(stat1a,stat1b,stat1c,stat1d,stat2a,stat2b,stat2c,stat3a,stat3b,stat3c,stat4a,stat4b,stat5a,stat5b)
rownames (stat_sum) <-c("lmfit1a","lmfit1b","lmfit1c","lmfit1d","lmfit2a","lmfit2b","lmfit2c", "lmfit3a","lmfit3b","lmfit3c","lmfit4a","lmfit4b","lmfit5a","lmfit5b") 
stat_sum 