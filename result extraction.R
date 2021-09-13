countries = c("Australia","Austria","Canada","China","France","Germany","Greece","Iceland","Iran","Iraq","Italy","Japan","Norway","Singapore","Spain","Sweden","Switzerland","Germany","Jordan","United_Kingdom","United_States_of_America")

tt <-  matrix(nrow=20, ncol=20)
ccc <- 0
j <- c(1,5,10,15,20)
for(c in countries){
  ccc<- ccc+1
  for(cc in j){
  
  r <- generateModel(country = c, ms = 1, ecdc = read.csv("Data/jhu.csv", row.names = 1, fileEncoding = "UTF-8"), r = 0, interventions.past = c(), interventions.future = c(), day.range = c("2020-09-15","2021-01-16"),kstepRMSE = cc)
  tt[ccc,cc] =r$RMSE
  
  }  
}
ttt<-cbind(countries,tt)

write.csv(ttt,choose.files())  
