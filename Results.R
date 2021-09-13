source("getJHU.R")
source("generateModel.R")
source("update_data_models.R")
source("printOutput.R")

doublingCurve <- function(data, doublingTime, last_n_days = length(data), rmse.percent = 0.2){
  # doublingtime in terms of days
  # full time series
  n = length(data)
  increment = 1/doublingTime
  y = sapply(seq(0,last_n_days/doublingTime-increment,increment),FUN = function(x) 2^x)
  residuals = y - data[(n - last_n_days+1):n]
  residuals.t = residuals[(n - rmse.percent*n + 1):n]
  rmse = sqrt(mean(residuals.t^2))
  output = list()
  output$curve = y
  output$RMSE = rmse
  output$residuals = residuals
  
  return(output)
}
linearReg <- function(data, last_n_days = length(data), rmse.percent = 0.2){
  n = length(data)
  data <- data[(n - last_n_days+1):n]
  df <- data.frame(log(data))
  df$day <- c(1:length(data))
  colnames(df) <- c("data", "day")
  fit <- lm(data ~ day, df) 
  residuals = exp(fit$fitted.values) - data
  residuals.t = residuals[(n - rmse.percent*n + 1):n]
  
  rmse = sqrt(mean(residuals.t^2))
  output = list()
  output$fit = fit
  output$RMSE = rmse
  output$residuals = residuals
  return(output)
}

# update_data_model()
ecdc <- read.csv("Data/jhu.csv", row.names = 1)
allModels <- readRDS("Data/allModels.rds")

res <- c()
countries <- unique(as.character(colnames(ecdc)))
for (i in 1:length(countries)) { 
  c <- countries[i]
  output <- allModels[[i]]
  
  coef <- output$model$coef
  se <- sqrt(diag(output$model$var.coef))
  model.coef = paste0( sapply(names(coef),FUN = function(x) paste(trimws(toupper(x)),": ", round(coef[x],3), " (", round(se[x],2),")" ,sep = "")), collapse = ", ")
  
  t <- checkresiduals(output$model, plot = F)
  
  
  out2 = doublingCurve(output$data, doublingTime = 2)
  out3 = doublingCurve(output$data, doublingTime = 3)
  out7 = doublingCurve(output$data, doublingTime = 7)
  regg = linearReg(output$data)
  n = length(output$data)
  st <- 1 #n - 0.2*n + 1
  p2 = wilcox.test(abs(output$model$residuals[st:n]), abs(out2$residuals[st:n]), alternative = "two.sided")$p.value
  p3 = wilcox.test(abs(output$model$residuals[st:n]), abs(out3$residuals[st:n]), alternative = "two.sided")$p.value
  p7 = wilcox.test(abs(output$model$residuals[st:n]), abs(out7$residuals[st:n]), alternative = "two.sided")$p.value
  pr = wilcox.test(abs(output$model$residuals[st:n]), abs(regg$residuals[st:n]), alternative = "two.sided")$p.value
  
  row <- c(c, output$name, paste(output$model), model.coef, round(t$p.value,3), round(output$RMSE,2),mean (output$data[round(length(output$data)*0.8):length(output$data)]),
           round(regg$RMSE,2), round(out2$RMSE,2),  round(out3$RMSE,2),  round(out7$RMSE,2), p2, p3, p7, pr)
  
  
  res <- rbind(res,row)
}

colnames(res) <- c("Country", "Transformation", "Model", "Parameters (SE)", "P.Value", "ARIMA.RMSE", "mean observed 92 last",
                   "LM.RMSE", "Doubled_2days.RMSE", "Doubled_3days.RMSE", "Doubled_7days.RMSE",
                   "LM.pvalue", "Doubled_2days.pvalue", "Doubled_3days.pvalue", "Doubled_7days.pvalue")


# Effect of Transformation ------------------------------------------------

countries = c("Australia","Austria","Canada","China","France","Germany","Greece","Iceland","Iran","Iraq","Italy","Japan","Norway","Singapore","South_Korea","Spain","Sweden","Switzerland","United_Kingdom","United_States_of_America")
par(mfrow=c(5,4))
pvals <- c()

countries = c("Germany")
par(mfrow=c(1,1))
pvals <- c()
for(c in countries){

  output <- allModels[[which(colnames(ecdc)==c)]]
  a <- output$allTrans
    ts.plot(ts(a[,1], start=c(1,1)), 
            ts(a[,2], start=c(1,1)),
            # ts(a[,3], start=c(1,1)), 
            ts(a[,4], start=c(1,1)),
            ts(a[,5], start=c(1,1)),
            gpars=list(lty = c(2,2,2,1), col=c(2, 3, 4, 1), main = c))
 }

#par(mfrow=c(5,4))
#pvals <- c()
countries = c("Burundi")
par(mfrow=c(1,1))
pvals <- c()
for(c in countries){
  output <- allModels[[which(colnames(ecdc)==c)]]
  a <- output$allTrans
  r = c(sqrt(mean((a[,1] - a[,5])^2)), sqrt(mean((a[,2] - a[,5])^2)), sqrt(mean((a[,4] - a[,5])^2)))
  barplot(r, col = c(2,3,4), width = 1, space = 1, main = c)
  p1 = t.test((a[,1] - a[,5])^2, (a[,2] - a[,5])^2, alternative = "two.sided")$p.value
  p2 = t.test((a[,1] - a[,5])^2, (a[,4] - a[,5])^2, alternative = "two.sided")$p.value
  p3 = t.test((a[,2] - a[,5])^2, (a[,4] - a[,5])^2, alternative = "two.sided")$p.value
  pvals <- rbind(pvals, c(p1,p2, p3))
}
#r
rownames(pvals) <- countries
colnames(pvals) <- c("without vs Ratio", "Without vs Log", "Ratio vs Log")


# Dynamic Model Estimation ------------------------------------------------
library(viridis)
country = "Iran"
dates = c("2020_04_03", "2020_04_04", "2020_04_05", "2020_04_06", "2020_04_07", 
          "2020_04_08", "2020_04_09", "2020_04_10", "2020_04_11")
tmp = ecdc
o12 <-  generateModel(country = country, 
                      ecdc = tmp, r = 0, 
                      interventions.past =  c(), 
                      interventions.future = c(), day.range = c("2020_03_05", "2020_04_21"))
errors = c()
pred <- list()
for(i in 1:length(dates)){
  o <-  generateModel(country = country, 
                      ecdc = tmp, r = 0, 
                      interventions.past =  c(), 
                      interventions.future = c(), day.range = c("2020_03_05", dates[i]))
  pred[[i]] <- o$meanp
  rmse <- sqrt(mean((o$meanp[10-i+1] - o12$data[40])^2)) # error for April 13th (today)
  errors <- c(errors, rmse)
}

color<- viridis(9)
t <- ts(o12$data[30:49], start = 30, end = 39)
ts.plot(t , pred[[1]], pred[[2]], pred[[3]], pred[[4]], 
        pred[[5]], pred[[6]], pred[[7]], pred[[8]], pred[[9]], col = c(1, color), 
        main = country)
# }
barplot(errors, col = color)
write.csv(pred,choose.files())

library(ggplot2)
library(ggfortify)
# plot(c(1:50), as.numeric(output$data[(length(output$data) - 50+1):length(output$data)]), type = "l")
# lines(c(1:50),doublingCurve(output$data, doublingTime = 2)$curve, col = "green")
# lines(c(1:50), doublingCurve(output$data, doublingTime = 3)$curve, col = "blue")
# lines(c(1:50), doublingCurve(output$data, doublingTime = 7)$curve, col = "red")
