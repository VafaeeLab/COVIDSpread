generateModel<- function(country, ecdc, r = 0,ms = 1, interventions.past = c(), interventions.future  =  c(), day.range = NULL, kstepRMSE = 10, kstepforecast = 10){
  
library(forecast)
library(tseries)
library(readtext)
library(openxlsx)
library(spatstat)
library(tsiR)
library(timetk)
library(imputeTS)
library(Metrics)
source("setRegressors.R")
source("modelselection.R")
source("transformation.R")
source("DataLog.R")  
source("DataNone.R")
source("DataPower.R")
source("DataRatio.R")
source("RMSE.R")
source("RMSEkstep.R")
source("modelselectionNone.R")
source("modelselectionRatio.R")
source("modelselectionPower.R")
source("modelselectionLog.R")
source("modelselectionBox.R")
  
# subset ECDC ----------------------------------------------------------------------------------------------------

# remove starting NAs

data.country <- ecdc[,country]
names(data.country)<-rownames(ecdc)
keep <- which(data.country>50) #Note: all NAs are removed even in recent days (e.g., SPAIN)
data.country <- rev(data.country[keep])
dates <- rev(rownames(ecdc)[keep])

# apply range 
if(!is.null(day.range)){
  day.range = as.character(day.range)
  start <- which(dates == day.range[1]%>% gsub("-","_",.))
  start <- if(length(start) == 0) 1 else start
  end   <- which(dates == day.range[2]%>% gsub("-","_",.))
  end <- if(length(end) == 0) length(data.country) else end
  dates <- dates[start:end]
  data.country <- data.country[start:end]
}
# interventions.past <- c(0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
# interventions.future <- c(0,1,0,0,0,0,0,1,0,0)
# r<-0
regg <- setRegressors(dates, interventions.past, interventions.future)
reggt <- regg$reggt
reggp <- regg$reggp
data.model <- na_locf(data.country, option = "nocb")
#data.model <- na_kalman(data.country, smooth = F)
  

# Transformation --------------------------------------------------------
#applying data transformation and finding the best fitting model for each transformation
output <- transformation (data.model,r,reggt)
sample.model1 <- output$model1
sample.model2 <- output$model2 
sample.model3 <- output$model3 
sample.model4 <- output$model4 
 
sig1  <- output$sig1 
sig2  <-output$sig2 
sig3  <-output$sig3 
sig4   <-output$sig4 


# model comparison --------------------------------------------------------

#Indicating last 20 percent of the data
l <- length(data.model)*0.2
# Rounding the number to calculate last 20 percent of the data
l <- round(l,digits = 0)

#data preparation for various transformations for RMSE calculation
data1 <- DataNone (data.model,sample.model1,reggt,sig1,l,r)
data2 <- DataRatio (data.model,sample.model2,reggt,sig2,l,r)
data3 <- DataPower (data.model,sample.model3,reggt,sig3,l,r)
data4 <- DataLog (data.model,sample.model4,reggt,sig4,l,r)


# Calculating RMSE for last 20 percent of the data step by step
# Calculating RMSE for without transformation
rmse1 <- RMSE(data.model,data1,l,r,1)
# Calculating RMSE for Ratio Transformation  
rmse2 <- RMSE(data.model,data2,l,r,2)
# Calculating RMSE for Power Transformation
rmse3 <- RMSE(data.model,data3,l,r,3)
# Calculating RMSE for logaritmic Transformation
rmse4 <- RMSE(data.model,data4,l,r,4)

transRMSE <- c(rmse1, rmse2, rmse3, rmse4)
names(transRMSE) <- c("None", "Ratio", "Power", "Log")

# Calculating RMSE for k steps together
# kstepRMSE <- 5 # added as input parameter
# Calculating RMSE for without transformation
rmse1 <- RMSEkstep(data.model,data1$data,data1$model,data1$reggt.validation,data1$sig,kstepRMSE,r,1)
# Calculating RMSE for Ratio Transformation
rmse2 <- RMSEkstep(data.model,data2$data,data2$model,data2$reggt.validation,data2$sig,kstepRMSE,r,2)
# Calculating RMSE for Power Transformation
rmse3 <- RMSEkstep(data.model,data3$data,data3$model,data3$reggt.validation,data3$sig,kstepRMSE,r,3)
# Calculating RMSE for logaritmic Transformation
rmse4 <- RMSEkstep(data.model,data4$data,data4$model,data4$reggt.validation,data4$sig,kstepRMSE,r,4)


# Model selection ---------------------------------------------------------
# ms is given by user to prioritize special type of transformation
# model selection function will prioritize transformation based on ms
# ms = 1 does not provide any priority
output <- modelselection (ms,rmse1,rmse2,rmse3,rmse4)
rmse1 <- output$rmse1
rmse2 <- output$rmse2
rmse3 <- output$rmse3
rmse4 <- output$rmse4




#model selection (could be based on AIC,BIC,AICc,RMSE)
# based on AIC   
#u <- min(sample.model1$aic,sample.model2$aic,sample.model3$aic,sample.model4$aic)
# based on RMSE
# rmse1 and rmse2 are prioritized by researcher with the factor of 0.9
u <- min(c(0.9*rmse1,0.9*rmse2,rmse3,rmse4))
# kstepforecast is the number of future values that user wants to predict 
# It should be noted that number of future predictions should match with regressor lenght
#kstepforecast = 7
if (u==0.9*rmse1)
{
  result <- modelselectionNone(rmse1,data.model, sample.model1,reggt,reggp,sig1,r,kstepforecast)
}
if (u==0.9*rmse2)
{
  result <- modelselectionRatio(rmse2,data.model, sample.model2,reggt,reggp,sig2,r,kstepforecast)
}
if (u==rmse3)
{
  result <- modelselectionPower(rmse3,data.model, sample.model3,reggt,reggp,sig3,r,kstepforecast)
}
if (u==rmse4)
{
  result <- modelselectionLog(rmse4,data.model, sample.model4,reggt,reggp,sig4,r,kstepforecast)
}

# Box cox ------------------------------------------------------------------
# This part applies box-Cox transformation in case user rrequires this specific transformation with defining ms equal to 6
if (ms==6)
{
  result <- modelselectionBox(data.model,reggt,reggp,r,kstepforecast)
}
  
# Output ------------------------------------------------------------------
output <- list()
#output$datamodel <- data.model
output$transRMSE <- transRMSE
output$dates <- dates
output$data <- ts(data.model, start=c(1,1))
output$country <- country
output$name <- result$name
output$model <- result$model
output$RMSE <- result$RMSE
output$regpvalue <- result$regpvalue

if(r==1){
  output$meanp <- result$meanpr
  output$meanu <- result$meanur
  output$meanl <- result$meanlr
  # output$meanpr <- meanpr
  # output$meanur <- meanur
  # output$meanlr <- meanlr
}
if(r==0){
  output$meanp <- result$meanp
  output$meanu <- result$meanu
  output$meanl <- result$meanl
}





return(output)

}
