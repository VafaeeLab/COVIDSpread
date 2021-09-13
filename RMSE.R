RMSE <- function(data.model,data1,l,r,tn ){
rmse1<- 0
datae <-ts(data.model, start=c(1,1))

data <- data1$data
reggt.validation <- data1$reggt.validation
sample.model1 <- data1$model
sig1 <- data1$sig
ar <- data1$ar
i <- data1$i
ma <- data1$ma

diff1 <- 0

for (o in 1:l)
{
  
  observedy <- data[length(data)]
  observedy <- data.model[length(data.model)]
  
  data <- data[-length(data)]
  data.model <- data.model[-length(data.model)]
  reggt.validation <- reggt.validation[-length(reggt.validation)]
  
  
  if (r==1)
  {
    regsig1 <- sig1[length(sig1)]
    p <- forecast(Arima(y = data, xreg =reggt.validation,  order = c(ar,diff1+i,ma),include.constant = FALSE,method = "ML"),h=1,xreg=reggt.validation) 
    if (tn==1)
    {
    meanp <-p$mean[1]
    }
    if (tn==2)
    {
      le <-  datae[length(datae)-o]
      meanp <- (p$mean[1]*le)
    }
    if (tn==3)
    {
      meanp <- (p$mean[1])^(length(data)+1)
    }
    if (tn==4)
    {
      meanp <- exp(p$mean[1])
    }
  }
  
  if (r==0)
  {
    
    p <- ( forecast(Arima(y = data, order = c(ar,diff1+i,ma),method = "CSS")))
    if (tn==1)
    {
      meanp <-p$mean[1]
    }
    if (tn==2)
    {
      le <-  datae[length(datae)-o]
      meanp <- (p$mean[1]*le)
    }
    if (tn==3)
    {
      meanp <- (p$mean[1])^(length(data)+1)
    }
    if (tn==4)
    {
      meanp <- exp(p$mean[1])
    }
  }
  rmse1 <- (((observedy)- meanp)^2)+rmse1
}
rmse1 <- ((rmse1)/l)^0.5
return(rmse1)
}