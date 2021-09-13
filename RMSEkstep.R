RMSEkstep <- function(data.model,data, sample.model1,reggt.validation,sig1,kstep,r,tn ){

  
  data <-data
  datae <- data.model
  kobserved <-  length(data) - kstep
  observedy <- data[(kobserved+1):length(data)]
  observedy <- datae[(kobserved+1):length(datae)]
  data <- data[1:kobserved]
  reggt.validation1 <- reggt.validation[(kobserved+1):length(reggt.validation)]
  reggt.validation2 <- reggt.validation[1:kobserved]
  
  model<- sample.model1
  ar <-model$arma[1]
  ma <-model$arma[2]
  i <-model$arma[6]
  diff1 <- 0
  rmse1 <- 0
  pp <- 0

  if (r==1)
  {
    regsig1 <- sig1[length(sig1)]
    p <- forecast(Arima(y = data, xreg =reggt.validation2,  order = c(ar,diff1+i,ma),include.constant = FALSE,method = "ML"),h=kstep,xreg=reggt.validation1) 
    
    
  }
  
  if (r==0)
  {
    
    p <- ( forecast(Arima(y = data, order = c(ar,diff1+i,ma),method = "CSS"),h=kstep))
    
  }
  
  if (tn==1)
  {
    rmse1 <- rmse (p$mean,observedy)
  }
  if (tn==2)
  {
    observedy <- datae[(kobserved+2):length(datae)]
    pp [1] <- datae[kobserved]
    for (o in 2:(kstep+1))
    {
      pp[o] <- pp[o-1] * p$mean[o-1] 
    }
    pp <- pp[-1]
    
    rmse1 <- rmse (pp,observedy)
  }
  if (tn==3)
  {
    for (o in (kobserved+1):length(datae))
    {
      t <- o- kobserved
      pp[t] <- (p$mean[t])^(o)
    }
    rmse1 <- rmse (pp,observedy)
  }
  if (tn==4)
  {
    meanp <- exp(p$mean)
    rmse1 <- rmse (meanp,observedy)
  }
  return(rmse1)
}