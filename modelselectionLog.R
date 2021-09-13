modelselectionLog <- function(rmse4,data.model, sample.model4,reggt,reggp,sig4,r,kstepforecast ){
  name <- "logaritmic Transformation"
  RMSE <- rmse4
  data <- ts(data.model, start=c(1,1))
  model <- sample.model4
  
  if(r>0){
    regpvalue <- sig4[length(sig4)]} else {regpvalue <- 1}
  
  if (regpvalue > 0.05)
  {
    r <-0
  }
  
  
  
  output <- transformation (data.model,r,reggt)
  sample.model4 <- output$model4 
  data <- log(data)
  ar <-sample.model4$arma[1]
  ma <-sample.model4$arma[2]
  i <-sample.model4$arma[6]
  
  
  if (r==1)
  {
    p <- ( forecast(Arima(y = data, order = c(ar,i,ma),method="ML"),h=kstepforecast) )
    meanpr <-p$mean
    meanur <-p$upper
    meanlr <-p$lower
    
    for (ii in 1:kstepforecast)
    {
      t <- Arima(y = data, xreg =reggt,  order = c(ar,i,ma),include.constant = FALSE,method = "ML")  
      t$coef <- sample.model4$coef
      p <- (forecast(t,data,xreg =reggp[ii]))
      meanpr[ii] <- exp(p$mean[1])
      meanur[ii] <- exp(p$upper[1])
      meanlr[ii] <- exp(p$lower[1])
      
      y <-length(data)+1
      length(data) <-y
      data [y] <- p$mean[1]
      
      
      y <-length(reggt)+1
      length(reggt) <-y
      if(ii<=i){
        reggt [y] <- 0}
      if(ii>i){
        reggt [y] <- reggp[ii-1]}
      
    }
    
    for (q in 1:kstepforecast){
      for (w in 1:2){
        if(q==1){
          meanpr[q] <- max(meanpr[q], data.model[length(data.model)])
          meanur[q,w] <- max(meanur[q,w], data.model[length(data.model)])
          meanlr[q,w] <- max(meanlr[q,w], data.model[length(data.model)])}
        if(q>1){
          meanpr[q] <- max(meanpr[q], meanpr[q-1],data.model[length(data.model)])
          meanur[q,w] <- max(meanur[q,w], meanur[q-1],data.model[length(data.model)])
          meanlr[q,w] <- max(meanlr[q,w], meanlr[q-1],data.model[length(data.model)])}}}
    
    meanpr <- ts(meanpr, start=c(1,length(data.model)+1))
    meanur <- ts(meanur, start=c(1,length(data.model)+1))
    meanlr <- ts(meanlr, start=c(1,length(data.model)+1))
  }
  
  data <- ts(data.model, start=c(1,1))
  data <- log(data)
  
  p <- ( forecast(Arima(y = data, order = c(ar,i,ma)) ,h=kstepforecast))
  meanp <- exp(p$mean[])
  meanu <- exp(p$upper[])
  meanl <- exp(p$lower[])
  
  for (q in 1:kstepforecast){
    for (w in 1:2){
      if(q==1){
        meanp[q] <- max(meanp[q], data.model[length(data.model)])
        meanu[q,w] <- max(meanu[q,w], data.model[length(data.model)])
        meanl[q,w] <- max(meanl[q,w], data.model[length(data.model)])}
      if(q>1){
        meanp[q] <- max(meanp[q], meanp[q-1],data.model[length(data.model)])
        meanu[q,w] <- max(meanu[q,w], meanu[q-1,w],data.model[length(data.model)])
        meanl[q,w] <- max(meanl[q,w], meanl[q-1,w],data.model[length(data.model)])}}}
  
  meanp <- ts(meanp, start=c(1,length(data.model)+1))
  meanu <- ts(meanu, start=c(1,length(data.model)+1))
  meanl <- ts(meanl, start=c(1,length(data.model)+1))
  
  
  result <- list()
  
  
  result$name <- name
  result$model <- model
  result$RMSE <- RMSE
  result$regpvalue <- regpvalue
  if(r==1){
    result$meanp <- meanpr
    result$meanu <- meanur
    result$meanl <- meanlr
    
  }
  if(r==0){
    result$meanp <- meanp
    result$meanu <- meanu
    result$meanl <- meanl
  }
  
  return(result)
}