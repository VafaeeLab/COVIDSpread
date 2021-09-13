modelselectionRatio <- function(rmse2,data.model, sample.model2,reggt,reggp,sig2,r,kstepforecast ){
  name <- "Ratio Transformation"
  RMSE <- rmse2
  data <- ts(data.model, start=c(1,1))
  model<- sample.model2
  
  if(r>0){
    regpvalue <- sig2[length(sig2)]} else {regpvalue <- 1}
  
  if (regpvalue > 0.05)
  {
    r <-0
  }
  
  
  
  output <- transformation (data.model,r,reggt)
  sample.model2 <- output$model2 
  data <- data[-1]/data[-length(data)]
  ar <-sample.model2$arma[1]
  ma <-sample.model2$arma[2]
  i <-sample.model2$arma[6]
  
  p <- ( forecast(Arima(y = data, order = c(ar,i,ma)),h=kstepforecast) )
  meanpr <- p$mean
  meanur <- p$upper
  meanlr <- p$lower
  
  
  if (r==1)
  {
    regsig2 <- sig2[length(sig2)]
    p <- (forecast(Arima(y = data, xreg =reggt[-1],  order = c(ar,i,ma)),xreg=reggp[1:kstepforecast],h=kstepforecast) )
    data <- ts(data.model, start=c(1,1))
    l <-  data[length(data)]
    
    meanpr <- p$mean
    meanur <- p$upper
    meanlr <- p$lower
    for (i in (length(data)+1):(length(data)+kstepforecast))
    {
      meanpr[i-length(data)] <- (p$mean[i-length(data)]*l)
      meanur[i-length(data),] <- (p$upper[i-length(data),]*l)
      meanlr[i-length(data),] <- (p$lower[i-length(data),]*l)
      
      # if(i-length(data) >1) {
      #  meanpr[i-length(data)] <- max((p$mean[i-length(data)]*l), meanpr[i-length(data)-1])
      #  meanur[i-length(data)] <- max((p$upper[i-length(data)]*l), meanur[i-length(data)-1])
      #  meanlr[i-length(data)] <- max((p$lower[i-length(data)]*l), meanlr[i-length(data)-1])  }                          
      
      l<- meanpr[i-length(data)]}
    meanpr <- ts(meanpr, start=c(1,length(data.model)+1))
    meanur <- ts(meanur, start=c(1,length(data.model)+1))
    meanlr <- ts(meanlr, start=c(1,length(data.model)+1))
    
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
  
  
 
  
  data <- ts(data.model, start=c(1,1))
  data <- data[-1]/data[-length(data)]
  reggt <- regg$reggt
  ar <-sample.model2$arma[1]
  ma <-sample.model2$arma[2]
  i <-sample.model2$arma[6]
  p <- ( forecast(Arima(y = data, order = c(ar,i,ma)),h=kstepforecast) )
  data <- ts(data.model, start=c(1,1))
  l <-  data[length(data)]
  meanp <- p$mean
  meanu <- p$upper
  meanl <- p$lower
  for (i in (length(data)+1):(length(data)+kstepforecast))
  { 
    
    meanp[i-length(data)] <- (p$mean[i-length(data)]*l)
    meanu[i-length(data),] <- (p$upper[i-length(data),]*l)
    meanl[i-length(data),] <- (p$lower[i-length(data),]*l)
    l<- meanp[i-length(data)]
    
  }
  
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