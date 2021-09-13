modelselectionPower <- function(rmse3,data.model, sample.model3,reggt,reggp,sig3,r,kstepforecast ){
  name <- "Power Transformation"
  RMSE <- rmse3
  data <- ts(data.model, start=c(1,1))
  model<- sample.model3
  
  if(r>0){
    regpvalue <- sig3[length(sig3)]} else {regpvalue <- 1}
  
  if (regpvalue > 0.05)
  {
    r <-0
  }
  
  
  
  
  output <- transformation (data.model,r,reggt)
  sample.model3 <- output$model3 
  
  tk_index(data)
  data <- data^(1/tk_index(data))
  data <- ts(data, start=c(1,1))
  ar <-sample.model3$arma[1]
  ma <-sample.model3$arma[2]
  i <-sample.model3$arma[6]
  
  meanpr <- c()
  meanur <- c()
  meanlr <- c()
  
  if (r==1)
  {
    
    p <- (forecast(Arima(y = data, xreg =reggt,  order = c(ar,i,ma)),xreg=reggp[1:kstepforecast],h=kstepforecast) )
    meanpr <- p$mean
    meanur <- p$upper
    meanlr <- p$lower
    
    for (ii in (length(data)+1):(length(data)+kstepforecast))
    {
      t <- Arima(y = data, xreg =reggt,  order = c(ar,i,ma),include.constant = FALSE,method = "ML")  
      t$coef <- sample.model3$coef
      p <- (forecast(t,data,xreg =reggp[ii-length(data.model)]))
      meanpr[ii-length(data.model)] <- (p$mean[ii-length(data)]^ii)
      meanur[ii-length(data.model)] <- (p$upper[ii-length(data)]^ii)
      meanlr[ii-length(data.model)] <- (p$lower[ii-length(data)]^ii)
      
      if(ii-length(data) >1) {
        meanpr[ii-length(data.model)] <- max(p$mean[ii-length(data)]^ii, meanpr[ii-length(data)]-1)
        meanur[ii-length(data.model)] <- max(p$upper[ii-length(data)]^ii,meanur[ii-length(data)]-1)
        meanlr[ii-length(data.model)] <- max(p$lower[ii-length(data)]^ii,meanlr[ii-length(data)]-1)}
      
      y <-length(data)+1
      length(data) <-y
      data [y] <- meanpr[(ii+1)-length(data)] ^ (1/ii)
      
      
      y <-length(reggt)+1
      length(reggt) <-y
      reggt [y] <- reggp[ii-length(data.model)]
      
    }
    
    
    meanpr <- ts(meanpr, start=c(1,length(data.model)+1))
    meanur <- ts(meanur, start=c(1,length(data.model)+1))
    meanlr <- ts(meanlr, start=c(1,length(data.model)+1))
  }
  
  
  
  data <- ts(data.model, start=c(1,1))
  tk_index(data)
  data <- data^(1/tk_index(data))
  data <- ts(data, start=c(1,1))
  reggt <- regg$reggt
  p <- ( forecast(Arima(y = data, order = c(ar,diff3+i,ma)),h=kstepforecast) )
  meanp <- p$mean
  meanu <- p$upper
  meanl <- p$lower
  for (i in (length(data)+1):(length(data)+kstepforecast))
  {
    meanp[i-length(data)] <- (p$mean[i-length(data)]^i)
    meanu[i-length(data),] <- (p$upper[i-length(data),]^i)
    meanl[i-length(data),] <- (p$lower[i-length(data),]^i)
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