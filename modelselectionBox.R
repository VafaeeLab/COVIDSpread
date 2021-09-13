modelselectionBox <- function(data.model, reggt,reggp,r,kstepforecast ){
  
  name <- "box-Cox Transformation"
  data <- ts(data.model, start=c(1,1))
  reggt <- regg$reggt
  reggp <- regg$reggp
  if (r==1){
    sample.model5 <- auto.arima (data, xreg=reggt, allowdrift = FALSE,approximation = FALSE,allowmean = FALSE, lambda = TRUE)
    sig5 <- (1-pnorm(abs(sample.model5$coef)/sqrt(diag(sample.model1$var.coef))))*2} 
  if (r==0){
    sample.model5 <- auto.arima (data, allowdrift = FALSE,approximation = FALSE, allowmean = FALSE, lambda = TRUE)
    sig5 <- (1-pnorm(abs(sample.model5$coef)/sqrt(diag(sample.model1$var.coef))))*2}
  
  if(r>0){
    regpvalue <- sig1[length(sig5)]} else {regpvalue <- 1}
  
  if (regpvalue > 0.05)
  {
    r <-0
  }
  
  if (r==1){
    sample.model5 <- auto.arima (data, xreg=reggt, allowdrift = FALSE,approximation = FALSE,allowmean = FALSE, lambda = TRUE)
    sig5 <- (1-pnorm(abs(sample.model5$coef)/sqrt(diag(sample.model1$var.coef))))*2} 
  if (r==0){
    sample.model5 <- auto.arima (data, allowdrift = FALSE,approximation = FALSE, allowmean = FALSE, lambda = TRUE)
    sig5 <- (1-pnorm(abs(sample.model5$coef)/sqrt(diag(sample.model1$var.coef))))*2}
  model <- sample.model5
  
  
  
  data <- ts(data.model, start=c(1,1))
  ar <-sample.model5$arma[1]
  ma <-sample.model5$arma[2]
  i <-sample.model5$arma[6]
  
  
  #result <- accuracy(sample.model5)
  rmse5 <- RMSENonekstep (data.model,sample.model5,reggt,sig5,kstep,r)
  
  #rmse5 <- result[2]
  
  
  
  if (r==1)
  {
    p <-  forecast(sample.model5,xreg=reggp,h=kstepforecast )
    meanpr <- p$mean
    meanur <- p$upper
    meanlr <- p$lower
    
    
    for (ii in 1:kstepforecast)
    {
      
      t <- auto.arima (data, xreg=reggt, allowdrift = FALSE,approximation = FALSE,allowmean = FALSE, lambda = TRUE) 
      t$coef <- sample.model5$coef
      p <- (forecast(t,data,xreg =reggp[ii]))
      meanpr[ii] <- (p$mean[1])
      meanur[ii,] <- (p$upper[1,])
      meanlr[ii,] <- (p$lower[1,])
      
      y <-length(data)+1
      length(data) <-y
      data [y] <- meanpr[ii]
      
      
      y <-length(reggt)+1
      length(reggt) <-y
      if(ii<=i){
        reggt [y] <- 0}
      if(ii>i){
        reggt [y] <- reggp[ii-i]}
      
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
  
  
  if (r==0)
  {
    data <- ts(data.model, start=c(1,1))
    ar <-sample.model5$arma[1]
    ma <-sample.model5$arma[2]
    i <-sample.model5$arma[6]
    
    
    p <- ( forecast(sample.model5,h=kstepforecast ) )
    
    meanp <-p$mean
    meanu <-p$upper
    meanl <-p$lower
    
    for (ii in 1:kstepforecast)
    {
      
      t <- auto.arima (data ,allowdrift = FALSE,approximation = FALSE,allowmean = FALSE, lambda = TRUE) 
      t$coef <- sample.model5$coef
      p <- (forecast(t,data))
      meanp[ii] <- (p$mean[1])
      meanu[ii] <- (p$upper[1])
      meanl[ii] <- (p$lower[1])
      
      if(ii>1){
        meanp[ii] <- max((p$mean[1]), meanp[ii-1])
        meanu[ii] <- max((p$upper[1]), meanu[ii-1])
        meanl[ii] <- max((p$lower[1]), meanl[ii-1])}
      
      y <-length(data)+1
      length(data) <-y
      data [y] <- meanp[ii]
      
      
      y <-length(reggt)+1
      length(reggt) <-y
      reggt [y] <- reggp[ii]
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
    
  }
  
  RMSE <- rmse5
  
  
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