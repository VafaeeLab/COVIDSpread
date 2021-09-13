DataRatio <- function(data.model, sample.model2,reggt,sig2,l,r ){
  # Calculating RMSE for Ratio Transformation  
  data <- ts(data.model, start=c(1,1))
  datae <-ts(data.model, start=c(1,1))
  reggt.validation <- reggt
  model<- sample.model2
  data <- data[-1]/data[-length(data)]
  ar <-model$arma[1]
  ma <-model$arma[2]
  i <-model$arma[6]
  
  
  
  transformatonoutput <- list()
  
  
  transformatonoutput$data <- data
  transformatonoutput$reggt.validation <- reggt.validation
  transformatonoutput$model <- model
  transformatonoutput$ar <- ar
  transformatonoutput$i <- i
  transformatonoutput$ma <- ma
  transformatonoutput$sig <- sig2
  return(transformatonoutput)
}