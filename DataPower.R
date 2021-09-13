DataPower <- function(data.model, sample.model3,reggt,sig3,l,r ){
  # Calculating RMSE for Power Transformation
  data <- ts(data.model, start=c(1,1))
  datae <-ts(data.model, start=c(1,1))
  reggt.validation <- reggt
  model<- sample.model3
  tk_index(data)
  data <- data^(1/tk_index(data))
  data <- ts(data, start=c(1,1))
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
  transformatonoutput$sig <- sig3
  return(transformatonoutput)
}