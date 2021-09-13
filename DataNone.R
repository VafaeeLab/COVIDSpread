DataNone <- function(data.model, sample.model1,reggt,sig1,l,r ){
# Calculating RMSE for without transformation
data <- ts(data.model, start=c(1,1))
datae <- ts(data.model, start=c(1,1))
reggt.validation <- reggt
model<- sample.model1
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
transformatonoutput$sig <- sig1
return(transformatonoutput)
}




